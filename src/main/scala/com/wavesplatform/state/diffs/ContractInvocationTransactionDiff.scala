package com.wavesplatform.state.diffs

import cats.kernel.Monoid
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, ContractResult}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.traits.domain.{DataItem, Recipient}
import com.wavesplatform.lang.{Global, Version}
import com.wavesplatform.state.{
  EitherExt2,
  AccountDataInfo,
  BinaryDataEntry,
  Blockchain,
  BooleanDataEntry,
  ByteStr,
  DataEntry,
  Diff,
  IntegerDataEntry,
  LeaseBalance,
  Portfolio,
  StringDataEntry
}
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction.smart.BlockchainContext.In
import com.wavesplatform.transaction.smart.script.v1.ScriptV2
import com.wavesplatform.transaction.smart.{ContractInvocationTransaction, WavesEnvironment}
import monix.eval.Coeval
import scodec.bits.ByteVector

object ContractInvocationTransactionDiff {
  def apply(blockchain: Blockchain, height: Int)(tx: ContractInvocationTransaction): Either[ValidationError, Diff] = {
    val sc = blockchain.accountScript(tx.contractAddress)
    sc match {
      case Some(ScriptV2(_, contract)) =>
        val functionName = tx.fc.function.asInstanceOf[FunctionHeader.User].name
        contract.cfs.find(_.u.name == functionName) match {
          case None => Left(GenericError(s"No function '$functionName' at address ${tx.contractAddress}"))
          case Some(f) =>
            val ctx = Monoid
              .combineAll(
                Seq(
                  PureContext.build(Version.V3),
                  CryptoContext.build(Global),
                  WavesContext.build(Version.V3,
                                     new WavesEnvironment(AddressScheme.current.chainId, Coeval(tx.asInstanceOf[In]), Coeval(height), blockchain),
                                     false)
                ))
              .evaluationContext

            val res =
              ContractEvaluator.apply(ctx, contract, ContractEvaluator.Invokation(functionName, tx.fc, ByteVector(tx.sender.toAddress.bytes.arr)))
            res.left
              .map(a => GenericError(a.toString))
              .map {
                case ContractResult(ds, ps) => {
                  val r: Seq[DataEntry[_]] = ds.map {
                    case DataItem.Bool(k, b) => BooleanDataEntry(k, b)
                    case DataItem.Str(k, b)  => StringDataEntry(k, b)
                    case DataItem.Lng(k, b)  => IntegerDataEntry(k, b)
                    case DataItem.Bin(k, b)  => BinaryDataEntry(k, ByteStr(b.toArray))
                  }
                  val pmts = ps.map {

                    case (Recipient.Address(addrBytes), amt) => Map(Address.fromBytes(addrBytes.toArray).explicitGet() -> amt)
                  }
                  // TODO:
                  //  - ensure all amounts are positive
                  //  - sum doesn't overflow
                  //  - whatever else tranfser/massTransfer ensures
                  //  - assetId is defined
                  import cats.implicits._
                  val paymentMap = Monoid.combineAll(pmts).mapValues(Portfolio(_, LeaseBalance.empty, Map.empty))
                  val feePart = Map(
                    tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)
                  )
                  val payablePart: Map[Address, Portfolio] = tx.payment match {
                    case None => Map.empty
                    case Some((assetOpt, amt)) =>
                      assetOpt match {
                        case Some(asset) =>
                          Map(tx.sender.toAddress -> Portfolio(0, LeaseBalance.empty, Map(asset -> -amt))).combine(
                            Map(tx.contractAddress -> Portfolio(0, LeaseBalance.empty, Map(asset -> amt)))
                          )
                        case None =>
                          Map(tx.sender.toAddress -> Portfolio(-amt, LeaseBalance.empty, Map.empty))
                            .combine(Map(tx.contractAddress -> Portfolio(amt, LeaseBalance.empty, Map.empty)))
                      }
                  }
                  val transfers = Monoid.combineAll(Seq(payablePart, feePart, paymentMap))
                  Diff(
                    height = height,
                    tx = tx,
                    portfolios = transfers,
                    accountData = Map(tx.contractAddress -> AccountDataInfo(r.map(d => d.key -> d).toMap))
                  )
                }
              }

        }

      case _ => Left(GenericError(s"No contract at address ${tx.contractAddress}"))
    }

  }
}

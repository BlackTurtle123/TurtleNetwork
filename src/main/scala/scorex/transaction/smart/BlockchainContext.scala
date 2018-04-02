package scorex.transaction.smart

import com.wavesplatform.lang.TypeChecker.TypeCheckerContext
import com.wavesplatform.lang.WavesContext
import com.wavesplatform.lang.traits.{DataType, Transaction => ContractTransaction}
import com.wavesplatform.state2._
import com.wavesplatform.state2.reader.SnapshotStateReader
import monix.eval.Coeval
import scodec.bits.ByteVector
import scorex.account.Address
import scorex.account.{AddressOrAlias, AddressScheme}
import scorex.transaction._
import scorex.transaction.assets._
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}

class BlockchainContext(override val networkByte: Byte, tx: Coeval[Transaction], h: Coeval[Int], state: SnapshotStateReader) extends WavesContext {

  import BlockchainContext._

  override def height: Int = h()

  override def transaction: ContractTransaction = convert(tx())

  override def transactionById(id: Array[Byte]): Option[ContractTransaction] =
    state
      .transactionInfo(ByteStr(id))
      .map(_._2)
      .map(convert)

  override def data(addressBytes: Array[Byte], key: String, dataType: DataType): Option[Any] = {
    val address = Address.fromBytes(addressBytes).explicitGet()
    val data    = state.accountData(address, key)
    data.map((_, dataType)).flatMap {
      case (LongDataEntry(_, value), DataType.Long)        => Some(value)
      case (BooleanDataEntry(_, value), DataType.Boolean)  => Some(value)
      case (BinaryDataEntry(_, value), DataType.ByteArray) => Some(ByteVector(value.arr))
      case _                                               => None
    }
  }

  override def resolveAddress(addressOrAlias: Array[Byte]): Either[String, Array[Byte]] = {
    (for {
      aoa     <- AddressOrAlias.fromBytes(bytes = addressOrAlias, position = 0)
      address <- state.resolveAliasEi(aoa._1)
    } yield address.bytes.arr).left.map(_.toString)
  }
}

object BlockchainContext {

  private val networkByte = AddressScheme.current.chainId

  lazy val typeCheckerContext: TypeCheckerContext =
    TypeCheckerContext.fromContext(new BlockchainContext(networkByte, Coeval(???), Coeval(???), null).build())

  def convert(tx: Transaction): ContractTransaction = new ContractTransaction {
    override def bodyBytes: Either[String, ByteVector] = tx match {
      case pt: ProvenTransaction => Right(ByteVector(pt.bodyBytes()))
      case _                     => Left("Transaction is not Proven, doesn't contain bodyBytes")
    }

    override def transactionType: Int = tx.builder.typeId

    override def senderPk: Either[String, ByteVector] = tx match {
      case pt: Authorized => Right(ByteVector(pt.sender.publicKey))
      case _              => Left("Transaction doesn't contain sender public key")
    }

    override def assetId: Either[String, Option[ByteVector]] = tx match {
      case tt: TransferTransaction           => Right(tt.assetId.map(x => ByteVector(x.arr)))
      case vtt: VersionedTransferTransaction => Right(vtt.assetId.map(x => ByteVector(x.arr)))
      case mtt: MassTransferTransaction      => Right(mtt.assetId.map(x => ByteVector(x.arr)))
      case _                                 => Left("Transaction doesn't contain asset id")
    }

    override def recipient: Either[String, ByteVector] = tx match {
      case pt: PaymentTransaction            => Right(ByteVector(pt.recipient.bytes.arr))
      case tt: TransferTransaction           => Right(ByteVector(tt.recipient.bytes.arr))
      case lt: LeaseTransaction              => Right(ByteVector(lt.recipient.bytes.arr))
      case vtt: VersionedTransferTransaction => Right(ByteVector(vtt.recipient.bytes.arr))
      case _                                 => Left("Transaction doesn't contain recipient")
    }

    override def proofs: Either[String, IndexedSeq[ByteVector]] = tx match {
      case pt: ProvenTransaction => Right(pt.proofs.proofs.map(pf => ByteVector(pf.arr)).toIndexedSeq)
      case _                     => Left("Transaction doesn't contain proofs")
    }

    override def id: ByteVector = ByteVector(tx.id().arr)

    override def fee: Long = tx.assetFee._2

    override def amount: Either[String, Long] = tx match {
      case g: GenesisTransaction           => Right(g.amount)
      case g: PaymentTransaction           => Right(g.amount)
      case g: IssueTransaction             => Right(g.quantity)
      case g: ReissueTransaction           => Right(g.quantity)
      case g: BurnTransaction              => Right(g.amount)
      case g: LeaseTransaction             => Right(g.amount)
      case g: TransferTransaction          => Right(g.amount)
      case g: VersionedTransferTransaction => Right(g.amount)
      case g: ExchangeTransaction          => Right(g.amount)
      case _: CreateAliasTransaction       => Left("Transaction doesn't contain amount")
      case _: SetScriptTransaction         => Left("Transaction doesn't contain amount")
      case _: MassTransferTransaction      => Left("Transaction doesn't contain amount")
      case _: LeaseCancelTransaction       => Left("Transaction doesn't contain amount")
      case _: DataTransaction              => Left("Transaction doesn't contain amount")
    }

    override def feeAssetId: Option[ByteVector] =
      tx.assetFee._1.map(aid => ByteVector(aid.arr))

    override def timestamp: Long = tx.timestamp
  }
}

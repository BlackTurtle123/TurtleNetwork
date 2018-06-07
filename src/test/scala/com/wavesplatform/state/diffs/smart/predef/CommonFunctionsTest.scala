package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.state._
import com.wavesplatform.state.diffs._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector
import scorex.account.Address

class CommonFunctionsTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  property("extract should transaction transfer assetId if exists") {
    forAll(transferV1Gen) {
      case (transfer) =>
        val result = runScript[ByteVector](
          """
            |match tx {
            | case ttx : TransferTransaction  =>  extract(ttx.transferAssetId)
            | case other => throw
            | }
            |""".stripMargin,
          transfer
        )
        transfer.assetId match {
          case Some(v) => result.explicitGet().toArray sameElements v.arr
          case None    => result should produce("from empty option")
        }
    }
  }

  property("isDefined should return true if transfer assetId exists") {
    forAll(transferV1Gen) {
      case (transfer) =>
        val result = runScript[Boolean](
          """
                                          |match tx {
                                          | case ttx : TransferTransaction  =>  isDefined(ttx.transferAssetId)
                                          | case other => throw
                                          | }
                                          |""".stripMargin,
          transfer
        )
        transfer.assetId.isDefined shouldEqual result.explicitGet()
    }
  }

  property("Some/None/extract/isDefined") {
    runScript[Any]("Some(3)".stripMargin) shouldBe Right(Some(3L))
    runScript[Any]("None".stripMargin) shouldBe Right(None)
    runScript[Boolean]("isDefined(Some(3))".stripMargin) shouldBe Right(true)
    runScript[Boolean]("isDefined(None)".stripMargin) shouldBe Right(false)
    runScript[Long]("extract(Some(3))".stripMargin) shouldBe Right(3L)
    runScript[Long]("extract(None)".stripMargin) should produce("Extract from empty option")
  }

  property("size") {
    val arr = Array(1: Byte, 2: Byte, 3: Byte)
    runScript[Long]("size(base58'')".stripMargin) shouldBe Right(0L)
    runScript[Long](s"size(base58'${ByteStr(arr).base58}')".stripMargin) shouldBe Right(3L)
  }

  property("getTransfer should extract MassTransfer transfers") {
    import scodec.bits.ByteVector
    forAll(massTransferGen.retryUntil(tg => tg.transfers.size > 0 && tg.transfers.map(_.address).forall(_.isInstanceOf[Address]))) {
      case (massTransfer) =>
        val resultAmount = runScript[Long](
          """
            |match tx {
            | case mttx : MassTransferTransaction  =>  mttx.transfers[0].amount
            | case other => throw
            | }
            |""".stripMargin,
          massTransfer
        )
        resultAmount shouldBe Right(massTransfer.transfers(0).amount)
        val resultAddress = runScript[ByteVector](
          """
                                                      |match tx {
                                                      | case mttx : MassTransferTransaction  =>
                                                      |       match mttx.transfers[0].recipient {
                                                      |           case address : Address => address.bytes
                                                      |           case other => throw
                                                      |       }
                                                      | case other => throw
                                                      | }
                                                      |""".stripMargin,
          massTransfer
        )
        resultAddress shouldBe Right(ByteVector(massTransfer.transfers(0).address.bytes.arr))
        val resultLen = runScript[Long](
          """
                                           |match tx {
                                           | case mttx : MassTransferTransaction  =>  size(mttx.transfers)
                                           | case other => throw
                                           | }
                                           |""".stripMargin,
          massTransfer
        )
        resultLen shouldBe Right(massTransfer.transfers.size.toLong)
    }
  }

  property("+ should check overflow") {
    runScript[Long]("2 + 3") shouldBe Right(5L)
    runScript[Long](s"1 + ${Long.MaxValue}") should produce("long overflow")
  }
}

package com.wavesplatform.it.sync.transactions

import com.wavesplatform.it.TransferSending
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.state.EitherExt2
import com.wavesplatform.utils.Base58
import org.scalatest.CancelAfterFailure
import play.api.libs.json._
import scorex.account.AddressOrAlias
import scorex.api.http.assets.SignedTransferV1Request
import scorex.transaction.transfer._

import scala.concurrent.duration._

class TransferTransactionV1Suite extends BaseTransactionSuite with TransferSending with CancelAfterFailure {

  test("asset transfer changes sender's and recipient's asset balance; issuer's.TN balance is decreased by fee") {
    val (firstBalance, firstEffBalance)   = notMiner.accountBalances(firstAddress)
    val (secondBalance, secondEffBalance) = notMiner.accountBalances(secondAddress)

    val issuedAssetId = sender.issue(firstAddress, "name", "description", someAssetAmount, 2, reissuable = false, issueFee).id

    nodes.waitForHeightAriseAndTxPresent(issuedAssetId)

    notMiner.assertBalances(firstAddress, firstBalance - issueFee, firstEffBalance - issueFee)
    notMiner.assertAssetBalance(firstAddress, issuedAssetId, someAssetAmount)

    val transferTransactionId = sender.transfer(firstAddress, secondAddress, someAssetAmount, fee, Some(issuedAssetId)).id
    nodes.waitForHeightAriseAndTxPresent(transferTransactionId)

    notMiner.assertBalances(firstAddress, firstBalance - fee - issueFee, firstEffBalance - fee - issueFee)
    notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance)
    notMiner.assertAssetBalance(firstAddress, issuedAssetId, 0)
    notMiner.assertAssetBalance(secondAddress, issuedAssetId, someAssetAmount)
  }

  test("TN transfer changes TN balances and eff.b.") {
    val (firstBalance, firstEffBalance)   = notMiner.accountBalances(firstAddress)
    val (secondBalance, secondEffBalance) = notMiner.accountBalances(secondAddress)

    val transferId = sender.transfer(firstAddress, secondAddress, transferAmount, fee).id

    nodes.waitForHeightAriseAndTxPresent(transferId)

    notMiner.assertBalances(firstAddress, firstBalance - transferAmount - fee, firstEffBalance - transferAmount - fee)
    notMiner.assertBalances(secondAddress, secondBalance + transferAmount, secondEffBalance + transferAmount)
  }

  test("invalid signed TN transfer should not be in UTX or blockchain") {
    def invalidTx(timestamp: Long = System.currentTimeMillis, fee: Long = 100000) =
      TransferTransactionV1
        .selfSigned(None, sender.privateKey, AddressOrAlias.fromString(sender.address).explicitGet(), 1, timestamp, None, fee, Array.emptyByteArray)
        .right
        .get

    def request(tx: TransferTransactionV1): SignedTransferV1Request =
      SignedTransferV1Request(
        Base58.encode(tx.sender.publicKey),
        tx.assetId.map(_.base58),
        tx.recipient.stringRepr,
        tx.amount,
        tx.fee,
        tx.feeAssetId.map(_.base58),
        tx.timestamp,
        tx.attachment.headOption.map(_ => Base58.encode(tx.attachment)),
        tx.signature.base58
      )

    implicit val w =
      Json.writes[SignedTransferV1Request].transform((jsobj: JsObject) => jsobj + ("type" -> JsNumber(TransferTransactionV1.typeId.toInt)))

    val (balance1, eff1) = notMiner.accountBalances(firstAddress)

    val invalidTxs = Seq(
      (invalidTx(timestamp = System.currentTimeMillis + 1.day.toMillis), "Transaction .* is from far future"),
      (invalidTx(fee = 99999), "Fee .* does not exceed minimal value")
    )

    for ((tx, diag) <- invalidTxs) {
      assertBadRequestAndResponse(sender.broadcastRequest(request(tx)), diag)
      nodes.foreach(_.ensureTxDoesntExist(tx.id().base58))
    }

    nodes.waitForHeightArise()
    notMiner.assertBalances(firstAddress, balance1, eff1)

  }

  test("can not make transfer without having enough effective balance") {
    val (secondBalance, secondEffBalance) = notMiner.accountBalances(secondAddress)

    assertBadRequest(sender.transfer(secondAddress, firstAddress, secondEffBalance, fee))
    nodes.waitForHeightArise()

    notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance)
  }

  test("can not make transfer without having enough balance") {
    val (secondBalance, secondEffBalance) = notMiner.accountBalances(secondAddress)

    assertBadRequestAndResponse(sender.transfer(secondAddress, firstAddress, secondBalance + 1.TN, fee), "Attempt to transfer unavailable funds")
    notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance)
  }

  test("can forge block with sending majority of some asset to self and to other account") {
    val (firstBalance, firstEffBalance)   = notMiner.accountBalances(firstAddress)
    val (secondBalance, secondEffBalance) = notMiner.accountBalances(secondAddress)

    val assetId = sender.issue(firstAddress, "second asset", "description", someAssetAmount, 0, reissuable = false, fee = issueFee).id

    nodes.waitForHeightAriseAndTxPresent(assetId)

    notMiner.assertBalances(firstAddress, firstBalance - issueFee, firstEffBalance - issueFee)
    notMiner.assertAssetBalance(firstAddress, assetId, someAssetAmount)

    val tx1 = sender.transfer(firstAddress, firstAddress, someAssetAmount, fee, Some(assetId)).id
    nodes.waitForHeightAriseAndTxPresent(tx1)

    val tx2 = sender.transfer(firstAddress, secondAddress, someAssetAmount / 2, fee, Some(assetId)).id
    nodes.waitForHeightAriseAndTxPresent(tx2)

    notMiner.assertBalances(firstAddress, firstBalance - issueFee - 2 * fee, firstEffBalance - issueFee - 2 * fee)
    notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance)
  }
}

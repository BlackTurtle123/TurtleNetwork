package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{minFee, setScriptFee, transferAmount}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer._
import org.scalatest.CancelAfterFailure

class SetScriptTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {
  private val fourthAddress: String = sender.createAddress()

  private def randomPk = PrivateKeyAccount(Array.fill[Byte](32)(Random.nextInt(Byte.MaxValue).toByte))

<<<<<<< HEAD:it/src/test/scala/com/wavesplatform/it/sync/smartcontract/SetScriptTransactionSuite.scala
  private val acc0 = randomPk
  private val acc1 = randomPk
  private val acc2 = randomPk
  private val acc3 = randomPk

  private val transferAmount: Long = 1.TN
  private val fee: Long            = 0.001.TN

  test("setup acc0 with 1 TN") {
    val tx =
      TransferTransactionV2
        .selfSigned(
          version = 2,
          assetId = None,
          sender = sender.privateKey,
          recipient = acc0,
          amount = 3 * transferAmount + 3 * (0.00001.TN + 0.00002.TN), // Script fee
          timestamp = System.currentTimeMillis(),
          feeAssetId = None,
          feeAmount = minFee,
          attachment = Array.emptyByteArray
        )
        .explicitGet()

    val transferId = sender
      .signedBroadcast(tx.json())
      .id
    nodes.waitForHeightAriseAndTxPresent(transferId)
=======
  test("setup acc0 with 1 waves") {
    sender.transfer(
      sender.address,
      acc0.stringRepr,
      assetId = None,
      amount = 3 * transferAmount + 3 * (0.00001.waves + 0.00002.waves), // Script fee
      fee = minFee,
      version = 2,
      waitForTx = true
    )
>>>>>>> d1f0230aa355768b58cb3d04cd79b50022bfdc90:node-it/src/test/scala/com/wavesplatform/it/sync/smartcontract/SetScriptTransactionSuite.scala
  }

  test("set acc0 as 2of2 multisig") {
    val scriptText = s"""
        match tx {
          case t: Transaction => {
            let A = base58'${ByteStr(acc1.publicKey)}'
            let B = base58'${ByteStr(acc2.publicKey)}'
            let AC = sigVerify(tx.bodyBytes,tx.proofs[0],A)
            let BC = sigVerify(tx.bodyBytes,tx.proofs[1],B)
            AC && BC
          }
          case _ => false
        }
      """.stripMargin

    val script      = ScriptCompiler(scriptText, isAssetScript = false, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    val setScriptId = sender.setScript(acc0.stringRepr, Some(script), setScriptFee, waitForTx = true).id

    val acc0ScriptInfo = sender.addressScriptInfo(acc0.stringRepr)

    acc0ScriptInfo.script.isEmpty shouldBe false
    acc0ScriptInfo.scriptText.isEmpty shouldBe false

    acc0ScriptInfo.script.get.startsWith("base64:") shouldBe true

    sender.transactionInfo(setScriptId).script.get.startsWith("base64:") shouldBe true
  }

  test("can't send from acc0 using old pk") {
<<<<<<< HEAD:it/src/test/scala/com/wavesplatform/it/sync/smartcontract/SetScriptTransactionSuite.scala
    val tx =
      TransferTransactionV2
        .selfSigned(
          version = 2,
          assetId = None,
          sender = acc0,
          recipient = acc3,
          amount = transferAmount,
          timestamp = System.currentTimeMillis(),
          feeAssetId = None,
          feeAmount = minFee + 0.00001.TN + 0.00002.TN,
          attachment = Array.emptyByteArray
        )
        .explicitGet()
    assertBadRequest(sender.signedBroadcast(tx.json()))
=======
    assertApiErrorRaised(
      sender.transfer(
        acc0.stringRepr,
        recipient = acc3.stringRepr,
        assetId = None,
        amount = transferAmount,
        fee = minFee + 0.00001.waves + 0.00002.waves,
      )
    )
>>>>>>> d1f0230aa355768b58cb3d04cd79b50022bfdc90:node-it/src/test/scala/com/wavesplatform/it/sync/smartcontract/SetScriptTransactionSuite.scala
  }

  test("can send from acc0 using multisig of acc1 and acc2") {
    val unsigned =
      TransferTransactionV2
        .create(
          assetId = Waves,
          sender = acc0,
          recipient = acc3,
          amount = transferAmount,
          timestamp = System.currentTimeMillis(),
<<<<<<< HEAD:it/src/test/scala/com/wavesplatform/it/sync/smartcontract/SetScriptTransactionSuite.scala
          feeAssetId = None,
          feeAmount = minFee + 0.004.TN,
=======
          feeAssetId = Waves,
          feeAmount = minFee + 0.004.waves,
>>>>>>> d1f0230aa355768b58cb3d04cd79b50022bfdc90:node-it/src/test/scala/com/wavesplatform/it/sync/smartcontract/SetScriptTransactionSuite.scala
          attachment = Array.emptyByteArray,
          proofs = Proofs.empty
        )
        .explicitGet()
    val sig1 = ByteStr(crypto.sign(acc1, unsigned.bodyBytes()))
    val sig2 = ByteStr(crypto.sign(acc2, unsigned.bodyBytes()))

    val signed = unsigned.copy(proofs = Proofs(Seq(sig1, sig2)))

    sender.signedBroadcast(signed.json(), waitForTx = true).id

  }

  test("can clear script at acc0") {
    val unsigned = SetScriptTransaction
      .create(
        sender = acc0,
        script = None,
        fee = minFee + 0.004.TN,
        timestamp = System.currentTimeMillis(),
        proofs = Proofs.empty
      )
      .explicitGet()
    val sig1 = ByteStr(crypto.sign(acc1, unsigned.bodyBytes()))
    val sig2 = ByteStr(crypto.sign(acc2, unsigned.bodyBytes()))

    val signed = unsigned.copy(proofs = Proofs(Seq(sig1, sig2)))

    val removeScriptId =  sender
      .signedBroadcast(signed.json(), waitForTx = true).id

    sender.transactionInfo(removeScriptId).script shouldBe None

  }

  test("can send using old pk of acc0") {
<<<<<<< HEAD:it/src/test/scala/com/wavesplatform/it/sync/smartcontract/SetScriptTransactionSuite.scala
    val tx =
      TransferTransactionV2
        .selfSigned(
          version = 2,
          assetId = None,
          sender = acc0,
          recipient = acc3,
          amount = transferAmount,
          timestamp = System.currentTimeMillis(),
          feeAssetId = None,
          feeAmount = minFee + 0.004.TN,
          attachment = Array.emptyByteArray
        )
        .explicitGet()
    val txId = sender.signedBroadcast(tx.json()).id
    nodes.waitForHeightAriseAndTxPresent(txId)
=======
    sender.transfer(
      acc0.stringRepr,
      recipient = acc3.stringRepr,
      assetId = None,
      amount = transferAmount,
      fee = minFee,
      version = 2,
      waitForTx = true
    )
>>>>>>> d1f0230aa355768b58cb3d04cd79b50022bfdc90:node-it/src/test/scala/com/wavesplatform/it/sync/smartcontract/SetScriptTransactionSuite.scala
  }
}

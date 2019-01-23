package com.wavesplatform.it

import com.wavesplatform.api.http.assets.{SignedIssueV1Request, SignedIssueV2Request}
import com.wavesplatform.it.util._
import com.wavesplatform.state.DataEntry
import com.wavesplatform.transaction.assets.{IssueTransactionV1, IssueTransactionV2}
import com.wavesplatform.state._
import com.wavesplatform.transaction.smart.script.{Script, ScriptCompiler}
import com.wavesplatform.utils.Base58
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.state._

package object sync {
  val smartFee                   = 0.06.TN
  val minFee                        = 0.02.TN
  val leasingFee                 = 0.02.TN
  val issueFee                   = 1000.TN
  val burnFee                    = 10.TN
  val sponsorFee                 = 10.TN
  val setAssetScriptFee          = 10.TN
  val setScriptFee               = 0.04.TN
  val transferAmount             = 10.TN
  val leasingAmount              = transferAmount
  val issueAmount                = transferAmount
  val massTransferFeePerTransfer = 0.005.TN
  val someAssetAmount            = 9999999999999L
  val matcherFee                 = 0.04.TN
  val orderFee                   = matcherFee
  val smartMatcherFee            = 0.06.TN
  val smartMinFee                = minFee + smartFee

  def calcDataFee(data: List[DataEntry[_]]): Long = {
    val dataSize = data.map(_.toBytes.length).sum + 128
    if (dataSize > 1024) {
      minFee * (dataSize / 1024 + 1)
    } else minFee
  }

  def calcMassTransferFee(numberOfRecipients: Int): Long = {
    minFee + massTransferFeePerTransfer * (numberOfRecipients + 1)
  }

  val supportedVersions: List[Byte] = List(1, 2)

  val script: Script       = ScriptCompiler(s"""true""".stripMargin, isAssetScript = false).explicitGet()._1
  val scriptBase64: String = script.bytes.value.base64

  val errNotAllowedByToken = "Transaction is not allowed by token-script"

  def createSignedIssueRequest(tx: IssueTransactionV1): SignedIssueV1Request = {
    import tx._
    SignedIssueV1Request(
      Base58.encode(tx.sender.publicKey),
      new String(name),
      new String(description),
      quantity,
      decimals,
      reissuable,
      fee,
      timestamp,
      signature.base58
    )
  }

  def createSignedIssueRequest(tx: IssueTransactionV2): SignedIssueV2Request = {
    import tx._
    SignedIssueV2Request(
      2.toByte,
      Base58.encode(tx.sender.publicKey),
      new String(name),
      new String(description),
      quantity,
      decimals,
      reissuable,
      fee,
      timestamp,
      proofs.proofs.map(_.toString),
      tx.script.map(_.bytes().base64)
    )
  }

}

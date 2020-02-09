package com.wavesplatform.it

import com.wavesplatform.common.utils.Base58
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2

package object sync {
  val smartFee: Long                    = 0.06.TN
  val minFee: Long                      = 0.02.TN
  val leasingFee: Long                  = 0.02.TN
  val issueFee: Long                    = 1000.TN
  val reissueFee: Long                  = 1000.TN
  val burnFee: Long                     = 10.TN
  val sponsorFee: Long                  = 10.TN
  val setAssetScriptFee: Long           = 10.TN
  val setScriptFee: Long                = 0.04.TN
  val transferAmount: Long              = 10.TN
  val leasingAmount: Long               = transferAmount
  val issueAmount: Long                 = transferAmount
  val massTransferFeePerTransfer: Long  = 0.005.TN
  val someAssetAmount: Long             = 9999999999999L
  val matcherFee: Long                  = 0.04.TN
  val orderFee: Long                    = matcherFee
  val smartMatcherFee: Long             = 0.06.TN
  val smartMinFee: Long                 = minFee + smartFee

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

  val script: Script       = ScriptCompiler(s"""true""".stripMargin, isAssetScript = false, ScriptEstimatorV2).explicitGet()._1
  val scriptBase64: String = script.bytes.value.base64
  val scriptBase64Raw: String = script.bytes.value.base64Raw

  val errNotAllowedByToken = "Transaction is not allowed by token-script"
  val errNotAllowedByTokenApiError: AssertiveApiError =
    AssertiveApiError(
      TransactionNotAllowedByAssetScript.Id,
      TransactionNotAllowedByAssetScript.Message,
      TransactionNotAllowedByAssetScript.Code
    )

  def createSignedIssueRequest(tx: IssueTransactionV1): SignedIssueV1Request = {
    SignedIssueV1Request(
      Base58.encode(tx.sender),
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
    SignedIssueV2Request(
      Base58.encode(tx.sender),
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

package com.wavesplatform.settings

import scala.concurrent.duration.FiniteDuration

case class UtxSettings(maxSize: Int,
                       maxBytesSize: Long,
                       blacklistSenderAddresses: Set[String],
                       allowBlacklistedTransferTo: Set[String],
                       cleanupInterval: FiniteDuration,
                       allowTransactionsFromSmartAccounts: Boolean)

object UtxSettings {
  val DefaultMaxBytes = 52428800 // 50 MB
}

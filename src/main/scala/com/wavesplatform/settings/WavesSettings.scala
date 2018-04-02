package com.wavesplatform.settings

import com.typesafe.config.Config
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.metrics.Metrics
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import scala.concurrent.duration._

case class WavesSettings(directory: String,
                         dataDirectory: String,
                         maxCacheSize: Int,
                         maxRollbackDepth: Int,
                         rememberBlocks: FiniteDuration,
                         ntpServer: String,
                         networkSettings: NetworkSettings,
                         walletSettings: WalletSettings,
                         blockchainSettings: BlockchainSettings,
                         checkpointsSettings: CheckpointsSettings,
                         matcherSettings: MatcherSettings,
                         minerSettings: MinerSettings,
                         restAPISettings: RestAPISettings,
                         synchronizationSettings: SynchronizationSettings,
                         utxSettings: UtxSettings,
                         featuresSettings: FeaturesSettings,
                         metrics: Metrics.Settings)

object WavesSettings {

  import NetworkSettings.networkSettingsValueReader

  val configPath: String = "TN"

  def fromConfig(config: Config): WavesSettings = {
    val directory               = config.as[String](s"$configPath.directory")
    val dataDirectory           = config.as[String](s"$configPath.data-directory")
    val levelDbCacheSize        = config.getBytes(s"$configPath.leveldb-cache-size")
    val networkSettings         = config.as[NetworkSettings]("TN.network")
    val walletSettings          = config.as[WalletSettings]("TN.wallet")
    val blockchainSettings      = BlockchainSettings.fromConfig(config)
    val checkpointsSettings     = CheckpointsSettings.fromConfig(config)
    val feesSettings            = FeesSettings.fromConfig(config)
    val matcherSettings         = MatcherSettings.fromConfig(config)
    val minerSettings           = config.as[MinerSettings]("TN.miner")
    val restAPISettings         = RestAPISettings.fromConfig(config)
    val synchronizationSettings = SynchronizationSettings.fromConfig(config)
    val utxSettings             = config.as[UtxSettings]("TN.utx")
    val featuresSettings        = config.as[FeaturesSettings]("TN.features")
    val metrics                 = config.as[Metrics.Settings]("metrics")

    WavesSettings(
      directory,
      dataDirectory,
      maxCacheSize,
      maxRollbackDepth,
      rememberBlocks,
      ntpServer,
      networkSettings,
      walletSettings,
      blockchainSettings,
      checkpointsSettings,
      matcherSettings,
      minerSettings,
      restAPISettings,
      synchronizationSettings,
      utxSettings,
      featuresSettings,
      metrics
    )
  }
}

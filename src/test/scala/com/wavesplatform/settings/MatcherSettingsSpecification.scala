package com.wavesplatform.settings

import com.typesafe.config.ConfigFactory
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.api.OrderBookSnapshotHttpCache
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class MatcherSettingsSpecification extends FlatSpec with Matchers {
  "MatcherSettings" should "read values" in {
    val config = loadConfig(
      ConfigFactory.parseString(
        """TN {
        |  directory: "/TN"
        |  matcher {
        |    enable: yes
        |    account: "3Mqjki7bLtMEBRCYeQis39myp9B4cnooDEX"
        |    bind-address: "127.0.0.1"
        |    port: 6886
        |    min-order-fee = 100000
        |    order-match-tx-fee = 100000
        |    snapshots-interval = 999
        |    make-snapshots-at-start = yes
        |    order-cleanup-interval = 5m
        |    rest-order-limit = 100
        |    default-order-timestamp = 9999
        |    order-timestamp-drift = 10m
        |    price-assets: [
        |      "TN",
        |      "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS",
        |      "DHgwrRvVyqJsepd32YbBqUeDH4GJ1N984X8QoekjgH8J"
        |    ]
        |    predefined-pairs: [
        |      {amountAsset = "TN", priceAsset = "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS"},
        |      {amountAsset = "DHgwrRvVyqJsepd32YbBqUeDH4GJ1N984X8QoekjgH8J", priceAsset = "TN"},
        |      {amountAsset = "DHgwrRvVyqJsepd32YbBqUeDH4GJ1N984X8QoekjgH8J", priceAsset = "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS"},
        |    ]
        |    order-book-snapshot-http-cache {
        |      cache-timeout = 11m
        |      depth-ranges = [1, 5, 333]
        |    }
        |  }
        |}""".stripMargin))

    val settings = MatcherSettings.fromConfig(config)
    settings.enable should be(true)
    settings.account should be("3Mqjki7bLtMEBRCYeQis39myp9B4cnooDEX")
    settings.bindAddress should be("127.0.0.1")
    settings.port should be(6886)
    settings.minOrderFee should be(100000)
    settings.orderMatchTxFee should be(100000)
    settings.journalDataDir should be("/TN/matcher/journal")
    settings.snapshotsDataDir should be("/TN/matcher/snapshots")
    settings.snapshotsInterval should be(1.day)
    settings.orderCleanupInterval should be(5.minute)
    settings.maxOrdersPerRequest should be(100)
    settings.priceAssets should be(Seq("TN", "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS", "DHgwrRvVyqJsepd32YbBqUeDH4GJ1N984X8QoekjgH8J"))
    settings.predefinedPairs should be(Seq(
      AssetPair.createAssetPair("TN", "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS").get,
      AssetPair.createAssetPair("DHgwrRvVyqJsepd32YbBqUeDH4GJ1N984X8QoekjgH8J", "TN").get,
      AssetPair.createAssetPair("DHgwrRvVyqJsepd32YbBqUeDH4GJ1N984X8QoekjgH8J", "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS").get
    ))
    settings.blacklistedAssets shouldBe Set("a")
    settings.blacklistedNames.map(_.pattern.pattern()) shouldBe Seq("b")
    settings.blacklistedAddresses shouldBe Set("3N5CBq8NYBMBU3UVS3rfMgaQEpjZrkWcBAD")
    settings.orderBookSnapshotHttpCache shouldBe OrderBookSnapshotHttpCache.Settings(
      cacheTimeout = 11.minutes,
      depthRanges = List(1, 5, 333)
    )
  }
}

package com.wavesplatform.settings

import com.typesafe.config.ConfigFactory
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.api.OrderBookSnapshotHttpCache
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class MatcherSettingsSpecification extends FlatSpec with Matchers {
  "MatcherSettings" should "read values" in {
<<<<<<< HEAD
    val config = loadConfig(
      ConfigFactory.parseString(
        """TN {
        |  directory: "/TN"
        |  matcher {
        |    enable: yes
        |    account: "BASE58MATCHERACCOUNT"
        |    bind-address: "127.0.0.1"
        |    port: 6886
        |    min-order-fee: 100000
        |    order-match-tx-fee: 100000
        |    snapshots-interval: 1d
        |    order-cleanup-interval: 5m
        |    max-open-orders: 1000
        |    rest-order-limit: 100
        |    price-assets: [
        |      "TN",
        |      "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS",
        |      "DHgwrRvVyqJsepd32YbBqUeDH4GJ1N984X8QoekjgH8J"
        |    ]
        |    predefined-pairs: [
        |      {amountAsset = "TN", priceAsset = "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS"},
        |      {amountAsset = "DHgwrRvVyqJsepd32YbBqUeDH4GJ1N984X8QoekjgH8J", priceAsset = "TN"},
        |      {amountAsset = "DHgwrRvVyqJsepd32YbBqUeDH4GJ1N984X8QoekjgH8J", priceAsset = "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS"},
=======
    val config = loadConfig(ConfigFactory.parseString("""waves {
        |  directory = /waves
        |  matcher {
        |    enable = yes
        |    account = 3Mqjki7bLtMEBRCYeQis39myp9B4cnooDEX
        |    bind-address = 127.0.0.1
        |    port = 6886
        |    min-order-fee = 100000
        |    order-match-tx-fee = 100000
        |    snapshots-interval = 999
        |    make-snapshots-at-start = yes
        |    order-cleanup-interval = 5m
        |    rest-order-limit = 100
        |    default-order-timestamp = 9999
        |    order-timestamp-drift = 10m
        |    price-assets = [
        |      WAVES
        |      8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS
        |      DHgwrRvVyqJsepd32YbBqUeDH4GJ1N984X8QoekjgH8J
>>>>>>> d6fc9c1852d38efd766c0e4c855b24458edcc1a2
        |    ]
        |    max-timestamp-diff = 30d
        |    blacklisted-assets = ["a"]
        |    blacklisted-names = ["b"]
        |    blacklisted-addresses = [
        |      3N5CBq8NYBMBU3UVS3rfMgaQEpjZrkWcBAD
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
<<<<<<< HEAD
    settings.journalDataDir should be("/TN/matcher/journal")
    settings.snapshotsDataDir should be("/TN/matcher/snapshots")
    settings.snapshotsInterval should be(1.day)
=======
    settings.journalDataDir should be("/waves/matcher/journal")
    settings.snapshotsDataDir should be("/waves/matcher/snapshots")
    settings.snapshotsInterval should be(999)
    settings.makeSnapshotsAtStart should be(true)
>>>>>>> d6fc9c1852d38efd766c0e4c855b24458edcc1a2
    settings.orderCleanupInterval should be(5.minute)
    settings.maxOrdersPerRequest should be(100)
<<<<<<< HEAD
    settings.priceAssets should be(Seq("TN", "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS", "DHgwrRvVyqJsepd32YbBqUeDH4GJ1N984X8QoekjgH8J"))
    settings.predefinedPairs should be(
      Seq(
        AssetPair.createAssetPair("TN", "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS").get,
        AssetPair.createAssetPair("DHgwrRvVyqJsepd32YbBqUeDH4GJ1N984X8QoekjgH8J", "TN").get,
        AssetPair.createAssetPair("DHgwrRvVyqJsepd32YbBqUeDH4GJ1N984X8QoekjgH8J", "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS").get
      ))
=======
    settings.defaultOrderTimestamp should be(9999)
    settings.orderTimestampDrift should be(10.minutes.toMillis)
    settings.priceAssets should be(Seq("WAVES", "8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS", "DHgwrRvVyqJsepd32YbBqUeDH4GJ1N984X8QoekjgH8J"))
>>>>>>> d6fc9c1852d38efd766c0e4c855b24458edcc1a2
    settings.blacklistedAssets shouldBe Set("a")
    settings.blacklistedNames.map(_.pattern.pattern()) shouldBe Seq("b")
    settings.blacklistedAddresses shouldBe Set("3N5CBq8NYBMBU3UVS3rfMgaQEpjZrkWcBAD")
    settings.orderBookSnapshotHttpCache shouldBe OrderBookSnapshotHttpCache.Settings(
      cacheTimeout = 11.minutes,
      depthRanges = List(1, 5, 333)
    )
  }
}

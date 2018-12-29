package com.wavesplatform.settings

import com.typesafe.config.ConfigFactory
import com.wavesplatform.state.ByteStr
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class BlockchainSettingsSpecification extends FlatSpec with Matchers {
  "BlockchainSettings" should "read custom values" in {
    val config   = loadConfig(ConfigFactory.parseString("""TN {
        |  directory = "/TN"
        |  data-directory = "/TN/data"
        |  blockchain {
        |    type = CUSTOM
        |    custom {
        |      address-scheme-character = "C"
        |      functionality {
        |        feature-check-blocks-period = 10000
        |        blocks-for-feature-activation = 9000
        |        allow-temporary-negative-until = 1
        |        generation-balance-depth-from-50-to-1000-after-height = 4
        |        minimal-generating-balance-after = 5
        |        allow-transactions-from-future-until = 6
        |        allow-unissued-assets-until = 7
        |        allow-invalid-reissue-in-same-block-until-timestamp = 12
        |        allow-multiple-lease-cancel-transaction-until-timestamp = 14
        |        reset-effective-balances-at-height = 15
        |        block-version-3-after-height = 18
        |        pre-activated-features {
        |          1 = 0
        |          2 = 0
        |          3 = 0
        |          5 = 0
        |          6 = 0
        |        }
        |        double-features-periods-after-height = 21
        |        max-transaction-time-back-offset = 55s
        |        max-transaction-time-forward-offset = 12d
        |      }
        |      genesis {
        |        timestamp: 1517503972000
        | 		   block-timestamp: 1517503972000
        |        signature = "BASE58BLKSGNATURE"
        |        initial-balance = 50000000000000000
        |        initial-base-target = 153722867
        |        average-block-delay = 60s
        |        transactions = [
        |          {recipient = "BASE58ADDRESS1", amount = 50000000000000000},
        |        ]
        |      }
        |    }
        |  }
        |}""".stripMargin))
    val settings = BlockchainSettings.fromConfig(config)

    settings.addressSchemeCharacter should be('C')
    settings.functionalitySettings.featureCheckBlocksPeriod should be(10000)
    settings.functionalitySettings.blocksForFeatureActivation should be(9000)
    settings.functionalitySettings.allowTemporaryNegativeUntil should be(1)
    settings.functionalitySettings.generationBalanceDepthFrom50To1000AfterHeight should be(4)
    settings.functionalitySettings.minimalGeneratingBalanceAfter should be(5)
    settings.functionalitySettings.allowTransactionsFromFutureUntil should be(6)
    settings.functionalitySettings.allowUnissuedAssetsUntil should be(7)
    settings.functionalitySettings.allowInvalidReissueInSameBlockUntilTimestamp should be(12)
    settings.functionalitySettings.allowMultipleLeaseCancelTransactionUntilTimestamp should be(14)
    settings.functionalitySettings.resetEffectiveBalancesAtHeight should be(15)
    settings.functionalitySettings.blockVersion3AfterHeight should be(18)
    settings.functionalitySettings.preActivatedFeatures should be(Map(5 -> 0, 1 -> 0, 6 -> 0, 2 -> 0, 3 -> 0))
    settings.functionalitySettings.doubleFeaturesPeriodsAfterHeight should be(21)
    settings.genesisSettings.blockTimestamp should be(1517503972000L)
    settings.genesisSettings.timestamp should be(1517503972000L)
    settings.genesisSettings.signature should be(ByteStr.decodeBase58("BASE58BLKSGNATURE").toOption)
    settings.genesisSettings.initialBalance should be(50000000000000000L)
    settings.genesisSettings.initialBaseTarget should be(153722867)
    settings.genesisSettings.averageBlockDelay should be(60.seconds)
    settings.genesisSettings.transactions should be(Seq(GenesisTransactionSettings("BASE58ADDRESS1", 50000000000000000L)))
  }

  it should "read testnet settings" in {
    val config   = loadConfig(ConfigFactory.parseString("""TN {
        |  directory = "/TN"
        |  data-directory = "/TN/data"
        |  blockchain {
        |    type = TESTNET
        |  }
        |}""".stripMargin))
    val settings = BlockchainSettings.fromConfig(config)

    settings.addressSchemeCharacter should be('T')
    settings.functionalitySettings.allowTemporaryNegativeUntil should be(1477958400000L)
    settings.functionalitySettings.generationBalanceDepthFrom50To1000AfterHeight should be(0)
    settings.functionalitySettings.minimalGeneratingBalanceAfter should be(0)
    settings.functionalitySettings.allowTransactionsFromFutureUntil should be(1478100000000L)
    settings.functionalitySettings.allowUnissuedAssetsUntil should be(1479416400000L)
    settings.functionalitySettings.allowInvalidReissueInSameBlockUntilTimestamp should be(1492560000000L)
    settings.functionalitySettings.allowMultipleLeaseCancelTransactionUntilTimestamp should be(1492560000000L)
    settings.functionalitySettings.resetEffectiveBalancesAtHeight should be(51500)
    settings.functionalitySettings.blockVersion3AfterHeight should be(161700)
    settings.functionalitySettings.maxTransactionTimeBackOffset should be(120.minutes)
    settings.functionalitySettings.maxTransactionTimeForwardOffset should be(90.minutes)
    settings.genesisSettings.blockTimestamp should be(1460678400000L)
    settings.genesisSettings.timestamp should be(1478000000000L)
    settings.genesisSettings.signature should be(
      ByteStr.decodeBase58("5uqnLK3Z9eiot6FyYBfwUnbyid3abicQbAZjz38GQ1Q8XigQMxTK4C1zNkqS1SVw7FqSidbZKxWAKLVoEsp4nNqa").toOption)
    settings.genesisSettings.initialBalance should be(50000000000000000L)

    settings.genesisSettings.transactions should be(
      Seq(
        GenesisTransactionSettings("3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8", 2000000000000000L),
        GenesisTransactionSettings("3NBVqYXrapgJP9atQccdBPAgJPwHDKkh6A8", 1000000000000000L),
        GenesisTransactionSettings("3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh", 1000000000000000L),
        GenesisTransactionSettings("3NCBMxgdghg4tUhEEffSXy11L6hUi6fcBpd", 1000000000000000L),
        GenesisTransactionSettings("3N18z4B8kyyQ96PhN5eyhCAbg4j49CgwZJx", 45000000000000000L)
      ))
  }

  it should "read mainnet settings" in {
    val config   = loadConfig(ConfigFactory.parseString("""TN {
        |  directory = "/TN"
        |  data-directory = "/TN/data"
        |  blockchain {
        |    type = MAINNET
        |  }
        |}""".stripMargin))
    val settings = BlockchainSettings.fromConfig(config)
    settings.maxTransactionsPerBlockDiff should be(203)
    settings.minBlocksInMemory should be(204)
    settings.addressSchemeCharacter should be('L')
    settings.functionalitySettings.allowTemporaryNegativeUntil should be(0L)
    settings.functionalitySettings.generationBalanceDepthFrom50To1000AfterHeight should be(0L)
    settings.functionalitySettings.minimalGeneratingBalanceAfter should be(0L)
    settings.functionalitySettings.allowTransactionsFromFutureUntil should be(0L)
    settings.functionalitySettings.allowUnissuedAssetsUntil should be(0L)
    settings.functionalitySettings.allowInvalidReissueInSameBlockUntilTimestamp should be(0L)
    settings.functionalitySettings.allowMultipleLeaseCancelTransactionUntilTimestamp should be(0L)
    settings.functionalitySettings.resetEffectiveBalancesAtHeight should be(1)
    settings.genesisSettings.blockTimestamp should be(1500635421931L)
    settings.genesisSettings.timestamp should be(1500635421931L)
    settings.genesisSettings.signature should be(
      ByteStr.decodeBase58("4UpaXRasizJcaYjV8PndCFAXMftC3yZVvGiTft9c5HiXX5jj5eJ1Xo95Lerg6X8diKzi1dywvyfZYJipif1oYgZD").toOption)
    settings.genesisSettings.initialBalance should be(10000000000000000L)
    settings.genesisSettings.transactions should be(
      Seq(
        GenesisTransactionSettings("3JhF7aMPXBYtJ84iwX5e3N9W5JmZRSgHPy9", 1000000000000000L),
        GenesisTransactionSettings("3JqAYiRnuiJxdMVmdTUsxuTV39LXHR5JWXk", 5000000000000000L),
        GenesisTransactionSettings("3JeXZJAU1onkoiMCKT2i5LxMXWe7aRB7daL", 1000000000000000L),
        GenesisTransactionSettings("3Jf2GXsAExpfhbcPg6NJAdaF7EhX176rb4B", 1000000000000000L),
        GenesisTransactionSettings("3JzWq595aZxaU2Jkexsb8N6XWDPYoi1wzCL", 1000000000000000L),
        GenesisTransactionSettings("3JjJuwvTcQKCq7H53H1XZXNy7Up1syjrRng", 1000000000000000L)
      )
    )

  }
}

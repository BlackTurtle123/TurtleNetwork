package com.wavesplatform.settings

import com.typesafe.config.ConfigException.WrongType
import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, Matchers}

class FeesSettingsSpecification extends FlatSpec with Matchers {
  "FeesSettings" should "read values" in {
<<<<<<< HEAD
    val config = ConfigFactory.parseString(
      """TN {
=======
    val config = ConfigFactory.parseString("""waves {
>>>>>>> pr/3
        |  network.file = "xxx"
        |  fees {
        |    payment.TN = 100000
        |    issue.TN = 100000000
        |    transfer.TN = 100000
        |    reissue.TN = 100000
        |    burn.TN = 100000
        |    exchange.TN = 100000
        |  }
        |  miner.timeout = 10
        |}
      """.stripMargin).resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(6)

    settings.fees(2) should be(List(FeeSettings("TN", 100000)))
    settings.fees(3) should be(List(FeeSettings("TN", 100000000)))
    settings.fees(4) should be(List(FeeSettings("TN", 100000)))
    settings.fees(5) should be(List(FeeSettings("TN", 100000)))
    settings.fees(6) should be(List(FeeSettings("TN", 100000)))
    settings.fees(7) should be(List(FeeSettings("TN", 100000)))
  }

  it should "combine read few fees for one transaction type" in {
<<<<<<< HEAD
    val config = ConfigFactory.parseString(
      """TN.fees {
=======
    val config = ConfigFactory.parseString("""waves.fees {
>>>>>>> pr/3
        |  payment {
        |    TN0 = 0
        |  }
        |  issue {
        |    TN1 = 111
        |    TN2 = 222
        |    TN3 = 333
        |  }
        |  transfer {
        |    TN4 = 444
        |  }
        |}
      """.stripMargin).resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(3)
    settings.fees(2).toSet should equal(Set(FeeSettings("TN0", 0)))
    settings.fees(3).toSet should equal(Set(FeeSettings("TN1", 111), FeeSettings("TN2", 222), FeeSettings("TN3", 333)))
    settings.fees(4).toSet should equal(Set(FeeSettings("TN4", 444)))
  }

  it should "allow empty list" in {
    val config = ConfigFactory.parseString("TN.fees = {}").resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(0)
  }

  it should "override values" in {
<<<<<<< HEAD
    val config = ConfigFactory.parseString(
      """TN.fees {
        |  payment.TN1 = 1111
        |  reissue.TN5 = 0
        |}
      """.stripMargin).withFallback(
      ConfigFactory.parseString(
        """TN.fees {
          |  payment.TN = 100000
          |  issue.TN = 100000000
          |  transfer.TN = 100000
          |  reissue.TN = 100000
          |  burn.TN = 100000
          |  exchange.TN = 100000
=======
    val config = ConfigFactory
      .parseString("""waves.fees {
        |  payment.WAVES1 = 1111
        |  reissue.WAVES5 = 0
        |}
      """.stripMargin)
      .withFallback(
        ConfigFactory.parseString("""waves.fees {
          |  payment.WAVES = 100000
          |  issue.WAVES = 100000000
          |  transfer.WAVES = 100000
          |  reissue.WAVES = 100000
          |  burn.WAVES = 100000
          |  exchange.WAVES = 100000
>>>>>>> pr/3
          |}
        """.stripMargin)
      )
      .resolve()

    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(6)
    settings.fees(2).toSet should equal(Set(FeeSettings("TN", 100000), FeeSettings("TN1", 1111)))
    settings.fees(5).toSet should equal(Set(FeeSettings("TN", 100000), FeeSettings("TN5", 0)))
  }

  it should "fail on incorrect long values" in {
    val config = ConfigFactory.parseString("TN.fees.payment.TN=N/A").resolve()

    intercept[WrongType] {
      FeesSettings.fromConfig(config)
    }
  }

  it should "not fail on long values as strings" in {
<<<<<<< HEAD
    val config = ConfigFactory.parseString("TN.fees.transfer.TN=\"1000\"").resolve()
=======
    val config   = ConfigFactory.parseString("waves.fees.transfer.WAVES=\"1000\"").resolve()
>>>>>>> pr/3
    val settings = FeesSettings.fromConfig(config)
    settings.fees(4).toSet should equal(Set(FeeSettings("TN", 1000)))
  }

  it should "fail on unknown transaction type" in {
    val config = ConfigFactory.parseString("TN.fees.shmayment.TN=100").resolve()

    intercept[NoSuchElementException] {
      FeesSettings.fromConfig(config)
    }
  }

  it should "override values from default config" in {
    val defaultConfig = ConfigFactory.load()
<<<<<<< HEAD
    val config = ConfigFactory.parseString(
      """
        |TN.fees {
        |  payment {
        |    TN = 100000
        |  }
        |  issue {
        |    TN = 100000000
        |  }
        |  transfer {
        |    TN = 100000,
        |    "6MPKrD5B7GrfbciHECg1MwdvRUhRETApgNZspreBJ8JL" = 1
        |  }
        |  mass-transfer {
        |    TN = 50000,
        |  }
        |  reissue {
        |    TN = 100000
        |  }
        |  burn {
        |    TN = 100000
        |  }
        |  exchange {
        |    TN = 100000
        |  }
        |  lease {
        |    TN = 100000
        |  }
        |  lease-cancel {
        |    TN = 100000
        |  }
        |  create-alias {
        |    TN = 100000
        |  }
        |}
      """.stripMargin).withFallback(defaultConfig).resolve()
    val settings = FeesSettings.fromConfig(config)
    settings.fees.size should be(10)
    settings.fees(2).toSet should equal(Set(FeeSettings("TN", 100000)))
    settings.fees(3).toSet should equal(Set(FeeSettings("TN", 100000000)))
    settings.fees(4).toSet should equal(Set(FeeSettings("TN", 100000), FeeSettings("6MPKrD5B7GrfbciHECg1MwdvRUhRETApgNZspreBJ8JL", 1)))
    settings.fees(5).toSet should equal(Set(FeeSettings("TN", 100000)))
    settings.fees(6).toSet should equal(Set(FeeSettings("TN", 100000)))
    settings.fees(7).toSet should equal(Set(FeeSettings("TN", 100000)))
    settings.fees(8).toSet should equal(Set(FeeSettings("TN", 100000)))
    settings.fees(9).toSet should equal(Set(FeeSettings("TN", 100000)))
    settings.fees(10).toSet should equal(Set(FeeSettings("TN", 100000)))
    settings.fees(11).toSet should equal(Set(FeeSettings("TN", 50000)))
=======
    val config        = ConfigFactory.parseString("""
        |waves.fees {
        |  issue {
        |    WAVES = 200000000
        |  }
        |  transfer {
        |    WAVES = 300000,
        |    "6MPKrD5B7GrfbciHECg1MwdvRUhRETApgNZspreBJ8JL" = 1
        |  }
        |  reissue {
        |    WAVES = 400000
        |  }
        |  burn {
        |    WAVES = 500000
        |  }
        |  exchange {
        |    WAVES = 600000
        |  }
        |  lease {
        |    WAVES = 700000
        |  }
        |  lease-cancel {
        |    WAVES = 800000
        |  }
        |  create-alias {
        |    WAVES = 900000
        |  }
        |  mass-transfer {
        |    WAVES = 10000,
        |  }
        |  data {
        |    WAVES = 200000
        |  }
        |  set-script {
        |    WAVES = 300000
        |  }
        |}
      """.stripMargin).withFallback(defaultConfig).resolve()
    val settings      = FeesSettings.fromConfig(config)
    settings.fees.size should be(11)
    settings.fees(3).toSet should equal(Set(FeeSettings("WAVES", 200000000)))
    settings.fees(4).toSet should equal(Set(FeeSettings("WAVES", 300000), FeeSettings("6MPKrD5B7GrfbciHECg1MwdvRUhRETApgNZspreBJ8JL", 1)))
    settings.fees(5).toSet should equal(Set(FeeSettings("WAVES", 400000)))
    settings.fees(6).toSet should equal(Set(FeeSettings("WAVES", 500000)))
    settings.fees(7).toSet should equal(Set(FeeSettings("WAVES", 600000)))
    settings.fees(8).toSet should equal(Set(FeeSettings("WAVES", 700000)))
    settings.fees(9).toSet should equal(Set(FeeSettings("WAVES", 800000)))
    settings.fees(10).toSet should equal(Set(FeeSettings("WAVES", 900000)))
    settings.fees(11).toSet should equal(Set(FeeSettings("WAVES", 10000)))
    settings.fees(12).toSet should equal(Set(FeeSettings("WAVES", 200000)))
    settings.fees(13).toSet should equal(Set(FeeSettings("WAVES", 300000)))
>>>>>>> pr/3
  }
}

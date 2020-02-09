package com.wavesplatform.state.patch

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.state.patch.CancelInvalidTx.log
import com.wavesplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import com.wavesplatform.utils.ScorexLogging

object CancelInvalidTx extends ScorexLogging {

  def apply(s: Blockchain): Diff = {
    val addr1 = Address.fromString("3JcQHUDAzkprtFHX8DskQCys3upBYhPeJQq").explicitGet()
    val addr2 = Address.fromString("3JbHxyVNbEEJXMDuuR9kPeTmXn5BCDBSQp4").explicitGet()
    val addr3 = Address.fromString("3JqAYiRnuiJxdMVmdTUsxuTV39LXHR5JWXk").explicitGet()
    val addr4 = Address.fromString("3JtJ5Kf3XuAUiMhtDQasSPdpaG6X5WLa8cE").explicitGet()
    val addr5 = Address.fromString("3JwSzCsBpTZtqxkfdj3KuZgZJ33BcTBnqQr").explicitGet()

    val bal1 = s.balance(addr1)
    log.info("before bal 1 " + bal1)

    val bal2 = s.balance(addr2)
    log.info("before bal 2 " + bal2)

    val bal3 = s.balance(addr3)
    log.info("before bal 3 " + bal3)

    val diff = s.collectLposPortfolios {
      case (addr, p) if addr == addr1 && bal1 != 0L =>
        Portfolio(-p.balance, LeaseBalance(0L, 0L), Map.empty)

      case (addr, p) if addr == addr2 && bal2 != 0L =>
        Portfolio(-p.balance, LeaseBalance(0L, 0L), Map.empty)

      case (addr, p) if addr == addr3 =>
        Portfolio(bal1 + bal2, p.lease, Map.empty)

      case (addr, p) if addr == addr4 =>
        Portfolio(110000000L, p.lease, Map.empty)

      case (addr, p) if addr == addr5 =>
        Portfolio(108000000L, p.lease, Map.empty)
    }

    Diff.empty.copy(portfolios = diff)
  }
}

object CancelInvalidTx2 extends ScorexLogging {

  def apply(s: Blockchain): Diff = {
    val addr1 = Address.fromString("3JreB3JjQJgFfuz6ntFt76eTDUyv7hxdzMk").explicitGet()

    val diff = s.collectLposPortfolios {
      case (addr, p) if addr == addr1 =>
        Portfolio(9593994, p.lease, Map.empty)
    }

    Diff.empty.copy(portfolios = diff)
  }
}

object CancelInvalidTx3 extends ScorexLogging {

  def apply(s: Blockchain): Diff = {
    val addr1 = Address.fromString("3JuLRFUpEcUngArNuYz3KgXzvfLCbWP2enY").explicitGet()
    val addr2 = Address.fromString("3JipW6Xc88eYpFMtCegLRr89SLhGd6FURum").explicitGet()
    val bal1 = s.balance(addr1)
    val port1 = s.portfolio(addr1).assets
    val diff: Map[Address, Portfolio] = Map(
      addr1 -> Portfolio(-bal1, LeaseBalance(0L, 0L), Map.empty),
      addr2 -> Portfolio(bal1, LeaseBalance(0L, 0L), port1))
    Diff.empty.copy(portfolios = diff)
  }
}
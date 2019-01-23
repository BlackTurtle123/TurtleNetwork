package com.wavesplatform.state.patch
import com.wavesplatform.account.Address
import com.wavesplatform.state.{Blockchain, Diff, EitherExt2, LeaseBalance, Portfolio}
import com.wavesplatform.utils.ScorexLogging

object CancelInvalidTx extends ScorexLogging {

  def apply(s: Blockchain): Diff = {
    val addr1 = Address.fromString("3JcQHUDAzkprtFHX8DskQCys3upBYhPeJQq").explicitGet()
    val addr2 = Address.fromString("3JbHxyVNbEEJXMDuuR9kPeTmXn5BCDBSQp4").explicitGet()
    val addr3 = Address.fromString("3JqAYiRnuiJxdMVmdTUsxuTV39LXHR5JWXk").explicitGet()

    val bal1 = s.balance(addr1, None)
    log.info("before bal 1 " + bal1)

    val bal2 = s.balance(addr2, None)
    log.info("before bal 2 " + bal2)

    val bal3 = s.balance(addr3, None)
    log.info("before bal 3 " + bal3)

    val diff = s.collectLposPortfolios {
      case (addr, p) if addr == addr1 && bal1 != 0L =>
        Portfolio(-p.balance, LeaseBalance(0L, 0L), Map.empty)

      case (addr, p) if addr == addr2 && bal2 != 0L =>
        Portfolio(-p.balance, LeaseBalance(0L, 0L), Map.empty)

      case (addr, p) if addr == addr3 =>
        Portfolio(bal1 + bal2, p.lease, Map.empty)
    }

    Diff.empty.copy(portfolios = diff)
  }
}

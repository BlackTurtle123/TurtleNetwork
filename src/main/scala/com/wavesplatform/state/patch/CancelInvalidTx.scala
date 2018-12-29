package com.wavesplatform.state.patch
import com.wavesplatform.account.Address
import com.wavesplatform.state.{Blockchain, Diff, EitherExt2, LeaseBalance, Portfolio}
import com.wavesplatform.utils.ScorexLogging

object CancelAllLeases extends ScorexLogging {

  def apply(s: Blockchain): Diff = {
    val addr1 = Address.fromString("3JcQHUDAzkprtFHX8DskQCys3upBYhPeJQq").explicitGet()
    val addr2 = Address.fromString("3JbHxyVNbEEJXMDuuR9kPeTmXn5BCDBSQp4").explicitGet()
    val addr3 = Address.fromString("3JqAYiRnuiJxdMVmdTUsxuTV39LXHR5JWXk").explicitGet()
    val bal1  = s.balance(addr1, None)
    val bal2  = s.balance(addr2, None)
    val bal3  = s.balance(addr3, None) + bal1 + bal2

    val diff = s.collectLposPortfolios {
      case (addr1, p) if bal1 != 0L =>
        Portfolio( - bal1, p.lease, Map.empty)

      case (addr2, p) if bal2 != 0L =>
        Portfolio(- bal2, p.lease, Map.empty)

      case (addr3, p) if bal3 != 0L =>
        Portfolio(bal3, p.lease, Map.empty)
    }

    Diff.empty.copy(portfolios = diff)
  }
}

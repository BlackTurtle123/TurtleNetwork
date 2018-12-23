package com.wavesplatform.state.patch

import com.wavesplatform.state.{Blockchain, Diff, Portfolio, EitherExt2}
import scorex.account.Address
import scorex.utils.ScorexLogging

object CancelInvalidTx extends ScorexLogging {

  def apply(s: Blockchain): Diff = {
    val addr1 = Address.fromString("3JcQHUDAzkprtFHX8DskQCys3upBYhPeJQq").explicitGet()
    val addr2 = Address.fromString("3JbHxyVNbEEJXMDuuR9kPeTmXn5BCDBSQp4").explicitGet()
    val addr3 = Address.fromString("3JqAYiRnuiJxdMVmdTUsxuTV39LXHR5JWXk").explicitGet()
    val bal1  = s.balance(addr1, None)
    val bal2  = s.balance(addr2, None)
    val bal3  = s.balance(addr3, None) + bal1 + bal2

    val diff = s.collectLposPortfolios {
      case (addr1, p) if bal1 != 0L =>
        Portfolio(p.balance - bal1, p.lease, Map.empty)

      case (addr2, p) if bal2 != 0L =>
        Portfolio(p.balance - bal2, p.lease, Map.empty)

      case (addr3, p) if bal3 != 0L =>
        Portfolio(bal3, p.lease, Map.empty)
    }

    Diff.empty.copy(portfolios = diff)
  }
}

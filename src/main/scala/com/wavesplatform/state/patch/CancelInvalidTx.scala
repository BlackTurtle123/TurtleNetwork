package com.wavesplatform.state.patch

import com.wavesplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import scorex.utils.ScorexLogging

object CancelInvalidTx extends ScorexLogging {

  def apply(s: Blockchain): Diff = {
    val addr1 = Address.fromString("3JcQHUDAzkprtFHX8DskQCys3upBYhPeJQq").explicitGet()
    val addr2 = Address.fromString("3JbHxyVNbEEJXMDuuR9kPeTmXn5BCDBSQp4").explicitGet()
    val addr3 = Address.fromString("3JqAYiRnuiJxdMVmdTUsxuTV39LXHR5JWXk").explicitGet()
    val bal1 = s.balance(addr1)
    val bal2 = s.balance(addr2)
    val bal3 = s.balance(addr3) + bal1 + bal2


    val diff = blockchain.collectLposPortfolios {
      case (addr1, p) if bal1 != 0L =>
        Portfolio(bal1, LeaseBalance(leaseInBalances.getOrElse(addr1, 0L) ,0), Map.empty)

      case (addr2, p) if bal2 !=  0L =>
        Portfolio(bal2, LeaseBalance(leaseInBalances.getOrElse(addr2, 0L) ,0), Map.empty)

      case (addr3, p) if bal3 !=  0L =>
        Portfolio(bal3, LeaseBalance(leaseInBalances.getOrElse(addr3, 0L) ,0), Map.empty)
    }

    Diff.empty.copy(portfolios = diff)
  }
}

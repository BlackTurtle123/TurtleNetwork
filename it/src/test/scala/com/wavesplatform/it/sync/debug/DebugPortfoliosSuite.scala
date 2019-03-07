package com.wavesplatform.it.sync.debug

import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._

class DebugPortfoliosSuite extends BaseTransactionSuite {

  test("getting a balance considering pessimistic transactions from UTX pool - changed after UTX") {
    val portfolioBefore = sender.debugPortfoliosFor(firstAddress, considerUnspent = true)
    val utxSizeBefore   = sender.utxSize

    sender.transfer(firstAddress, secondAddress, 5.TN, 5.TN)
    sender.transfer(secondAddress, firstAddress, 7.TN, 5.TN)

    sender.waitForUtxIncreased(utxSizeBefore)

    val portfolioAfter = sender.debugPortfoliosFor(firstAddress, considerUnspent = true)

    val expectedBalance = portfolioBefore.balance - 10.TN // withdraw + fee
    assert(portfolioAfter.balance == expectedBalance)

  }

  test("getting a balance without pessimistic transactions from UTX pool - not changed after UTX") {
    nodes.waitForHeightArise()

    val portfolioBefore = sender.debugPortfoliosFor(firstAddress, considerUnspent = false)
    val utxSizeBefore   = sender.utxSize

    sender.transfer(firstAddress, secondAddress, 5.TN, fee = 5.TN)
    sender.waitForUtxIncreased(utxSizeBefore)

    val portfolioAfter = sender.debugPortfoliosFor(firstAddress, considerUnspent = false)
    assert(portfolioAfter.balance == portfolioBefore.balance)
  }
}

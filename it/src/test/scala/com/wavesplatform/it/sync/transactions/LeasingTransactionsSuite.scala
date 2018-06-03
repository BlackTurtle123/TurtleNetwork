package com.wavesplatform.it.sync.transactions

import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import org.scalatest.CancelAfterFailure
import play.api.libs.json.Json
import com.wavesplatform.it.sync._

class LeasingTransactionsSuite extends BaseTransactionSuite with CancelAfterFailure {

  test("leasing TN decreases lessor's eff.b. and increases lessee's eff.b.; lessor pays fee") {
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)

    val createdLeaseTxId = sender.lease(firstAddress, secondAddress, leasingAmount, leasingFee = fee).id
    nodes.waitForHeightAriseAndTxPresent(createdLeaseTxId)

    notMiner.assertBalances(firstAddress, balance1 - fee, eff1 - leasingAmount - fee)
    notMiner.assertBalances(secondAddress, balance2, eff2 + leasingAmount)

  }

  test("can not make leasing without having enough balance") {
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)

    //secondAddress effective balance more than general balance
    assertBadRequestAndResponse(sender.lease(secondAddress, firstAddress, balance2 + 1.TN, fee), "Reason: Cannot lease more than own")
    nodes.waitForHeightArise()

    notMiner.assertBalances(firstAddress, balance1, eff1)
    notMiner.assertBalances(secondAddress, balance2, eff2)
  }

  test("can not make leasing without having enough TN for fee") {
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)

    assertBadRequestAndResponse(sender.lease(firstAddress, secondAddress, balance1, fee), "Reason: Cannot lease more than own")
    nodes.waitForHeightArise()

    notMiner.assertBalances(firstAddress, balance1, eff1)
    notMiner.assertBalances(secondAddress, balance2, eff2)
  }

  test("lease cancellation reverts eff.b. changes; lessor pays fee for both lease and cancellation") {
    import scorex.transaction.lease.LeaseTransaction.Status._

    def getStatus(txId: String): String = {
      val r = sender.get(s"/transactions/info/$txId")
      (Json.parse(r.getResponseBody) \ "status").as[String]
    }

    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)

    val createdLeaseTxId = sender.lease(firstAddress, secondAddress, leasingAmount, fee).id
    nodes.waitForHeightAriseAndTxPresent(createdLeaseTxId)

    notMiner.assertBalances(firstAddress, balance1 - fee, eff1 - leasingAmount - fee)
    notMiner.assertBalances(secondAddress, balance2, eff2 + leasingAmount)

    val status1 = getStatus(createdLeaseTxId)
    status1 shouldBe Active

    val activeLeases = sender.activeLeases(secondAddress)
    assert(activeLeases.forall(!_.sender.contains(secondAddress)))

    val leases1 = sender.activeLeases(firstAddress)
    assert(leases1.exists(_.id == createdLeaseTxId))

    val createdCancelLeaseTxId = sender.cancelLease(firstAddress, createdLeaseTxId, fee).id
    nodes.waitForHeightAriseAndTxPresent(createdCancelLeaseTxId)

    notMiner.assertBalances(firstAddress, balance1 - 2 * fee, eff1 - 2 * fee)
    notMiner.assertBalances(secondAddress, balance2, eff2)

    val status2 = getStatus(createdLeaseTxId)
    status2 shouldBe Canceled

    val leases2 = sender.activeLeases(firstAddress)
    assert(leases2.forall(_.id != createdLeaseTxId))

    leases2.size shouldBe leases1.size - 1
  }

  test("lease cancellation can be done only once") {
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)

    val createdLeasingTxId = sender.lease(firstAddress, secondAddress, leasingAmount, fee).id
    nodes.waitForHeightAriseAndTxPresent(createdLeasingTxId)

    notMiner.assertBalances(firstAddress, balance1 - fee, eff1 - leasingAmount - fee)
    notMiner.assertBalances(secondAddress, balance2, eff2 + leasingAmount)

    val createdCancelLeaseTxId = sender.cancelLease(firstAddress, createdLeasingTxId, fee).id
    nodes.waitForHeightAriseAndTxPresent(createdCancelLeaseTxId)

    assertBadRequestAndResponse(sender.cancelLease(firstAddress, createdLeasingTxId, fee), "Reason: Cannot cancel already cancelled lease")

    notMiner.assertBalances(firstAddress, balance1 - 2 * fee, eff1 - 2 * fee)
    notMiner.assertBalances(secondAddress, balance2, eff2)
  }

  test("only sender can cancel lease transaction") {
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)

    val createdLeaseTxId = sender.lease(firstAddress, secondAddress, leasingAmount, leasingFee = fee).id
    nodes.waitForHeightAriseAndTxPresent(createdLeaseTxId)

    notMiner.assertBalances(firstAddress, balance1 - fee, eff1 - leasingAmount - fee)
    notMiner.assertBalances(secondAddress, balance2, eff2 + leasingAmount)

    assertBadRequestAndResponse(sender.cancelLease(thirdAddress, createdLeaseTxId, fee), "LeaseTransaction was leased by other sender")
  }

  test("can not make leasing without having enough your TN to self") {
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    assertBadRequestAndResponse(sender.lease(firstAddress, firstAddress, balance1 + 1.TN, fee), "Transaction to yourself")
    nodes.waitForHeightArise()

    notMiner.assertBalances(firstAddress, balance1, eff1)
  }
}

<<<<<<< HEAD

package com.wavesplatform.it.transactions

import com.wavesplatform.it.api._
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.util._
import org.scalatest.CancelAfterFailure
import play.api.libs.json.Json
import scorex.transaction.lease.LeaseTransaction

import scala.concurrent.Await
import scala.concurrent.Future.traverse
import scala.concurrent.duration._

class LeasingTransactionsSuite extends BaseTransactionSuite with CancelAfterFailure {

  private val waitCompletion = 2.minutes
  private val defaultFee     = 2.TN
  private val leasingAmount  = 5.TN

  test("leasing TN decreases lessor's eff.b. and increases lessee's eff.b.; lessor pays fee") {
    val f = for {
      height <- traverse(nodes)(_.height).map(_.max)
      _ <- traverse(nodes)(_.waitForHeight(height + 1))
      ((firstBalance, firstEffBalance), (secondBalance, secondEffBalance)) <- notMiner.accountBalances(firstAddress)
        .zip(notMiner.accountBalances(secondAddress))

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, leasingAmount, fee = defaultFee).map(_.id)
      _ <- nodes.waitForHeightAraiseAndTxPresent(createdLeaseTxId)
      _ <- notMiner.assertBalances(firstAddress, firstBalance - defaultFee, firstEffBalance - leasingAmount - defaultFee)
        .zip(notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance + leasingAmount))
    } yield succeed

    Await.result(f, waitCompletion)
  }

  test("can not make leasing without having enough TN") {
    val f = for {
      fb <- traverse(nodes)(_.height).map(_.min)

      ((firstBalance, firstEffBalance), (secondBalance, secondEffBalance)) <- notMiner.accountBalances(firstAddress)
        .zip(notMiner.accountBalances(secondAddress))

      //secondAddress effective balance more than general balance
      leaseFailureAssertion <- assertBadRequest(sender.lease(secondAddress, firstAddress, secondBalance + 1.TN, defaultFee))

      _ <- traverse(nodes)(_.waitForHeight(fb + 2))
      _ <- notMiner.assertBalances(firstAddress, firstBalance, firstEffBalance)
        .zip(notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance))
    } yield leaseFailureAssertion

    Await.result(f, waitCompletion)
  }

  test("can not make leasing without having enough TN for fee") {
    val f = for {
      fb <- traverse(nodes)(_.height).map(_.min)


      ((firstBalance, firstEffBalance), (secondBalance, secondEffBalance)) <- notMiner.accountBalances(firstAddress)
        .zip(notMiner.accountBalances(secondAddress))

      transferFailureAssertion <- assertBadRequest(sender.lease(firstAddress, secondAddress, firstEffBalance, fee = defaultFee))
      _ <- traverse(nodes)(_.waitForHeight(fb + 2))
      _ <- notMiner.assertBalances(firstAddress, firstBalance, firstEffBalance)
        .zip(notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance))
    } yield transferFailureAssertion

    Await.result(f, waitCompletion)
  }

  test("lease cancellation reverts eff.b. changes; lessor pays fee for both lease and cancellation") {
    import LeaseTransaction.Status._
    def status(txId: String) = sender.get(s"/transactions/info/$txId").map { r =>
      (Json.parse(r.getResponseBody) \ "status").as[String]
    }
    def activeLeases(address: String) = sender.get(s"/leasing/active/$address").as[Seq[Transaction]]

    val f = for {
      ((firstBalance, firstEffBalance), (secondBalance, secondEffBalance)) <- notMiner.accountBalances(firstAddress)
        .zip(notMiner.accountBalances(secondAddress))

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, leasingAmount, fee = defaultFee).map(_.id)
      _ <- nodes.waitForHeightAraiseAndTxPresent(createdLeaseTxId)
      _ <- notMiner.assertBalances(firstAddress, firstBalance - defaultFee, firstEffBalance - leasingAmount - defaultFee)
        .zip(notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance + leasingAmount))

      status1 <- status(createdLeaseTxId)
      _ = assert(status1 == Active)

      leases0 <- activeLeases(secondAddress)
      _ = assert(leases0.isEmpty)

      leases1 <- activeLeases(firstAddress)
      _ = assert(leases1.exists(_.id == createdLeaseTxId))

      createdCancelLeaseTxId <- sender.cancelLease(firstAddress, createdLeaseTxId, fee = defaultFee).map(_.id)

      _ <- nodes.waitForHeightAraiseAndTxPresent(createdCancelLeaseTxId)
      _ <- notMiner.assertBalances(firstAddress, firstBalance - 2 * defaultFee, firstEffBalance - 2 * defaultFee)
        .zip(notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance))

      status2 <- status(createdLeaseTxId)
      _ = assert(status2 == Canceled)

      leases2 <- activeLeases(firstAddress)
      _ = assert(leases2.forall(_.id != createdLeaseTxId))
      _ = assert(leases2.size == leases1.size - 1)
    } yield succeed

    Await.result(f, waitCompletion)
  }

  test("lease cancellation can be done only once") {
    val f = for {

      ((firstBalance, firstEffBalance), (secondBalance, secondEffBalance)) <- notMiner.accountBalances(firstAddress)
        .zip(notMiner.accountBalances(secondAddress))

      createdLeasingTxId <- sender.lease(firstAddress, secondAddress, leasingAmount, fee = defaultFee).map(_.id)
      _ <- nodes.waitForHeightAraiseAndTxPresent(createdLeasingTxId)
      _ <- notMiner.assertBalances(firstAddress, firstBalance - defaultFee, firstEffBalance - leasingAmount - defaultFee)
        .zip(notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance + leasingAmount))

      createdCancelLeaseTxId <- sender.cancelLease(firstAddress, createdLeasingTxId, fee = defaultFee).map(_.id)
      _ <- nodes.waitForHeightAraiseAndTxPresent(createdCancelLeaseTxId)
      _ <- assertBadRequest(sender.cancelLease(firstAddress, createdLeasingTxId, fee = defaultFee).map(_.id))
      _ <- notMiner.assertBalances(firstAddress, firstBalance - 2 * defaultFee, firstEffBalance - 2 * defaultFee)
        .zip(notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance))
    } yield succeed

    Await.result(f, waitCompletion)
  }

  test("only sender can cancel lease transaction") {
    val f = for {

      ((firstBalance, firstEffBalance), (secondBalance, secondEffBalance)) <- notMiner.accountBalances(firstAddress)
        .zip(notMiner.accountBalances(secondAddress))

      createdLeaseTxId <- sender.lease(firstAddress, secondAddress, leasingAmount, fee = defaultFee).map(_.id)

      height <- traverse(nodes)(_.height).map(_.max)

      _ <- traverse(nodes)(_.waitForHeight(height + 1))
      _ <- traverse(nodes)(_.waitForTransaction(createdLeaseTxId))

      _ <- notMiner.assertBalances(firstAddress, firstBalance - defaultFee, firstEffBalance - leasingAmount - defaultFee)
        .zip(notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance + leasingAmount))

      _ <- assertBadRequest(sender.cancelLease(thirdAddress, createdLeaseTxId, fee = defaultFee))
    } yield succeed

    Await.result(f, waitCompletion)
  }

  test("can not make leasing without having enough your TN to self") {
    val f = for {
      fb <- traverse(nodes)(_.height).map(_.min)

      (firstBalance, firstEffBalance) <- notMiner.accountBalances(firstAddress)

      transferFailureAssertion <- assertBadRequest(sender.lease(firstAddress, firstAddress, firstBalance + 1.TN, fee = defaultFee))
      _ <- traverse(nodes)(_.waitForHeight(fb + 2))
      _ <- notMiner.assertBalances(firstAddress, firstBalance, firstEffBalance)
    } yield transferFailureAssertion

    Await.result(f, waitCompletion)
  }

=======
package com.wavesplatform.it.sync.transactions

import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import org.scalatest.CancelAfterFailure
import play.api.libs.json.Json
import com.wavesplatform.it.sync._

class LeasingTransactionsSuite extends BaseTransactionSuite with CancelAfterFailure {
  private val errorMessage = "Reason: Cannot lease more than own"

  test("leasing waves decreases lessor's eff.b. and increases lessee's eff.b.; lessor pays fee") {
    for (v <- supportedVersions) {
      val (balance1, eff1) = miner.accountBalances(firstAddress)
      val (balance2, eff2) = miner.accountBalances(secondAddress)

      val createdLeaseTxId = sender.lease(firstAddress, secondAddress, leasingAmount, leasingFee = minFee, version = v).id
      nodes.waitForHeightAriseAndTxPresent(createdLeaseTxId)

      miner.assertBalances(firstAddress, balance1 - minFee, eff1 - leasingAmount - minFee)
      miner.assertBalances(secondAddress, balance2, eff2 + leasingAmount)
    }
  }

  test("cannot lease non-own waves") {
    for (v <- supportedVersions) {
      val createdLeaseTxId = sender.lease(firstAddress, secondAddress, leasingAmount, leasingFee = minFee, version = v).id
      nodes.waitForHeightAriseAndTxPresent(createdLeaseTxId)

      val eff2 = miner.accountBalances(secondAddress)._2

      assertBadRequestAndResponse(sender.lease(secondAddress, thirdAddress, eff2 - minFee, leasingFee = minFee, version = v), errorMessage)
    }
  }

  test("can not make leasing without having enough balance") {
    for (v <- supportedVersions) {
      val (balance1, eff1) = miner.accountBalances(firstAddress)
      val (balance2, eff2) = miner.accountBalances(secondAddress)

      //secondAddress effective balance more than general balance
      assertBadRequestAndResponse(sender.lease(secondAddress, firstAddress, balance2 + 1.TN, minFee, version = v), errorMessage)
      nodes.waitForHeightArise()

      assertBadRequestAndResponse(sender.lease(firstAddress, secondAddress, balance1, minFee, version = v), errorMessage)
      nodes.waitForHeightArise()
      assertBadRequestAndResponse(sender.lease(firstAddress, secondAddress, balance1 - minFee / 2, minFee, version = v), errorMessage)
      nodes.waitForHeightArise()

      val newAddress = sender.createAddress()
      assertBadRequestAndResponse(sender.lease(newAddress, secondAddress, minFee, minFee, version = v), errorMessage)
      nodes.waitForHeightArise()

      miner.assertBalances(firstAddress, balance1, eff1)
      miner.assertBalances(secondAddress, balance2, eff2)
    }
  }

  test("lease cancellation reverts eff.b. changes; lessor pays fee for both lease and cancellation") {
    import com.wavesplatform.transaction.lease.LeaseTransaction.Status._

    def getStatus(txId: String): String = {
      val r = sender.get(s"/transactions/info/$txId")
      (Json.parse(r.getResponseBody) \ "status").as[String]
    }

    for (v <- supportedVersions) {
      val (balance1, eff1) = miner.accountBalances(firstAddress)
      val (balance2, eff2) = miner.accountBalances(secondAddress)

      val createdLeaseTxId = sender.lease(firstAddress, secondAddress, leasingAmount, minFee, version = v).id
      nodes.waitForHeightAriseAndTxPresent(createdLeaseTxId)

      miner.assertBalances(firstAddress, balance1 - minFee, eff1 - leasingAmount - minFee)
      miner.assertBalances(secondAddress, balance2, eff2 + leasingAmount)

      val status1 = getStatus(createdLeaseTxId)
      status1 shouldBe Active

      val activeLeases = sender.activeLeases(secondAddress)
      assert(activeLeases.forall(!_.sender.contains(secondAddress)))

      val leases1 = sender.activeLeases(firstAddress)
      assert(leases1.exists(_.id == createdLeaseTxId))

      val createdCancelLeaseTxId = sender.cancelLease(firstAddress, createdLeaseTxId, minFee).id
      nodes.waitForHeightAriseAndTxPresent(createdCancelLeaseTxId)

      miner.assertBalances(firstAddress, balance1 - 2 * minFee, eff1 - 2 * minFee)
      miner.assertBalances(secondAddress, balance2, eff2)

      val status2 = getStatus(createdLeaseTxId)
      status2 shouldBe Canceled

      val leases2 = sender.activeLeases(firstAddress)
      assert(leases2.forall(_.id != createdLeaseTxId))

      leases2.size shouldBe leases1.size - 1
    }
  }

  test("lease cancellation can be done only once") {
    for (v <- supportedVersions) {
      val (balance1, eff1) = miner.accountBalances(firstAddress)
      val (balance2, eff2) = miner.accountBalances(secondAddress)

      val createdLeasingTxId = sender.lease(firstAddress, secondAddress, leasingAmount, minFee, version = v).id
      nodes.waitForHeightAriseAndTxPresent(createdLeasingTxId)

      miner.assertBalances(firstAddress, balance1 - minFee, eff1 - leasingAmount - minFee)
      miner.assertBalances(secondAddress, balance2, eff2 + leasingAmount)

      val createdCancelLeaseTxId = sender.cancelLease(firstAddress, createdLeasingTxId, minFee).id
      nodes.waitForHeightAriseAndTxPresent(createdCancelLeaseTxId)

      assertBadRequestAndResponse(sender.cancelLease(firstAddress, createdLeasingTxId, minFee), "Reason: Cannot cancel already cancelled lease")

      miner.assertBalances(firstAddress, balance1 - 2 * minFee, eff1 - 2 * minFee)
      miner.assertBalances(secondAddress, balance2, eff2)
    }
  }

  test("only sender can cancel lease transaction") {
    for (v <- supportedVersions) {
      val (balance1, eff1) = miner.accountBalances(firstAddress)
      val (balance2, eff2) = miner.accountBalances(secondAddress)

      val createdLeaseTxId = sender.lease(firstAddress, secondAddress, leasingAmount, leasingFee = minFee, version = v).id
      nodes.waitForHeightAriseAndTxPresent(createdLeaseTxId)

      miner.assertBalances(firstAddress, balance1 - minFee, eff1 - leasingAmount - minFee)
      miner.assertBalances(secondAddress, balance2, eff2 + leasingAmount)

      assertBadRequestAndResponse(sender.cancelLease(thirdAddress, createdLeaseTxId, minFee), "LeaseTransaction was leased by other sender")
    }
  }

  test("can not make leasing to yourself") {
    for (v <- supportedVersions) {
<<<<<<<< HEAD:it/src/test/scala/com/wavesplatform/it/sync/transactions/LeasingTransactionsSuite.scala
      val (balance1, eff1) = notMiner.accountBalances(firstAddress)
      assertBadRequestAndResponse(sender.lease(firstAddress, firstAddress, balance1 + 1.TN, minFee, v), "Transaction to yourself")
========
      val (balance1, eff1) = miner.accountBalances(firstAddress)
      assertBadRequestAndResponse(sender.lease(firstAddress, firstAddress, balance1 + 1.waves, minFee, v), "Transaction to yourself")
>>>>>>>> d1f0230aa355768b58cb3d04cd79b50022bfdc90:node-it/src/test/scala/com/wavesplatform/it/sync/transactions/LeasingTransactionsSuite.scala
      nodes.waitForHeightArise()

      miner.assertBalances(firstAddress, balance1, eff1)
    }
  }
>>>>>>> d1f0230aa355768b58cb3d04cd79b50022bfdc90
}

package com.wavesplatform.transaction.smart.script.v1

import com.wavesplatform.crypto
import com.wavesplatform.lang.Version._
import com.wavesplatform.lang.contract.{Contract, ContractSerDe}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.{FunctionHeader, ScriptEstimator, Serde}
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.smart.script.{Script, ScriptCompiler}
import com.wavesplatform.crypto
import com.wavesplatform.utils.{functionCosts, varNames}
import monix.eval.Coeval
import com.wavesplatform.state._

object ScriptV1 {
  val checksumLength         = 4
  private val maxComplexity  = 20 * functionCosts(V1)(FunctionHeader.Native(SIGVERIFY))()
  private val maxSizeInBytes = 8 * 1024

  def validateBytes(bs: Array[Byte]): Either[String, Unit] =
    Either.cond(bs.length <= maxSizeInBytes, (), s"Script is too large: ${bs.length} bytes > $maxSizeInBytes bytes")

  def apply(x: EXPR): Either[String, Script] = apply(V1, x)

  def apply(version: Version, x: EXPR, checkSize: Boolean = true): Either[String, Script] =
    for {
      scriptComplexity <- ScriptEstimator(varNames(version), functionCosts(version), x)
      _                <- Either.cond(scriptComplexity <= maxComplexity, (), s"Script is too complex: $scriptComplexity > $maxComplexity")
      s = new ScriptV1Impl(version, x)
      _ <- if (checkSize) validateBytes(s.bytes().arr) else Right(())
    } yield s

  private class ScriptV1[V <: ScriptVersion](override val version: V, override val expr: EXPR) extends Script { self =>
    override type Ver = V
    override val text: String             = expr.toString
    override val complexity: Coeval[Long] = Coeval.evalOnce(ScriptCompiler.estimate(self, V1).explicitGet())
    override val bytes: Coeval[ByteStr] =
      Coeval.evalOnce {
        val s = Array(version.toByte) ++ Serde.serialize(expr)
        ByteStr(s ++ crypto.secureHash(s).take(checksumLength))
      }
  }
}

case class ScriptV2(override val version: Version, override val expr: Contract) extends Script {
  override type Expr = Contract
  override val text: String = expr.toString
  override val bytes: Coeval[ByteStr] =
    Coeval.evalOnce {
      val s = Array(version.toByte) ++ ContractSerDe.serialize(expr)
      ByteStr(s ++ crypto.secureHash(s).take(checksumLength))
    }
}

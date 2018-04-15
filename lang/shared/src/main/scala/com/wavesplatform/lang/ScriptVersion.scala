package com.wavesplatform.lang

import com.wavesplatform.lang.v1.Terms
import com.wavesplatform.lang.v1.ctx.Context

sealed trait ScriptVersion { self =>
  type ExprT
  type CtxT
  val value: Int
}

object ScriptVersion {

  private val versions: Map[Int, ScriptVersion] = Map(1 -> Versions.V1)

  def fromInt(i: Int): Option[ScriptVersion] = versions.get(i)

  object Versions {
    object V1 extends ScriptVersion { self =>
      override type ExprT = Terms.Typed.EXPR
      override type CtxT = Context
      override val value: Int = 1
    }
  }
}

package com.wavesplatform.lang

import cats.kernel.Monoid
import com.wavesplatform.lang.Common.multiplierFunction
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.{CaseType, NativeFunction}

package object compiler {

  val pointType = CaseType("Point", List("x" -> LONG, "y" -> LONG))

  val idT = NativeFunction("idT", 1, 10000, TYPEPARAM('T'), "p1" -> TYPEPARAM('T'))(Right(_))
  val extract = NativeFunction("extract", 1, EXTRACT, TYPEPARAM('T'), "p1" -> OPTIONTYPEPARAM(TYPEPARAM('T'))) {
    case Some(vl) :: Nil => Right(vl)
    case _               => Left("extracting from empty option")
  }
  val undefinedOptionLong = NativeFunction("undefinedOptionLong", 1, 10001, OPTION(LONG), List.empty: _*)(_ => ???)
  val idOptionLong        = NativeFunction("idOptionLong", 1, 10002, UNIT, "opt" -> OPTION(OPTION(LONG)))(_ => Right(()))
  val unitOnNone          = NativeFunction("unitOnNone", 1, 10003, UNIT, "opt" -> OPTION(NOTHING))(_ => Right(()))
  val functionWithTwoPrarmsOfTheSameType =
    NativeFunction("functionWithTwoPrarmsOfTheSameType", 1, 10004, TYPEPARAM('T'), "p1" -> TYPEPARAM('T'), "p2" -> TYPEPARAM('T'))(Right(_))

  val compilerContext = Monoid
    .combine(
      PureContext.ctx,
      CTX(
        Seq(pointType, Common.pointTypeA, Common.pointTypeB),
        Map(("p", (Common.AorB, null))),
        Seq(multiplierFunction, functionWithTwoPrarmsOfTheSameType, idT, unitOnNone, undefinedOptionLong, idOptionLong)
      )
    )
    .compilerContext

}

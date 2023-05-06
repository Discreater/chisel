// SPDX-License-Identifier: Apache-2.0

// This file transform macro definitions to explicitly add implicit source info to Chisel method
// calls.

package chisel3.internal.sourceinfo

// TODO warn if...

// // Workaround for https://github.com/sbt/sbt/issues/3966
// object IntLiteralApplyTransform

// class IntLiteralApplyTransform(val c: Context) extends AutoSourceTransform {
//   import c.universe._

//   def safeApply(x: c.Tree): c.Tree = {
//     c.macroApplication match {
//       case q"$_.$clazz($lit).$func.apply($arg)" =>
//         if (
//           Set("U", "S").contains(func.toString) &&
//           Set("fromStringToLiteral", "fromIntToLiteral", "fromLongToIteral", "fromBigIntToLiteral").contains(
//             clazz.toString
//           )
//         ) {
//           val msg =
//             s"""Passing an Int to .$func is usually a mistake: It does *not* set the width but does a bit extract.
//                |Did you mean .$func($arg.W)?
//                |If you do want bit extraction, use .$func.extract($arg) instead.
//                |""".stripMargin
//           c.warning(c.enclosingPosition, msg)
//         }
//       case _ => // do nothing
//     }
//     q"$thisObj.$doFuncTerm($x)($implicitSourceInfo)"
//   }
// }

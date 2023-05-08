// SPDX-License-Identifier: Apache-2.0

// This file transform macro definitions to explicitly add implicit source info to Chisel method
// calls.

package chisel3.internal.sourceinfo

// import scala.language.experimental.macros
// import scala.reflect.macros.blackbox.Context
// import scala.reflect.macros.whitebox

// /** Transforms a function call so that it can both provide implicit-style source information and
//   * have a chained apply call. Without macros, only one is possible, since having a implicit
//   * argument in the definition will cause the compiler to interpret a chained apply as an
//   * explicit implicit argument and give type errors.
//   *
//   * Instead of an implicit argument, the public-facing function no longer takes a SourceInfo at all.
//   * The macro transforms the public-facing function into a call to an internal function that takes
//   * an explicit SourceInfo by inserting an implicitly[SourceInfo] as the explicit argument.
//   */
// trait SourceInfoTransformMacro {
//   val c: Context
//   import c.universe._
//   def thisObj: Tree = c.prefix.tree
//   def implicitSourceInfo = q"implicitly[_root_.chisel3.experimental.SourceInfo]"
// }

// /** "Automatic" source information transform / insertion macros, which generate the function name
//   * based on the macro invocation (instead of explicitly writing out every transform).
//   */
// abstract class AutoSourceTransform extends SourceInfoTransformMacro {
//   import c.universe._

//   /** Returns the TermName of the transformed function, which is the applied function name with do_
//     * prepended.
//     */
//   def doFuncTerm: TermName = {
//     val funcName = c.macroApplication match {
//       case q"$_.$funcName[..$_](...$_)" => funcName
//       case _ =>
//         throw new Exception(
//           s"Chisel Internal Error: Could not resolve function name from macro application: ${showCode(c.macroApplication)}"
//         )
//     }
//     TermName("do_" + funcName)
//   }
// }

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

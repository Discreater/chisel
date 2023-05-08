// SPDX-License-Identifier: Apache-2.0

// This file transform macro definitions to explicitly add implicit source info to Chisel method
// calls.

package chisel3.internal.sourceinfo

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.whitebox

import chisel3.{UInt, Bool, Bits}
import chisel3.experimental.SourceInfo

import scala.quoted.*
/** Transforms a function call so that it can both provide implicit-style source information and
  * have a chained apply call. Without macros, only one is possible, since having a implicit
  * argument in the definition will cause the compiler to interpret a chained apply as an
  * explicit implicit argument and give type errors.
  *
  * Instead of an implicit argument, the public-facing function no longer takes a SourceInfo at all.
  * The macro transforms the public-facing function into a call to an internal function that takes
  * an explicit SourceInfo by inserting an implicitly[SourceInfo] as the explicit argument.
  */
trait SourceInfoTransformMacro(using q: Quotes) {
  import q.reflect.*
  def thisObj: Expr[Any]
  inline def implicitSourceInfo: Expr[SourceInfo] = {
    Implicits.search(TypeRepr.of[SourceInfo]) match {
      case success: ImplicitSearchSuccess => success.tree.asExprOf[SourceInfo]
      case _ => report.errorAndAbort("Unable to find implicit SourceInfo")
    }
  }
}
def implicitSourceInfo(using q: Quotes): Expr[SourceInfo] = {
  import q.reflect.*
  Expr.summon[SourceInfo] match {
    case Some(sourceInfo) => sourceInfo
    case None => report.errorAndAbort("Unable to find implicit SourceInfo")
  }
}

def applySourceInfo[T](using q: Quotes)(fun: q.reflect.Term): Expr[T] = {
  import q.reflect.* 
  Implicits.search(TypeRepr.of[SourceInfo]) match {
    case success: ImplicitSearchSuccess => {
      val sourceInfo = success.tree.asExprOf[SourceInfo]
      Apply(fun, List(sourceInfo.asTerm)).asExprOf[T]
    }
    case _ => report.errorAndAbort("Unable to find implicit SourceInfo")
  }
}

def resolveThis(using Quotes): quotes.reflect.Term = {
  import quotes.reflect.* 
  var sym = Symbol.spliceOwner.owner
  if sym.isClassDef then {
    return This(sym)
  }
  if sym.isPackageDef then {
    return Ident(sym.termRef)
  }
  report.errorAndAbort(s"unknown owner: ${sym.fullName}")
}

object UIntTransform {
  def bitset(off: Expr[UInt], dat: Expr[Bool])(using q: Quotes): Expr[UInt] = {
    import q.reflect.*
    val sourceInfo = implicitSourceInfo
    applySourceInfo(Apply(Select.unique(resolveThis, "do_bitSet"), List(off.asTerm, dat.asTerm)))
  }
}

object ProbeTransform {
  def sourceApply[T](source: Expr[T])(using q: Quotes, t: Type[T]): Expr[T] = {
    import q.reflect.* 
    val sourceInfo = implicitSourceInfo
  
    Apply(Apply(TypeApply(Select.unique(resolveThis, "do_apply"), List(TypeTree.of[T])), List(source.asTerm)), List(sourceInfo.asTerm)).asExprOf[T]
  }

  def sourceRead[T](source: Expr[T])(using q: Quotes, t: Type[T]): Expr[T] = {
    import q.reflect.* 
    val sourceInfo = implicitSourceInfo
    Apply(Apply(TypeApply(Select.unique(resolveThis, "do_read"), List(TypeTree.of[T])), List(source.asTerm)), List(sourceInfo.asTerm)).asExprOf[T]
  }
}

// Module instantiation transform

object InstTransform {
  // TODO: check whether we can use `T` directly, since the `bc` type in `apply` is `=> T`
  def apply[T <: chisel3.experimental.BaseModule](bc: Expr[T])(using q: Quotes, tpe: Type[T]) = {
    import q.reflect.*
    val sourceInfo = implicitSourceInfo
    Apply(Apply(TypeApply(Select.unique())))
    '{ ${self}.do_apply(${bc})(using $sourceInfo) }
  }
}


// Module instantiation transform

object DefinitionTransform {
  import chisel3.experimental.BaseModule
  import chisel3.experimental.hierarchy.core.IsInstantiable
  import chisel3.experimental.hierarchy.core.Definition
  // TODO: check whether we can use `T` directly, since the `bc` type in `apply` is `=> T` 
  def apply[T <: BaseModule with IsInstantiable](proto: Expr[T])(self: Expr[Definition.type])(using q: Quotes): Expr[Definition[T]] = {
    import q.reflect.* 
    val sourceInfo = implicitSourceInfo
    '{ ${self}.do_apply(${proto})(using $sourceInfo) }
  }
}

// Module instantiation transform
// object DefinitionWrapTransform {
//     def wrap[T <: chisel3.experimental.BaseModule with chisel3.experimental.hierarchy.core.IsInstantiable](proto: Expr[T], self: Expr[chisel3.experimental.hierarchy.core.Definition.type])(using q: Quotes): Expr[chisel3.experimental.hierarchy.core.Definition[T]] = {
//     import q.reflect.* 
//     val sourceInfo = implicitSourceInfo
//     '{ ${self}.do_wrap(${proto})($sourceInfo) }
//   }
// }

// Module instantiation transform
object InstanceTransform {
  import chisel3.experimental.BaseModule
  import chisel3.experimental.hierarchy.core.{Definition, IsInstantiable, Instance}
  def apply[T <: BaseModule with IsInstantiable](definition: Expr[Definition[T]])(self: Expr[Instance.type])(using q: Quotes): Expr[Instance[T]] = {
    import q.reflect.* 
    val sourceInfo = implicitSourceInfo
    '{ ${self}.do_apply(${definition})(using $sourceInfo) }
  }
}

object MemTransform {
  import chisel3.{Data}
  def apply[T <: Data, M[T]](size: Expr[Any], t: Expr[T])(self: Expr[Any])(using q: Quotes): Expr[M[T]] = {
    import q.reflect.*
    val sourceInfo = implicitSourceInfo
    // Implicits.search(TypeRepr.of[SourceInfo]) match {
    //   case suc: ImplicitSearchSuccess => suc.tree.asExprOf
    // }
    Apply(Apply(Select.unique(self.asTerm, "do_apply"), List(size.asTerm, t.asTerm)), List(sourceInfo.asTerm)).asExprOf[M[T]]
  }

  def apply_ruw[T <: Data, M[T]](size: Expr[Any], t: Expr[T], ruw: Expr[Any])(self: Expr[Any])(using q: Quotes): Expr[M[T]] = {
    import q.reflect.*
    val sourceInfo = implicitSourceInfo
    Apply(Apply(Select.unique(self.asTerm, "do_apply"), List(size.asTerm, t.asTerm, ruw.asTerm)), List(sourceInfo.asTerm)).asExprOf[M[T]]
  }
}

object MuxTransform {
  import chisel3.{Data, Bool}
  def apply[T <: Data](cond: Expr[Bool], con: Expr[T], alt: Expr[T])(self: Expr[Any])(using q: Quotes): Expr[T] = {
    import q.reflect.*
    val sourceInfo = implicitSourceInfoTerm
    Apply(Apply(Select.unique(self.asTerm, "do_apply"), List(cond.asTerm, con.asTerm, alt.asTerm)), List(sourceInfo)).asExprOf[T]
  } 
}

object MuxLookupTransform {
  import chisel3.{Data, UInt}
  def applyCurried[T <: Data](key: Expr[UInt], default: Expr[T])(mapping: Expr[Seq[(UInt, T)]])(self: Expr[Any])(using q: Quotes): Expr[T] = {
    import q.reflect.* 
    Apply(Apply(Select.unique(self.asTerm, "do_apply"), List(key.asTerm, default.asTerm, mapping.asTerm)), List(implicitSourceInfoTerm))
  } 
}

class MuxLookupTransform(val c: Context) extends SourceInfoTransformMacro {
  import c.universe._

  def applyCurried[S: c.WeakTypeTag, T: c.WeakTypeTag](key: c.Tree, default: c.Tree)(mapping: c.Tree): c.Tree = {
    val sType = weakTypeOf[S]
    val tType = weakTypeOf[T]
    q"$thisObj.do_apply[$sType, $tType]($key, $default, $mapping)($implicitSourceInfo)"
  }

  def applyEnum[S: c.WeakTypeTag, T: c.WeakTypeTag](key: c.Tree, default: c.Tree)(mapping: c.Tree): c.Tree = {
    val sType = weakTypeOf[S]
    val tType = weakTypeOf[T]
    q"$thisObj.do_applyEnum[$sType, $tType]($key, $default, $mapping)($implicitSourceInfo)"
  }
}

// Workaround for https://github.com/sbt/sbt/issues/3966
object VecTransform
class VecTransform(val c: Context) extends SourceInfoTransformMacro {
  import c.universe._
  def apply_elts(elts: c.Tree): c.Tree = {
    q"$thisObj.do_apply($elts)($implicitSourceInfo)"
  }
  def apply_elt0(elt0: c.Tree, elts: c.Tree*): c.Tree = {
    q"$thisObj.do_apply($elt0, ..$elts)($implicitSourceInfo)"
  }
  def tabulate(n: c.Tree)(gen: c.Tree): c.Tree = {
    q"$thisObj.do_tabulate($n)($gen)($implicitSourceInfo)"
  }
  def tabulate2D(n: c.Tree, m: c.Tree)(gen: c.Tree): c.Tree = {
    q"$thisObj.do_tabulate($n,$m)($gen)($implicitSourceInfo)"
  }
  def tabulate3D(n: c.Tree, m: c.Tree, p: c.Tree)(gen: c.Tree): c.Tree = {
    q"$thisObj.do_tabulate($n,$m,$p)($gen)($implicitSourceInfo)"
  }
  def fill(n: c.Tree)(gen: c.Tree): c.Tree = {
    q"$thisObj.do_fill($n)($gen)($implicitSourceInfo)"
  }
  def fill2D(n: c.Tree, m: c.Tree)(gen: c.Tree): c.Tree = {
    q"$thisObj.do_fill($n,$m)($gen)($implicitSourceInfo)"
  }
  def fill3D(n: c.Tree, m: c.Tree, p: c.Tree)(gen: c.Tree): c.Tree = {
    q"$thisObj.do_fill($n,$m,$p)($gen)($implicitSourceInfo)"
  }
  def fill4D(n: c.Tree, m: c.Tree, p: c.Tree, q: c.Tree)(gen: c.Tree): c.Tree = {
    q"$thisObj.do_fill($n,$m,$p,$q)($gen)($implicitSourceInfo)"
  }
  def iterate(start: c.Tree, len: c.Tree)(f: c.Tree): c.Tree = {
    q"$thisObj.do_iterate($start,$len)($f)($implicitSourceInfo)"
  }
  def contains(x: c.Tree)(ev: c.Tree): c.Tree = {
    q"$thisObj.do_contains($x)($implicitSourceInfo, $ev)"
  }
  def reduceTree(redOp: c.Tree, layerOp: c.Tree): c.Tree = {
    q"$thisObj.do_reduceTree($redOp,$layerOp)($implicitSourceInfo)"
  }
  def reduceTreeDefault(redOp: c.Tree): c.Tree = {
    q"$thisObj.do_reduceTree($redOp)($implicitSourceInfo)"
  }
}

/** "Automatic" source information transform / insertion macros, which generate the function name
  * based on the macro invocation (instead of explicitly writing out every transform).
  */
abstract class AutoSourceTransform extends SourceInfoTransformMacro {
  import c.universe._

  /** Returns the TermName of the transformed function, which is the applied function name with do_
    * prepended.
    */
  def doFuncTerm: TermName = {
    val funcName = c.macroApplication match {
      case q"$_.$funcName[..$_](...$_)" => funcName
      case _ =>
        throw new Exception(
          s"Chisel Internal Error: Could not resolve function name from macro application: ${showCode(c.macroApplication)}"
        )
    }
    TermName("do_" + funcName)
  }
}

// Workaround for https://github.com/sbt/sbt/issues/3966
object SourceInfoTransform
class SourceInfoTransform(val c: Context) extends AutoSourceTransform {
  import c.universe._

  def noArg: c.Tree = {
    q"$thisObj.$doFuncTerm($implicitSourceInfo)"
  }

  /** Necessary for dummy methods to auto-apply their arguments to this macro */
  def noArgDummy(dummy: c.Tree*): c.Tree = {
    q"$thisObj.$doFuncTerm($implicitSourceInfo)"
  }

  def thatArg(that: c.Tree): c.Tree = {
    q"$thisObj.$doFuncTerm($that)($implicitSourceInfo)"
  }

  def nArg(n: c.Tree): c.Tree = {
    q"$thisObj.$doFuncTerm($n)($implicitSourceInfo)"
  }

  def pArg(p: c.Tree): c.Tree = {
    q"$thisObj.$doFuncTerm($p)($implicitSourceInfo)"
  }

  def inArg(in: c.Tree): c.Tree = {
    q"$thisObj.$doFuncTerm($in)($implicitSourceInfo)"
  }

  def xArg(x: c.Tree): c.Tree = {
    q"$thisObj.$doFuncTerm($x)($implicitSourceInfo)"
  }

  def xyArg(x: c.Tree, y: c.Tree): c.Tree = {
    q"$thisObj.$doFuncTerm($x, $y)($implicitSourceInfo)"
  }

  def nxArg(n: c.Tree, x: c.Tree): c.Tree = {
    q"$thisObj.$doFuncTerm($n, $x)($implicitSourceInfo)"
  }

  def idxDataArg(idx: c.Tree, data: c.Tree): c.Tree = {
    q"$thisObj.$doFuncTerm($idx, $data)($implicitSourceInfo)"
  }

  def idxDataClockArg(idx: c.Tree, data: c.Tree, clock: c.Tree): c.Tree = {
    q"$thisObj.$doFuncTerm($idx, $data, $clock)($implicitSourceInfo)"
  }

  def idxEnClockArg(idx: c.Tree, en: c.Tree, clock: c.Tree): c.Tree = {
    q"$thisObj.$doFuncTerm($idx, $en, $clock)($implicitSourceInfo)"
  }

  def idxDataEnIswArg(idx: c.Tree, writeData: c.Tree, en: c.Tree, isWrite: c.Tree): c.Tree = {
    q"$thisObj.$doFuncTerm($idx, $writeData, $en, $isWrite)($implicitSourceInfo)"
  }

  def idxDataMaskArg(idx: c.Tree, writeData: c.Tree, mask: c.Tree)(evidence: c.Tree): c.Tree = {
    q"$thisObj.$doFuncTerm($idx, $writeData, $mask)($evidence, $implicitSourceInfo)"
  }

  def idxDataMaskClockArg(idx: c.Tree, writeData: c.Tree, mask: c.Tree, clock: c.Tree)(evidence: c.Tree): c.Tree = {
    q"$thisObj.$doFuncTerm($idx, $writeData, $mask, $clock)($evidence, $implicitSourceInfo)"
  }

  def idxDataEnIswClockArg(idx: c.Tree, writeData: c.Tree, en: c.Tree, isWrite: c.Tree, clock: c.Tree): c.Tree = {
    q"$thisObj.$doFuncTerm($idx, $writeData, $en, $isWrite, $clock)($implicitSourceInfo)"
  }

  def idxDataMaskEnIswArg(
    idx:       c.Tree,
    writeData: c.Tree,
    mask:      c.Tree,
    en:        c.Tree,
    isWrite:   c.Tree
  )(evidence:  c.Tree
  ): c.Tree = {
    q"$thisObj.$doFuncTerm($idx, $writeData, $mask, $en, $isWrite)($evidence, $implicitSourceInfo)"
  }

  def idxDataMaskEnIswClockArg(
    idx:       c.Tree,
    writeData: c.Tree,
    mask:      c.Tree,
    en:        c.Tree,
    isWrite:   c.Tree,
    clock:     c.Tree
  )(evidence:  c.Tree
  ): c.Tree = {
    q"$thisObj.$doFuncTerm($idx, $writeData, $mask, $en, $isWrite, $clock)($evidence, $implicitSourceInfo)"
  }

  def xEnArg(x: c.Tree, en: c.Tree): c.Tree = {
    q"$thisObj.$doFuncTerm($x, $en)($implicitSourceInfo)"
  }

  def arArg(a: c.Tree, r: c.Tree*): c.Tree = {
    q"$thisObj.$doFuncTerm($a, ..$r)($implicitSourceInfo)"
  }

  def rArg(r: c.Tree): c.Tree = {
    q"$thisObj.$doFuncTerm($r)($implicitSourceInfo)"
  }

  def nInArg(n: c.Tree, in: c.Tree): c.Tree = {
    q"$thisObj.$doFuncTerm($n, $in)($implicitSourceInfo)"
  }

  def nextEnableArg(next: c.Tree, enable: c.Tree): c.Tree = {
    q"$thisObj.$doFuncTerm($next, $enable)($implicitSourceInfo)"
  }

  def nextInitEnableArg(next: c.Tree, init: c.Tree, enable: c.Tree): c.Tree = {
    q"$thisObj.$doFuncTerm($next, $init, $enable)($implicitSourceInfo)"
  }

  def inNArg(in: c.Tree, n: c.Tree): c.Tree = {
    q"$thisObj.$doFuncTerm($in, $n)($implicitSourceInfo)"
  }

  def inNEnArg(in: c.Tree, n: c.Tree, en: c.Tree): c.Tree = {
    q"$thisObj.$doFuncTerm($in, $n, $en)($implicitSourceInfo)"
  }

  def inNResetEnArg(in: c.Tree, n: c.Tree, reset: c.Tree, en: c.Tree): c.Tree = {
    q"$thisObj.$doFuncTerm($in, $n, $reset, $en)($implicitSourceInfo)"
  }

  def inNResetDataArg(in: c.Tree, n: c.Tree, resetData: c.Tree): c.Tree = {
    q"$thisObj.$doFuncTerm($in, $n, $resetData)($implicitSourceInfo)"
  }

  def inNResetDataEnArg(in: c.Tree, n: c.Tree, resetData: c.Tree, en: c.Tree): c.Tree = {
    q"$thisObj.$doFuncTerm($in, $n, $resetData, $en)($implicitSourceInfo)"
  }
}

// Workaround for https://github.com/sbt/sbt/issues/3966
object SourceInfoWhiteboxTransform

/** Special whitebox version of the blackbox SourceInfoTransform, used when fun things need to
  * happen to satisfy the type system while preventing the use of macro overrides.
  */
class SourceInfoWhiteboxTransform(val c: whitebox.Context) extends AutoSourceTransform {
  import c.universe._

  def noArg: c.Tree = {
    q"$thisObj.$doFuncTerm($implicitSourceInfo)"
  }

  /** Necessary for dummy methods to auto-apply their arguments to this macro */
  def noArgDummy(dummy: c.Tree*): c.Tree = {
    q"$thisObj.$doFuncTerm($implicitSourceInfo)"
  }

  def thatArg(that: c.Tree): c.Tree = {
    q"$thisObj.$doFuncTerm($that)($implicitSourceInfo)"
  }
}

// Workaround for https://github.com/sbt/sbt/issues/3966
object IntLiteralApplyTransform

class IntLiteralApplyTransform(val c: Context) extends AutoSourceTransform {
  import c.universe._

  def safeApply(x: c.Tree): c.Tree = {
    c.macroApplication match {
      case q"$_.$clazz($lit).$func.apply($arg)" =>
        if (
          Set("U", "S").contains(func.toString) &&
          Set("fromStringToLiteral", "fromIntToLiteral", "fromLongToIteral", "fromBigIntToLiteral").contains(
            clazz.toString
          )
        ) {
          val msg =
            s"""Passing an Int to .$func is usually a mistake: It does *not* set the width but does a bit extract.
               |Did you mean .$func($arg.W)?
               |If you do want bit extraction, use .$func.extract($arg) instead.
               |""".stripMargin
          c.warning(c.enclosingPosition, msg)
        }
      case _ => // do nothing
    }
    q"$thisObj.$doFuncTerm($x)($implicitSourceInfo)"
  }
}

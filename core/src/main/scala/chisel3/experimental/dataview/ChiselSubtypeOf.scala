package chisel3.experimental

import chisel3._

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

import scala.quoted.*

/** Enforces that A is a Chisel subtype of B.
  *
  * A is a Chisel subtype of B if A contains all of B's fields (same names and
  * same types). Only public fields that are subtypes of chisel3.Data are
  * considered when checking for containment.
  *
  * In the following example A is a Chisel subtype of B:
  *
  *  {{{
  *    class A extends Bundle {
  *      val x = UInt(3.W)
  *      val y = UInt(3.W)
  *      val z = UInt(3.W)
  *    }
  *    class B extends Bundle {
  *      val x = UInt(3.W)
  *      val y = UInt(3.W)
  *    }
  *  }}}
  */
sealed trait ChiselSubtypeOf[A, B]

object ChiselSubtypeOf {
  def isGetter(using q: Quotes)(sym: q.reflect.Symbol): Boolean ={
    import q.reflect.*
    val params = sym.paramSymss.filter(ps => ps.headOption.isDefined && !ps.headOption.get.isType)
    params.isEmpty || params.forall(p => p.isEmpty)
  }

  extension (using q:Quotes)(sym: quotes.reflect.Symbol) {
    def members: List[quotes.reflect.Symbol] = {
      import q.reflect.*
      sym.fieldMembers ++ sym.methodMembers.filter(isGetter)
    }

    def member(name: String): Option[q.reflect.Symbol] = {
      import q.reflect.*
      sym.members.find(p => p.name == name)
    }
  }

  def genChiselSubtypeOf[A, B](using q: Quotes, tpa: Type[A], tpb: Type[B]): Expr[ChiselSubtypeOf[A, B]] = {
    import q.reflect.* 

      def baseType(t: TypeRepr): TypeRepr =
      if (t.baseClasses.length > 0)
        t.baseType(t.baseClasses(0))
      else
        Symbol.noSymbol.info

    def couldBeEqual(a: TypeRepr, b: TypeRepr): Boolean =
            // If one baseType is a subtype the other baseType, then these two
      // types could be equal, so we allow it and leave it to elaboration to
      // figure out. Otherwise, the types must be equal or we throw an error.
      (baseType(b) <:< baseType(a) || baseType(a) <:< baseType(b)) || a =:= b

    val a = TypeTree.of[A].symbol
    val b = TypeTree.of[B].symbol
    val tData = TypeRepr.of[Data]

    // Only look at public members that are getters and that are subtypes of Data.
    val mb = b.members.filter(m => m.typeRef <:< tData)
    // Go through every public member of b and make sure a member with the
    // same name exists in a and it has the same structural type.
    for (vb <- mb) {
      val vaTyp = a.member(vb.name).map(_.info.widen)
      val vbTyp = vb.info.widen
      if vaTyp.isEmpty || !couldBeEqual(vaTyp.get, vbTyp) then {
        val err = if vaTyp.isEmpty then s"${a}.${vb.name} does not exist" else s"${vaTyp.get} != ${vbTyp}"
        report.error(s"${a} is not a Chisel subtype of ${b}: mismatch at ${b}.${vb.name}: $err. Did you mean .viewAs[${b}]? " +
            "Please see https://www.chisel-lang.org/chisel3/docs/cookbooks/dataview")
      }
    }
    '{new ChiselSubtypeOf[A, B]{}}
  }
  // implicit inline def genChisel[A, B]: ChiselSubtypeOf[A, B] = ${ ChiselSubtypeOf.genChiselSubtypeOf[A, B] }
}

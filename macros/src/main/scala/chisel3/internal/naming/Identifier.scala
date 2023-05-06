// SPDX-License-Identifier: Apache-2.0

package chisel3.naming

// import scala.language.experimental.macros
// import scala.annotation.StaticAnnotation
// import scala.reflect.macros.blackbox
import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._

@experimental
class fixTraitIdentifier  extends MacroAnnotation {
  override def transform(using q: Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] = {
    import q.reflect._
    tree match {
      case ClassDef(name, ctr, parents, self, body) => {
        body.foreach {stat => 
          stat match {
            case aDef: DefDef if aDef.name.toString() == "_traitModuleDefinitionIdentifierProposal" =>
              report.error(s"Custom implementations of _traitModuleDefinitionIdentifierProposal are not allowed", aDef.pos)
            case _ => 
          }
        }
        val methType = MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Option[String]])
        val methSym = Symbol.newMethod(Symbol.spliceOwner, "_traitModuleDefinitionIdentifierProposal", methType, Flags.Override & Flags.Protected, Symbol.noSymbol)
        val methDef = DefDef(methSym, _ => Some(Literal(StringConstant(name))))
        List(methDef, tree)
      }
      case _ => {
        report.error(s"Can only use @identify on traits, not classes, objects, vals, or defs", tree.pos)
        List(tree)
      }
    }
  }
}

// private[chisel3] object identifyMacro {

//   def impl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
//     import c.universe._
//     val result = {
//       val (clz, objOpt) = annottees.map(_.tree).toList match {
//         case Seq(c, o) => (c, Some(o))
//         case Seq(c)    => (c, None)
//         case _ =>
//           throw new Exception(
//             s"Internal Error: Please file an issue at https://github.com/chipsalliance/chisel3/issues: Match error: annottees.map(_.tree).toList=${annottees.map(_.tree).toList}"
//           )
//       }
//       val newClz = clz match {
//         case q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
//           val defname = TypeName(tpname.toString + c.freshName())
//           val instname = TypeName(tpname.toString + c.freshName())
//           stats.foreach { stat =>
//             stat match {
//               case aDef: DefDef if aDef.name.toString == "_traitModuleDefinitionIdentifierProposal" =>
//                 c.error(aDef.pos, s"Custom implementations of _traitModuleDefinitionIdentifierProposal are not allowed")
//               case _ =>
//             }
//           }
//           val newMethod = q"override protected def _traitModuleDefinitionIdentifierProposal = Some(${tpname.toString})"
//           val newStats = newMethod +: stats
//           (
//             q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$newStats }",
//           )
//         case _ =>
//           c.error(c.enclosingPosition, "Can only use @identify on traits, not classes, objects, vals, or defs")
//           clz
//       }
//       objOpt match {
//         case None => newClz
//         case Some(o) =>
//           q"""
//             $newClz

//             $o
//           """
//       }
//     }
//     c.Expr[Any](result)
//   }
// }

// private[chisel3] class fixTraitIdentifier extends StaticAnnotation {
//   def macroTransform(annottees: Any*): Any = macro identifyMacro.impl
// }

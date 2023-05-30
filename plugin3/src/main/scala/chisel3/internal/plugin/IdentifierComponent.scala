// SPDX-License-Identifier: Apache-2.0

package chisel3.internal.plugin

import scala.collection.mutable
import scala.collection.mutable
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.report
import dotty.tools.dotc.transform.{PickleQuotes, Staging}
import dotty.tools.dotc.typer.{TyperPhase}


// The component of the chisel plugin. Not sure exactly what the difference is between
//   a Plugin and a PluginComponent.
class IdentifierComponent(arguments: ChiselPluginArguments)
    extends PluginPhase
    // with TypingTransformers
    with ChiselOuterUtils {
  import tpd._
  val runsAfter: Set[String] = Set(TyperPhase.name)
  val phaseName: String = "identifiercomponent"

  override def transformApply(tree: Apply)(using ctx: Context): Tree = {
    if (ChiselPlugin.runComponent(global, arguments)(unit)) {
      unit.body = new MyTypingTransformer(unit).transform(unit.body)
    }
  }

  private class MyTypingTransformer(unit: CompilationUnit) extends TypingTransformer(unit) with ChiselInnerUtils {

    def getConstructorAndParams(body: List[Tree]): (Option[DefDef], Seq[Symbol]) = {
      val paramAccessors = mutable.ListBuffer[Symbol]()
      var primaryConstructor: Option[DefDef] = None
      body.foreach {
        case acc: ValDef if acc.symbol.isParamAccessor && !acc.mods.hasFlag(Flag.BYNAMEPARAM) =>
          paramAccessors += acc.symbol
        case con: DefDef if con.symbol.isPrimaryConstructor =>
          primaryConstructor = Some(con)
        case d: DefDef if isNullaryMethodNamed("_moduleDefinitionIdentifierProposal", d) =>
          val msg = "Users cannot override _moduleDefinitionIdentifierProposal. Let the compiler plugin generate it."
          global.reporter.error(d.pos, msg)
        case _ =>
      }
      (primaryConstructor, paramAccessors.toList)
    }

    def generateIdentifierMethod(module: ClassDef, thiz: global.This, baseClass: global.Symbol): Tree = {
      val (conOpt, params) = getConstructorAndParams(module.impl.body)

      // The params have spaces after them (Scalac implementation detail)
      val paramLookup: Map[String, Symbol] = params.map { sym => sym.name.toString.trim -> sym }.toMap

      val str = stringFromTypeName(module.name)
      // Create a getProposal(this.<ref>) for each field matching order of constructor arguments
      val tpedNames: List[Tree] = (localTyper.typed(q"$str")) +:
        conOpt.toList.flatMap { x =>
          x.vparamss.flatMap(_.flatMap { vp =>
            paramLookup.get(vp.name.toString) match {
              case Some(p) =>
                // Make this.<ref>
                val select = gen.mkAttributedSelect(thiz.asInstanceOf[Tree], p)
                List(localTyper.typed(q"_root_.chisel3.naming.IdentifierProposer.getProposal($select)"))
              case None => Nil
            }
          })
        }
      val body = localTyper.typed(q"_root_.chisel3.naming.IdentifierProposer.makeProposal(..$tpedNames)")

      // Create the symbol for the method and have it be associated with the Module class
      val identifierSym = module.symbol.newMethod(
        TermName("_moduleDefinitionIdentifierProposal"),
        module.symbol.pos.focus,
        Flag.OVERRIDE | Flag.PROTECTED
      )
      identifierSym.setInfo(NullaryMethodType(stringTpe))

      localTyper.typed(DefDef(identifierSym, body)).asInstanceOf[DefDef]
    }

    override def transform(tree: Tree): Tree = tree match {

      case module: ClassDef
          if isAModule(module.symbol)
            && !isExactBaseModule(module.symbol)
            && !module.mods.hasFlag(Flags.TRAIT)
            && !module.name.decode.contains("$anon") =>
        val thiz: global.This = gen.mkAttributedThis(module.symbol)
        val original = baseModuleTpe.termSymbol

        // ==================== Generate _moduleDefinitionIdentifierProposal ====================
        val identifierMethod = generateIdentifierMethod(module, thiz, original)

        val withMethods = deriveClassDef(module) { t =>
          localTyper.typed(deriveTemplate(t) { x => x ++ Seq(identifierMethod) }).asInstanceOf[Template]
        }

        val typed = localTyper.typed(withMethods).asInstanceOf[ClassDef]

        super.transform(typed)

      case _ => super.transform(tree)
    }
  }
}

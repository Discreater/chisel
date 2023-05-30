// SPDX-License-Identifier: Apache-2.0

package chisel3.internal.plugin

import scala.collection.mutable
import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.report
import dotty.tools.dotc.transform.{PickleQuotes, Staging}
import dotty.tools.dotc.core.Types.AppliedType

/** Performs three operations
  * 1) Records that this plugin ran on a bundle by adding a method
  *    `override protected def _usingPlugin: Boolean = true`
  * 2) Constructs a cloneType method
  * 3) Builds a `def elements` that is computed once in this plugin
  *    Eliminates needing reflection to discover the hardware fields of a `Bundle`
  *
  * @param global     the environment
  * @param arguments  run time parameters to code
  */
private[plugin] class BundleComponent(arguments: ChiselPluginArguments)
    extends PluginPhase
    // with TastyTransforms
    with ChiselOuterUtils {
  import tpd._

  val phaseName: String = "chiselbundlephase"
  val runsAfter: Set[String] = Set("typer")

  override def transformApply(tree: Apply)(using ctx: Context): Tree =
    if (ChiselPlugin.runComponent(arguments)) {
      (new MyTypingTransformer).transform(tree)
    } else {
      tree
    }

  private class MyTypingTransformer(using ctx: Context) extends ChiselInnerUtils {
    import ctx._

    def cloneTypeFull(tree: Tree): Tree = {
      val methName = "chisel3.reflect.DataMirror.internal.chiselTypeClone"
      val funRef = ref(Symbols.requiredMethod(methName))
      Apply(TypeApply(funRef, List(TypeTree(tree.tpe))), List(tree))
    }

    def isVarArgs(sym: Symbol): Boolean = sym.info.isRepeatedParam

    def getConstructorAndParams(body: List[Tree], isBundle: Boolean): (Option[DefDef], Seq[Symbol]) = {
      val paramAccessors = mutable.ListBuffer[Symbol]()
      var primaryConstructor: Option[DefDef] = None
      body.foreach {
        case acc: ValDef if acc.symbol.is(Flags.Param) =>
          paramAccessors += acc.symbol
        case con: DefDef if con.symbol.isPrimaryConstructor =>
          if (con.symbol.isPrivate) {
            val msg = "Private bundle constructors cannot automatically be cloned, try making it package private"
            report.error(msg, con.sourcePos)
          } else {
            primaryConstructor = Some(con)
          }

        case d: DefDef if isNullaryMethodNamed("_cloneTypeImpl", d) =>
          val msg = "Users cannot override _cloneTypeImpl. Let the compiler plugin generate it."
          report.error(msg, d.sourcePos)
        case d: DefDef if isNullaryMethodNamed("_elementsImpl", d) && isBundle =>
          val msg = "Users cannot override _elementsImpl. Let the compiler plugin generate it."
          report.error(msg, d.sourcePos)
        case d: DefDef if isNullaryMethodNamed("_usingPlugin", d) && isBundle =>
          val msg = "Users cannot override _usingPlugin, it is for the compiler plugin's use only."
          report.error(msg, d.sourcePos)
        case d: DefDef if isNullaryMethodNamed("cloneType", d) =>
          val prefix = if (isBundle) "Bundles" else "Records"
          val msg = s"$prefix cannot override cloneType. Let the compiler plugin generate it."
          report.error(msg, d.sourcePos)
        case _ =>
      }
      (primaryConstructor, paramAccessors.toList)
    }

    def generateAutoCloneType(record: TypeDef, thiz: This, isBundle: Boolean): Option[Tree] = {
      val (recordTypeParams,recordBody) = record.rhs match {
        case Template(constr, _, _, body) => (constr.leadingTypeParams, body match {
          case b: Lazy[List[Tree]] => b.complete
          case b: List[Tree] => b
        })
        case _ => {
          report.error("Not an ClassDef", record.sourcePos)
          return None
        }
      }
      // TODO: check
      val (con, params) = getConstructorAndParams(recordBody, isBundle)
      if (con.isEmpty) {
        report.warning("Unable to determine primary constructor!", record.sourcePos)
        return None
      }

      val constructor = con.get

      // The params have spaces after them (Scalac implementation detail)
      val paramLookup: String => Symbol = params.map(sym => sym.name.toString.trim -> sym).toMap

      // Create a this.<ref> for each field matching order of constructor arguments
      // List of Lists because we can have multiple parameter lists
      val conArgs: List[List[Tree]] =
        constructor.paramss.map(_.map { vp =>
          val p = paramLookup(vp.name.toString)
          // Make this.<ref>
          val select = Select(thiz, p.name)
          // Clone any Data parameters to avoid field aliasing, need full clone to include direction
          val cloned = if (isData(vp.symbol)) cloneTypeFull(select.asInstanceOf[Tree]) else select
          // TODO: Check
          cloned
          // Need to splat varargs
          // if (isVarArgs(vp.symbol)) Typed(cloned, TypeTree.TypeLambda(List(TypeSymbol.fresh("T")), _ => TypeTree.Repeated(TypeTree(p.info.finalResultType))))
          // else cloned
        })

      val tparamList = recordTypeParams.map { t => t.typeOpt }
      
      val ttpe =
        if (tparamList.nonEmpty) AppliedType(record.typeOpt, tparamList) else record.typeOpt


      val newUntyped = New(ttpe, conArgs.flatten) // TODO: Check

      // TODO For private default constructors this crashes with a
      // TypeError. Figure out how to make this local to the object so
      // that private default constructors work.
      val neww = localTyper.typed(newUntyped)

      // Create the symbol for the method and have it be associated with the Record class
      val cloneTypeSym =
        record.symbol.newMethod(TermName("_cloneTypeImpl"), record.symbol.pos.focus, Flag.OVERRIDE | Flag.PROTECTED)
      // Handwritten cloneTypes don't have the Method flag set, unclear if it matters
      cloneTypeSym.resetFlag(Flags.METHOD)

      cloneTypeSym.setInfo(NullaryMethodType(recordTpe))

      Some(localTyper.typed(DefDef(cloneTypeSym, neww)))
    }

    def generateElements(bundle: ClassDef, thiz: global.This): Tree = {
      /* extract the true fields from the super classes a given bundle
       * depth argument can be helpful for debugging
       */
      def getAllBundleFields(bundleSymbol: Symbol, depth: Int = 0): List[(String, Tree)] = {

        def isBundleField(member: Symbol): Boolean = {
          if (!member.isAccessor) {
            false
          } else if (isData(member.tpe.typeSymbol)) {
            true
          } else if (isOptionOfData(member)) {
            true
          } else if (isSeqOfData(member)) {
            // This field is passed along, even though it is illegal
            // An error for this will be generated in `Bundle.elements`
            // It would be possible here to check for Seq[Data] and make a compiler error, but
            // that would be a API error difference. See reference in docs/chisel-plugin.md
            // If Bundle is subclass of IgnoreSeqInBundle then don't pass this field along

            !isIgnoreSeqInBundle(bundleSymbol)
          } else {
            // none of the above
            false
          }
        }

        val currentFields = bundleSymbol.info.members.flatMap {

          case member if member.isPublic =>
            if (isBundleField(member)) {
              // The params have spaces after them (Scalac implementation detail)
              Some(member.name.toString.trim -> gen.mkAttributedSelect(thiz.asInstanceOf[Tree], member))
            } else {
              None
            }

          case _ => None
        }.toList

        val allParentFields = bundleSymbol.parentSymbols.flatMap { parentSymbol =>
          val fieldsFromParent = if (depth < 1 && !isExactBundle(bundleSymbol)) {
            val foundFields = getAllBundleFields(parentSymbol, depth + 1)
            foundFields
          } else {
            List()
          }
          fieldsFromParent
        }
        allParentFields ++ currentFields
      }

      val elementArgs = getAllBundleFields(bundle.symbol)

      val elementsImplSym =
        bundle.symbol.newMethod(TermName("_elementsImpl"), bundle.symbol.pos.focus, Flag.OVERRIDE | Flag.PROTECTED)
      elementsImplSym.resetFlag(Flags.METHOD)
      elementsImplSym.setInfo(NullaryMethodType(itStringAnyTpe))

      val elementsImpl = localTyper.typed(
        DefDef(elementsImplSym, q"scala.collection.immutable.Vector.apply[(String, Any)](..$elementArgs)")
      )

      elementsImpl
    }

    override def transform(tree: Tree): Tree = tree match {
      case record: ClassDef
          if isARecord(record.symbol) && !record.mods.hasFlag(Flag.ABSTRACT) => // check that its not abstract
        val isBundle: Boolean = isABundle(record.symbol)
        val thiz:     This = This(record.symbol)

        // ==================== Generate _cloneTypeImpl ====================
        val cloneTypeImplOpt = generateAutoCloneType(record, thiz, isBundle)

        // ==================== Generate val elements (Bundles only) ====================
        val elementsImplOpt = if (isBundle) Some(generateElements(record, thiz)) else None

        // ==================== Generate _usingPlugin ====================
        val usingPluginOpt = if (isBundle) {
          // Unclear why quasiquotes work here but didn't for cloneTypeSym, maybe they could.
          Some(localTyper.typed(q"override protected def _usingPlugin: Boolean = true"))
        } else {
          None
        }

        val withMethods = deriveClassDef(record) { t =>
          deriveTemplate(t)(_ ++ cloneTypeImplOpt ++ usingPluginOpt ++ elementsImplOpt)
        }

        super.transform(localTyper.typed(withMethods))

      case _ => super.transform(tree)
    }
  }
}

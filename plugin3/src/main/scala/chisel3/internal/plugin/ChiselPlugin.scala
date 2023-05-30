// SPDX-License-Identifier: Apache-2.0

package chisel3.internal.plugin

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

private[plugin] case class ChiselPluginArguments(
  val skipFiles: mutable.HashSet[String] = mutable.HashSet.empty) {
  var deprecateSFC: Boolean = true
  def useBundlePluginOpt = "useBundlePlugin"
  def useBundlePluginFullOpt = s"-P:${ChiselPlugin.name}:$useBundlePluginOpt"
  def genBundleElementsOpt = "genBundleElements"
  def genBundleElementsFullOpt = s"-P:${ChiselPlugin.name}:$genBundleElementsOpt"
  // Annoying because this shouldn't be used by users
  def skipFilePluginOpt = "INTERNALskipFile:"
  def skipFilePluginFullOpt = s"-P:${ChiselPlugin.name}:$skipFilePluginOpt"
}

object ChiselPlugin {
  val name = "chiselplugin"

  // Also logs why the component was not run
  private[plugin] def runComponent(
    arguments: ChiselPluginArguments
  )(using ctx: Context
  ): Boolean = {
    // This plugin doesn't work on Scala 2.11 nor Scala 3. Rather than complicate the sbt build flow,
    // instead we just check the version and if its an early Scala version, the plugin does nothing
    val scalaVersion = scala.util.Properties.versionNumberString.split('.')
    val scalaVersionOk = scalaVersion(0).toInt == 2 && scalaVersion(1).toInt >= 12
    val skipFile = arguments.skipFiles(unit.source.file.path)
    if (scalaVersionOk && !skipFile) {
      true
    } else {
      val reason = if (!scalaVersionOk) {
        s"invalid Scala version '${scala.util.Properties.versionNumberString}'"
      } else {
        s"file skipped via '${arguments.skipFilePluginFullOpt}'"
      }
      // Enable this with scalacOption '-Ylog:chiselbundlephase'
      report.log(s"Skipping BundleComponent on account of $reason.")
      false
    }
  }
}

// The plugin to be run by the Scala compiler during compilation of Chisel code
class ChiselPlugin extends StandardPlugin {
  val name = ChiselPlugin.name
  val description = "Plugin for Chisel 3 Hardware Description Language"
  private val arguments = ChiselPluginArguments()
  val components: List[PluginPhase] = List[PluginPhase](
    new ChiselComponent(arguments),
    new BundleComponent(arguments),
    new IdentifierComponent(arguments),
    new DeprecateSFCComponent(arguments)
  )

  override def init(options: List[String], error: String => Unit): Boolean = {
    // Deprecate Scala 2.12 via the compiler plugin
    val scalaVersion = scala.util.Properties.versionNumberString.split('.')
    if (scalaVersion(0).toInt == 2 && scalaVersion(1).toInt == 12) {
      val msg = s"Chisel 5 is the last version that will support Scala 2.12. Please upgrade to Scala 2.13."

      global.reporter.warning(NoPosition, msg)
    }

    for (option <- options) {
      if (option == arguments.useBundlePluginOpt) {
        val msg = s"'${arguments.useBundlePluginFullOpt}' is now default behavior, you can remove the scalacOption."
        global.reporter.warning(NoPosition, msg)
      } else if (option.startsWith(arguments.skipFilePluginOpt)) {
        val filename = option.stripPrefix(arguments.skipFilePluginOpt)
        arguments.skipFiles += filename
      } else if (option == arguments.genBundleElementsOpt) {
        val msg = s"'${arguments.genBundleElementsOpt}' is now default behavior, you can remove the scalacOption."
        global.reporter.warning(NoPosition, msg)
      } else {
        error(s"Option not understood: '$option'")
      }
    }
    true
  }
}

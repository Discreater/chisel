// SPDX-License-Identifier: Apache-2.0

package chisel3.experimental.sourceinfo

import scala.quoted.*

import chisel3.experimental.{SourceInfo, SourceLine}

/** Provides a macro that returns the source information at the invocation point.
  */
object SourceInfoMacro {
  // def generate_source_info(c: Context): c.Tree = {
  //   import c.universe._
  //   val p = c.enclosingPosition

  //   val userDir = sys.props.get("user.dir") // Figure out what to do if not provided
  //   val projectRoot = sys.props.get("chisel.project.root")
  //   val root = projectRoot.orElse(userDir)

  //   val path = root.map(r => p.source.file.canonicalPath.stripPrefix(r)).getOrElse(p.source.file.name)
  //   val pathNoStartingSlash = if (path(0) == '/') path.tail else path

  //   q"_root_.chisel3.experimental.SourceLine($pathNoStartingSlash, ${p.line}, ${p.column})"
  // }
  def generate_source_info(using q: Quotes): Expr[SourceInfo] = {
    import q.reflect.* 
    val p = Position.ofMacroExpansion

    val userDir = sys.props.get("user.dir") // Figure out what to do if not provided
    val projectRoot = sys.props.get("chisel.project.root")
    val root = projectRoot.orElse(userDir)
    
  
    val path = root.flatMap(r => p.sourceFile.getJPath.map(_.toString().stripPrefix(r))).getOrElse(p.sourceFile.name)
    val pathNoStartingSlash = if (path(0) == '/') path.tail else path
    '{SourceLine(pathNoStartingSlash, p.startLine, p.startColumn)}
  }
}

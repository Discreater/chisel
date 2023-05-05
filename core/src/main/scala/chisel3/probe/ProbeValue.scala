// SPDX-License-Identifier: Apache-2.0

package chisel3.probe

import chisel3.{Data, SourceInfoDoc}
import chisel3.internal.{Builder, OpBinding}
import chisel3.internal.firrtl.{ProbeExpr, RWProbeExpr}
import chisel3.experimental.{requireIsHardware, SourceInfo}

import scala.language.experimental.macros

private[chisel3] sealed trait ProbeValueBase {
  protected def apply[T <: Data](source: T, writable: Boolean)(implicit sourceInfo: SourceInfo): T = {
    requireIsHardware(source)
    // construct probe to return with cloned info
    val clone = if (writable) RWProbe(source.cloneType) else Probe(source.cloneType)
    clone.bind(OpBinding(Builder.forcedUserModule, Builder.currentWhen))
    if (writable) {
      clone.setRef(RWProbeExpr(source.ref))
    } else {
      clone.setRef(ProbeExpr(source.ref))
    }
    clone
  }
}

object ProbeValue extends ProbeValueBase with SourceInfoDoc {

  /** Create a read-only probe expression. */
  inline def apply[T <: Data](inline source: T): T ={
    given sourceInfo: SourceInfo = summonInline[SourceInfo]
    do_apply(source)
  }

  /** @group SourceInfoTransformMacro */
  def do_apply[T <: Data](source: T)(implicit sourceInfo: SourceInfo): T = super.apply(source, writable = false)
}

object RWProbeValue extends ProbeValueBase with SourceInfoDoc {

  /** Create a read/write probe expression. */
  inline def apply[T <: Data](inline source: T): T ={
    given sourceInfo: SourceInfo = summonInline[SourceInfo]
    do_apply(source)
  }

  /** @group SourceInfoTransformMacro */
  def do_apply[T <: Data](source: T)(implicit sourceInfo: SourceInfo): T = super.apply(source, writable = true)
}

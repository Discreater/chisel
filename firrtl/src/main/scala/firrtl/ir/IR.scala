// SPDX-License-Identifier: Apache-2.0

package firrtl
package ir

import firrtl.annotations.Annotation
import org.apache.commons.text.translate.{AggregateTranslator, JavaUnicodeEscaper, LookupTranslator}

import scala.jdk.CollectionConverters._

/** Intermediate Representation */
abstract class FirrtlNode {
  def serialize: String
}

/** Use the [[firrtl.ir.Serializer]] to serialize this node. */
private[firrtl] trait UseSerializer extends FirrtlNode {
  def serialize: String = Serializer.serialize(this)
}

abstract class Info extends FirrtlNode with UseSerializer
case object NoInfo extends Info {
  override def toString: String = ""
}

/** Stores the string of a file info annotation in its escaped form. */
case class FileInfo(escaped: String) extends Info {
  override def toString: String = " @[" + escaped + "]"
  def unescaped:         String = FileInfo.unescape(escaped)
  def split:             (String, String, String) = FileInfo.split(escaped)
}

object FileInfo {
  def fromEscaped(s:   String): FileInfo = new FileInfo(s)
  def fromUnescaped(s: String): FileInfo = new FileInfo(escape(s))

  /** prepends a `\` to: `\`, `\n`, `\t` and `]` */
  def escape(s: String): String = EscapeFirrtl.translate(s)

  /** removes the `\` in front of `\`, `\n`, `\t` and `]` */
  def unescape(s: String): String = UnescapeFirrtl.translate(s)

  /** take an already escaped String and do the additional escaping needed for Verilog comment */
  def escapedToVerilog(s: String): String = EscapedToVerilog.translate(s)

  // custom `CharSequenceTranslator` for FIRRTL Info String escaping
  type CharMap = (CharSequence, CharSequence)
  private val EscapeFirrtl = new LookupTranslator(
    Seq[CharMap](
      "\\" -> "\\\\",
      "\n" -> "\\n",
      "\t" -> "\\t",
      "]" -> "\\]"
    ).toMap.asJava
  )
  private val UnescapeFirrtl = new LookupTranslator(
    Seq[CharMap](
      "\\\\" -> "\\",
      "\\n" -> "\n",
      "\\t" -> "\t",
      "\\]" -> "]"
    ).toMap.asJava
  )
  // EscapeFirrtl + EscapedToVerilog essentially does the same thing as running StringEscapeUtils.unescapeJava
  private val EscapedToVerilog = new AggregateTranslator(
    new LookupTranslator(
      Seq[CharMap](
        // ] is the one character that firrtl needs to be escaped that does not need to be escaped in
        "\\]" -> "]",
        "\"" -> "\\\"",
        // \n and \t are already escaped
        "\b" -> "\\b",
        "\f" -> "\\f",
        "\r" -> "\\r"
      ).toMap.asJava
    ),
    JavaUnicodeEscaper.outsideOf(32, 0x7f)
  )

  // Splits the FileInfo into its corresponding file, line, and column strings
  private def split(s: String): (String, String, String) = {
    s match {
      // Yield the three
      case FileInfoRegex(file, line, col) => (file, line, col)
      // Otherwise, just return the string itself and null for the other values
      case _ => (s, null, null)
    }
  }

  private val FileInfoRegex = """(?:([^\s]+)(?: (\d+)\:(\d+)))""".r
}

trait HasName {
  val name: String
}
trait HasInfo {
  val info: Info
}
trait IsDeclaration extends HasName with HasInfo

case class StringLit(string: String) extends FirrtlNode {
  import org.apache.commons.text.StringEscapeUtils

  /** Returns an escaped and quoted String */
  def escape: String = {
    "\"" + serialize + "\""
  }
  def serialize: String = StringEscapeUtils.escapeJava(string)

  /** Format the string for Verilog */
  def verilogFormat: StringLit = {
    StringLit(string.replaceAll("%x", "%h"))
  }

  /** Returns an escaped and quoted String */
  def verilogEscape: String = {
    // normalize to turn things like ö into o
    import java.text.Normalizer
    val normalized = Normalizer.normalize(string, Normalizer.Form.NFD)
    val ascii = normalized.flatMap(StringLit.toASCII)
    ascii.mkString("\"", "", "\"")
  }
}
object StringLit {
  import org.apache.commons.text.StringEscapeUtils

  /** Maps characters to ASCII for Verilog emission */
  private def toASCII(char: Char): List[Char] = char match {
    case nonASCII if !nonASCII.isValidByte => List('?')
    case '"'                               => List('\\', '"')
    case '\\'                              => List('\\', '\\')
    case c if c >= ' ' && c <= '~'         => List(c)
    case '\n'                              => List('\\', 'n')
    case '\t'                              => List('\\', 't')
    case _                                 => List('?')
  }

  /** Create a StringLit from a raw parsed String */
  def unescape(raw: String): StringLit = {
    StringLit(StringEscapeUtils.unescapeJava(raw))
  }
}

/** Primitive Operation
  *
  * See [[PrimOps]]
  */
abstract class PrimOp extends FirrtlNode {
  def serialize: String = this.toString
  def apply(args: Any*): DoPrim = {
    val groups = args.groupBy {
      case x: Expression => "exp"
      case x: BigInt     => "int"
      case x: Int        => "int"
      case other => "other"
    }
    val exprs = groups.getOrElse("exp", Nil).collect {
      case e: Expression => e
    }
    val consts = groups.getOrElse("int", Nil).map {
      _ match {
        case i: BigInt => i
        case i: Int    => BigInt(i)
      }
    }
    groups.get("other") match {
      case None    =>
      case Some(x) => sys.error(s"Shouldn't be here: $x")
    }
    DoPrim(this, exprs, consts, UnknownType)
  }
}

abstract class Expression extends FirrtlNode {
  def tpe: Type
}

case class Reference(name: String, tpe: Type = UnknownType) extends Expression with HasName with UseSerializer

case class SubField(expr: Expression, name: String, tpe: Type = UnknownType)
    extends Expression
    with HasName
    with UseSerializer

case class SubIndex(expr: Expression, value: Int, tpe: Type) extends Expression with UseSerializer

case class SubAccess(expr: Expression, index: Expression, tpe: Type) extends Expression with UseSerializer

case class Mux(cond: Expression, tval: Expression, fval: Expression, tpe: Type = UnknownType)
    extends Expression
    with UseSerializer
case class ValidIf(cond: Expression, value: Expression, tpe: Type) extends Expression with UseSerializer
abstract class Literal extends Expression {
  val value: BigInt
  val width: Width
}
case class UIntLiteral(value: BigInt, width: Width) extends Literal with UseSerializer {
  def tpe: UIntType = UIntType(width)
}
object UIntLiteral {
  def minWidth(value: BigInt): Width = IntWidth(BigInt(math.max(value.bitLength, 1)))
  def apply(value:    BigInt): UIntLiteral = new UIntLiteral(value, minWidth(value))

  /** Utility to construct UIntLiterals masked by the width
    *
    * This supports truncating negative values as well as values that are too wide for the width
    */
  def masked(value: BigInt, width: IntWidth): UIntLiteral = {
    val mask = (BigInt(1) << width.width.toInt) - BigInt(1)
    UIntLiteral(value & mask, width)
  }
}
case class SIntLiteral(value: BigInt, width: Width) extends Literal with UseSerializer {
  def tpe: SIntType = SIntType(width)
}
object SIntLiteral {
  def minWidth(value: BigInt): Width = IntWidth(BigInt(value.bitLength + 1))
  def apply(value:    BigInt): SIntLiteral = new SIntLiteral(value, minWidth(value))
}
case class DoPrim(op: PrimOp, args: Seq[Expression], consts: Seq[BigInt], tpe: Type)
    extends Expression
    with UseSerializer

abstract class Statement extends FirrtlNode
case class DefWire(info: Info, name: String, tpe: Type) extends Statement with IsDeclaration with UseSerializer
case class DefRegister(
  info:  Info,
  name:  String,
  tpe:   Type,
  clock: Expression,
  reset: Expression,
  init:  Expression)
    extends Statement
    with IsDeclaration
    with UseSerializer

object DefInstance {
  def apply(name: String, module: String): DefInstance = DefInstance(NoInfo, name, module)
}

case class DefInstance(info: Info, name: String, module: String, tpe: Type = UnknownType)
    extends Statement
    with IsDeclaration
    with UseSerializer

object ReadUnderWrite extends Enumeration {
  val Undefined: Value = Value("undefined")
  val Old: Value = Value("old")
  val New: Value = Value("new")
}

case class DefMemory(
  info:         Info,
  name:         String,
  dataType:     Type,
  depth:        BigInt,
  writeLatency: Int,
  readLatency:  Int,
  readers:      Seq[String],
  writers:      Seq[String],
  readwriters:  Seq[String],
  // TODO: handle read-under-write
  readUnderWrite: ReadUnderWrite.Value = ReadUnderWrite.Undefined)
    extends Statement
    with IsDeclaration
    with UseSerializer
case class DefNode(info: Info, name: String, value: Expression) extends Statement with IsDeclaration with UseSerializer
case class Conditionally(
  info:   Info,
  pred:   Expression,
  conseq: Statement,
  alt:    Statement)
    extends Statement
    with HasInfo
    with UseSerializer

object Block {
  def apply(head: Statement, tail: Statement*): Block = Block(head +: tail)
}

case class Block(stmts: Seq[Statement]) extends Statement with UseSerializer
case class Connect(info: Info, loc: Expression, expr: Expression) extends Statement with HasInfo with UseSerializer
case class IsInvalid(info: Info, expr: Expression) extends Statement with HasInfo with UseSerializer
case class Attach(info: Info, exprs: Seq[Expression]) extends Statement with HasInfo with UseSerializer

// TODO: @data class?
final class Stop(val info: Info,val ret: Int, val clk: Expression, val en: Expression, val name: String = "")
    extends Statement
    with HasInfo
    with IsDeclaration
    with UseSerializer
    with Product
    with Serializable {

  def withInfo(info: Info): Stop = new Stop(info = info, ret = ret, clk = clk, en = en, name = name)
  def withRet(ret: Int): Stop = new Stop(info = info, ret = ret, clk = clk, en = en, name = name)
  def withClk(clk: Expression): Stop = new Stop(info = info, ret = ret, clk = clk, en = en, name = name)
  def withEn(en: Expression): Stop = new Stop(info = info, ret = ret, clk = clk, en = en, name = name)
  def withName(name: String): Stop = new Stop(info = info, ret = ret, clk = clk, en = en, name = name)
  def copy(info: Info = info, ret: Int = ret, clk: Expression = clk, en: Expression = en): Stop = {
    new Stop(info, ret, clk, en, name)
  }

  override def canEqual(obj: Any) = obj != null & obj.isInstanceOf[Stop]

  override def equals(obj: Any) = this.eq(obj.asInstanceOf[AnyRef]) || canEqual(obj) && {
    val other = obj.asInstanceOf[Stop]
    info == other.info && ret == other.ret && clk == other.clk && en == other.en && name == other.name
  }

  override def hashCode() = {
    var code = 17 + "Stop".##
    code = 37 * code + info.##
    code = 37 * code + ret.##
    code = 37 * code + clk.##
    code = 37 * code + en.##
    code = 37 * code + name.##
    37 * code
  }

  private def tuple = Tuple5(info, ret, clk, en, name)

  override def productArity = 5
  override def productElement(n: Int) = n match {
    case 0 => info
    case 1 => ret
    case 2 => clk
    case 3 => en
    case 4 => name
    case n => throw new IndexOutOfBoundsException(n.toString)
  }
}
object Stop {
  def apply(info: Info, ret: Int, clk: Expression, en: Expression): Stop = new Stop(info, ret, clk, en)
  def apply(info: Info, ret: Int, clk: Expression, en: Expression, name: String): Stop = new Stop(info, ret, clk, en, name)
  def unapply(s: Stop): Some[(Info, Int, Expression, Expression)] = {
    Some((s.info, s.ret, s.clk, s.en))
  }
}
final class Print(
  val info:   Info,
  val string: StringLit,
  val args:   Seq[Expression],
  val clk:    Expression,
  val en:     Expression,
  val name: String = "")
    extends Statement
    with HasInfo
    with IsDeclaration
    with UseSerializer
    with Product
    with Serializable {

  def withInfo(info: Info): Print = new Print(info = info, string = string, args = args, clk = clk, en = en, name = name)
  def withString(string: StringLit): Print = new Print(info = info, string = string, args = args, clk = clk, en = en, name = name)
  def withArgs(args: Seq[Expression]): Print = new Print(info = info, string = string, args = args, clk = clk, en = en, name = name)
  def withClk(clk: Expression): Print = new Print(info = info, string = string, args = args, clk = clk, en = en, name = name)
  def withEn(en: Expression): Print = new Print(info = info, string = string, args = args, clk = clk, en = en, name = name)
  def withName(name: String): Print = new Print(info = info, string = string, args = args, clk = clk, en = en, name = name)
  def copy(
    info:   Info = info,
    string: StringLit = string,
    args:   Seq[Expression] = args,
    clk:    Expression = clk,
    en:     Expression = en
  ): Print = {
    Print(info, string, args, clk, en, name)
  }

  override def toString = {
    val b = new StringBuilder("Print(")
    b.append(String.valueOf(info))
    b.append(", ")
    b.append(String.valueOf(string))
    b.append(", ")
    b.append(String.valueOf(args))
    b.append(", ")
    b.append(String.valueOf(clk))
    b.append(", ")
    b.append(String.valueOf(en))
    b.append(", ")
    b.append(String.valueOf(name))
    b.append(")")
    b.toString
  }

  override def canEqual(obj: Any) = obj != null && obj.isInstanceOf[Print]

  override def equals(obj: Any) = this.eq(obj.asInstanceOf[AnyRef]) || canEqual(obj) && {
    val other = obj.asInstanceOf[Print]
    info == other.info && string == other.string && args == other.args && clk == other.clk && en == other.en && name == other.name
  }

  override def hashCode() = {
    var code = 17 + "Print".##
    code = 37 * code + info.##
    code = 37 * code + string.##
    code = 37 * code + args.##
    code = 37 * code + clk.##
    code = 37 * code + en.##
    code = 37 * code + name.##
    37 * code
  }

  private def tuple = (info, string, args, clk, en, name)

  override def productArity: Int = 6

  override def productElement(n: Int): Any = n match {
    case 0 => this.info
    case 1 => this.string
    case 2 => this.args
    case 3 => this.clk
    case 4 => this.en
    case 5 => this.name
    case _ => throw new IndexOutOfBoundsException(n.toString)
  }
}
object Print {

  def apply(info: Info, string: StringLit, args: Seq[Expression], clk: Expression, en: Expression): Print = new Print(info, string, args, clk, en)
  def apply(info: Info, string: StringLit, args: Seq[Expression], clk: Expression, en: Expression, name: String): Print = new Print(info, string, args, clk, en, name)

  def unapply(s: Print): Some[(Info, StringLit, Seq[Expression], Expression, Expression)] = {
    Some((s.info, s.string, s.args, s.clk, s.en))
  }
}

case class ProbeDefine(info: Info, sink: Expression, probeExpr: Expression) extends Statement with UseSerializer
case class ProbeExpr(expr: Expression, tpe: Type = UnknownType) extends Expression with UseSerializer

case class RWProbeExpr(expr: Expression, tpe: Type = UnknownType) extends Expression with UseSerializer
case class ProbeRead(expr: Expression, tpe: Type = UnknownType) extends Expression with UseSerializer

case class ProbeForceInitial(info: Info, probe: Expression, value: Expression) extends Statement with UseSerializer
case class ProbeReleaseInitial(info: Info, probe: Expression) extends Statement with UseSerializer
case class ProbeForce(info: Info, clock: Expression, cond: Expression, probe: Expression, value: Expression)
    extends Statement
    with UseSerializer
case class ProbeRelease(info: Info, clock: Expression, cond: Expression, probe: Expression)
    extends Statement
    with UseSerializer

// formal
object Formal extends Enumeration {
  val Assert: Value = Value("assert")
  val Assume: Value = Value("assume")
  val Cover: Value = Value("cover")
}

final class Verification(
  val op:   Formal.Value,
  val info: Info,
  val clk:  Expression,
  val pred: Expression,
  val en:   Expression,
  val msg:  StringLit,
  val name: String = "")
    extends Statement
    with HasInfo
    with IsDeclaration
    with UseSerializer
    with Product
    with Serializable {

  def withOp(op: Formal.Value): Verification = new Verification(op, info, clk, pred, en, msg, name)
  def withInfo(info: Info): Verification = new Verification(op, info, clk, pred, en, msg, name)
  def withClk(clk: Expression): Verification = new Verification(op, info, clk, pred, en, msg, name)
    def withPred(pred: Expression): Verification = new Verification(op, info, clk, pred, en, msg, name)
    def withEn(en: Expression): Verification = new Verification(op, info, clk, pred, en, msg, name)
    def withMsg(msg: StringLit): Verification = new Verification(op, info, clk, pred, en, msg, name)
    def withName(name: String): Verification = new Verification(op, info, clk, pred, en, msg, name)

  def copy(
    op:   Formal.Value = op,
    info: Info = info,
    clk:  Expression = clk,
    pred: Expression = pred,
    en:   Expression = en,
    msg:  StringLit = msg
  ): Verification = {
    Verification(op, info, clk, pred, en, msg, name)
  }

  override def toString = {
    val b = new StringBuilder("Verification(")
    b.append(String.valueOf(op))
    b.append(", ")
    b.append(String.valueOf(info))
    b.append(", ")
    b.append(String.valueOf(clk))
    b.append(", ")
    b.append(String.valueOf(pred))
    b.append(", ")
    b.append(String.valueOf(en))
    b.append(", ")
    b.append(String.valueOf(msg))
    b.append(", ")
    b.append(String.valueOf(name))
    b.append(")")
    b.toString
  }

  override def canEqual(obj: Any): Boolean = obj != null && obj.isInstanceOf[Verification]

  override def equals(obj: Any) = this.eq(obj.asInstanceOf[AnyRef]) || canEqual(obj) && {
    val other = obj.asInstanceOf[Verification]
    op == other.op && info == other.info && clk == other.clk && pred == other.pred && en == other.en && msg == other.msg && name == other.name
  }

  override def hashCode() = {
    var code = 17 + "Verification".##
    code = 37 * code + op.##
    code = 37 * code + info.##
    code = 37 * code + clk.##
    code = 37 * code + pred.##
    code = 37 * code + en.##
    code = 37 * code + msg.##
    code = 37 * code + name.##
    37 * code
  }

  private def tuple = (op, info, clk, pred, en, msg, name)

  override def productArity: Int = 7

  override def productElement(n: Int): Any = n match {
    case 0 => this.op
    case 1 => this.info
    case 2 => this.clk
    case 3 => this.pred
    case 4 => this.en
    case 5 => this.msg
    case 6 => this.name
    case _ => throw new IndexOutOfBoundsException(n.toString)
  }
}
object Verification {
  def apply(op: Formal.Value, info: Info, clk: Expression, pred: Expression, en: Expression, msg: StringLit): Verification =
    new Verification(op, info, clk, pred, en, msg)

  def apply(op: Formal.Value, info: Info, clk: Expression, pred: Expression, en: Expression, msg: StringLit, name: String): Verification =
    new Verification(op, info, clk, pred, en, msg, name)

  def unapply(s: Verification): Some[(Formal.Value, Info, Expression, Expression, Expression, StringLit)] = {
    Some((s.op, s.info, s.clk, s.pred, s.en, s.msg))
  }
}
// end formal

case object EmptyStmt extends Statement with UseSerializer

abstract class Width extends FirrtlNode {
  def +(x: Width): Width = (this, x) match {
    case (a: IntWidth, b: IntWidth) => IntWidth(a.width + b.width)
    case _ => UnknownWidth
  }
  def -(x: Width): Width = (this, x) match {
    case (a: IntWidth, b: IntWidth) => IntWidth(a.width - b.width)
    case _ => UnknownWidth
  }
  def max(x: Width): Width = (this, x) match {
    case (a: IntWidth, b: IntWidth) => IntWidth(a.width.max(b.width))
    case _ => UnknownWidth
  }
  def min(x: Width): Width = (this, x) match {
    case (a: IntWidth, b: IntWidth) => IntWidth(a.width.min(b.width))
    case _ => UnknownWidth
  }
}

/** Positive Integer Bit Width of a [[GroundType]] */
object IntWidth {
  private val maxCached = 1024
  private val cache = new Array[IntWidth](maxCached + 1)
  def apply(width: BigInt): IntWidth = {
    if (BigInt(0) <= width && width <= maxCached) {
      val i = width.toInt
      var w = cache(i)
      if (w eq null) {
        w = new IntWidth(width)
        cache(i) = w
      }
      w
    } else new IntWidth(width)
  }
  // For pattern matching
  def unapply(w: IntWidth): Option[BigInt] = Some(w.width)
}
class IntWidth(val width: BigInt) extends Width with Product with UseSerializer {
  override def equals(that: Any): Boolean = that match {
    case w: IntWidth => width == w.width
    case _ => false
  }
  override def hashCode = width.toInt
  override def productPrefix = "IntWidth"
  override def toString: String = s"$productPrefix($width)"
  def copy(width:    BigInt = width): IntWidth = IntWidth(width)
  def canEqual(that: Any): Boolean = that.isInstanceOf[Width]
  def productArity = 1
  def productElement(int: Int): Any = int match {
    case 0 => width
    case _ => throw new IndexOutOfBoundsException
  }
}
case object UnknownWidth extends Width with UseSerializer

/** Orientation of [[Field]] */
abstract class Orientation extends FirrtlNode
case object Default extends Orientation {
  def serialize: String = ""
}
case object Flip extends Orientation {
  def serialize: String = "flip "
}

/** Field of [[BundleType]] */
case class Field(name: String, flip: Orientation, tpe: Type) extends FirrtlNode with HasName with UseSerializer

/** Types of [[FirrtlNode]] */
abstract class Type extends FirrtlNode
abstract class GroundType extends Type {
  val width: Width
}
object GroundType {
  def unapply(ground: GroundType): Option[Width] = Some(ground.width)
}
abstract class AggregateType extends Type

case class ProbeType(underlying: Type) extends Type with UseSerializer
case class RWProbeType(underlying: Type) extends Type with UseSerializer

case class ConstType(underlying: Type) extends Type with UseSerializer

case class UIntType(width: Width) extends GroundType with UseSerializer
case class SIntType(width: Width) extends GroundType with UseSerializer

case class BundleType(fields: Seq[Field]) extends AggregateType with UseSerializer
case class VectorType(tpe: Type, size: Int) extends AggregateType with UseSerializer
case object ClockType extends GroundType with UseSerializer {
  val width: IntWidth = IntWidth(BigInt(1))
}
/* Abstract reset, will be inferred to UInt<1> or AsyncReset */
case object ResetType extends GroundType with UseSerializer {
  val width: IntWidth = IntWidth(BigInt(1))
}
case object AsyncResetType extends GroundType with UseSerializer {
  val width: IntWidth = IntWidth(BigInt(1))
}
case class AnalogType(width: Width) extends GroundType with UseSerializer
case object UnknownType extends Type with UseSerializer

/** [[Port]] Direction */
sealed abstract class Direction extends FirrtlNode
case object Input extends Direction {
  def serialize: String = "input"
}
case object Output extends Direction {
  def serialize: String = "output"
}

/** [[DefModule]] Port */
case class Port(
  info:      Info,
  name:      String,
  direction: Direction,
  tpe:       Type)
    extends FirrtlNode
    with IsDeclaration
    with UseSerializer

/** Parameters for external modules */
sealed abstract class Param extends FirrtlNode {
  def name: String
}

/** Integer (of any width) Parameter */
case class IntParam(name: String, value: BigInt) extends Param with UseSerializer

/** IEEE Double Precision Parameter (for Verilog real) */
case class DoubleParam(name: String, value: Double) extends Param with UseSerializer

/** String Parameter */
case class StringParam(name: String, value: StringLit) extends Param with UseSerializer

/** Raw String Parameter
  * Useful for Verilog type parameters
  * @note Firrtl doesn't guarantee anything about this String being legal in any backend
  */
case class RawStringParam(name: String, value: String) extends Param with UseSerializer

/** Base class for modules */
abstract class DefModule extends FirrtlNode with IsDeclaration {
  val info:  Info
  val name:  String
  val ports: Seq[Port]
}

/** Internal Module
  *
  * An instantiable hardware block
  */
case class Module(info: Info, name: String, ports: Seq[Port], body: Statement) extends DefModule with UseSerializer

/** External Module
  *
  * Generally used for Verilog black boxes
  * @param defname Defined name of the external module (ie. the name Firrtl will emit)
  */
case class ExtModule(
  info:    Info,
  name:    String,
  ports:   Seq[Port],
  defname: String,
  params:  Seq[Param])
    extends DefModule
    with UseSerializer

/** Intrinsic Module
  *
  * Used for compiler intrinsics.
  * @param intrinsic Defined intrinsic of the module
  */
case class IntModule(
  info:      Info,
  name:      String,
  ports:     Seq[Port],
  intrinsic: String,
  params:    Seq[Param])
    extends DefModule
    with UseSerializer

case class Circuit(info: Info, modules: Seq[DefModule], main: String) extends FirrtlNode with HasInfo with UseSerializer

case class CircuitWithAnnos(circuit: Circuit, annotations: Seq[Annotation]) extends FirrtlNode with UseSerializer

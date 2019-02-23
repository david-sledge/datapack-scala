package scala.data.pack

import scala.data.Assortment
import java.math.BigInteger

package object obj {

  sealed abstract class QualifiedName
  final case class Name(name: String) extends QualifiedName
  final case class FullName(namespaceName: String, name: String)
      extends QualifiedName

  sealed abstract class Token
  final case class DInt(int: BigInteger) extends Token
  final case class DFloat(float: Float) extends Token
  final case class DDouble(double: Double) extends Token
  case object DNil extends Token
  case object DTrue extends Token
  case object DFalse extends Token
  final case class DBin(bin: List[Byte]) extends Token
  final case class DStr(str: String) extends Token
  final case class DSequence(className: Option[QualifiedName] = None,
      Sequence: List[Token] = Nil) extends Token
  final case class DObject(className: Option[QualifiedName] = None,
      properties: Assortment[QualifiedName, Token] = Assortment()) extends Token
  final case class DAssortment(
      assortment: Assortment[Token, Token] = Assortment()) extends Token

  def toDInt(int: Long) = DInt(BigInteger.valueOf(int))
}

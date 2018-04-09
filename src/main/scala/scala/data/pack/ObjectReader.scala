package scala.data.pack

import java.io.InputStream
import scala.data.Assortment
import scala.data.Assortment._

import FormatBytes._
import ObjectReader._
import Reader._

object ObjectReader {

  private abstract class Zipper

  private abstract class AZValueHole extends Zipper

  private abstract class AZSequence extends AZValueHole
  private abstract class AZAssortment extends AZValueHole
  private abstract class AZObject extends Zipper

  private trait TZClassed

  private case object ZRoot extends AZValueHole
  final private case class ZSequenceHead(zipper: AZValueHole,
      className: Option[QualifiedName]) extends AZSequence with TZClassed
  final private case class ZSequence(zipper: AZSequence, data: Token)
      extends AZSequence
  final private case class ZAssortmentHead(zipper: AZValueHole)
      extends AZAssortment
  final private case class ZAssortment(zipper: AZAssortment,
      element: Element[Token, Token]) extends AZAssortment
  final private case class ZObjectHead(zipper: AZValueHole,
      className: Option[QualifiedName]) extends AZObject with TZClassed
  final private case class ZObject(zipper: AZObject,
      element: Element[QualifiedName, Token]) extends AZObject
  final private case class ZKey(zipper: AZAssortment, key: Token)
      extends AZValueHole
  final private case class ZName(zipper: AZObject, name: QualifiedName)
      extends AZValueHole
  final private case class ZAssortmentItem(zipper: AZAssortment)
      extends AZValueHole
  final private case class ZObjectItem(zipper: AZObject)
      extends AZValueHole

  final private case class ZClassName(zipper: TZClassed) extends Zipper
  final private case class ZQualifiedName(className: ZClassName,
      namespaceName: String) extends Zipper
  final private case class ZNamespaceName(zipper: AZObject,
      namespaceName: String) extends Zipper

  private def zipFoward(zipper: AZValueHole, data: Token) =
    zipper match {
    case ZRoot => Left(data)
    case z @ ZSequenceHead(_, _) => Right(ZSequence(z, data))
    case z @ ZSequence(_, _) => Right(ZSequence(z, data))
    case z @ ZAssortmentHead(_) => Right(ZKey(z, data))
    case z @ ZAssortment(_, _) => Right(ZKey(z, data))
    case ZKey(zipper_, key) => Right(ZAssortment(zipper_, Mapping(key, data)))
    case ZName(zipper_, name) => Right(ZObject(zipper_, Mapping(name, data)))
    case ZAssortmentItem(zipper_) => Right(ZAssortment(zipper_, Item(data)))
    case ZObjectItem(zipper_) => Right(ZObject(zipper_, Item(data)))
    case _ => throw new Exception("Programmatic error.  Flog the developer!")
  }

  private def toAZValueHole(zipper: Zipper) =
    zipper match {
      case z: AZValueHole => z
      case _ => throw new Exception("Programmatic error.  Flog the developer!")
    }

  private def toTZClassed(zipper: Zipper) =
    zipper match {
      case z: TZClassed => z
      case _ => throw new Exception("Programmatic error.  Flog the developer!")
    }

  private def foldSequence(zipper: AZSequence, list: List[Token]):
      Either[Token, Zipper] = {
    zipper match {
      case ZSequenceHead(zipper_, className) =>
        zipFoward(zipper_, DSequence(className, list))
      case ZSequence(zipper_, data) => foldSequence(zipper_, data :: list)
    }
  }

  private def foldAssortment(zipper: AZAssortment,
      assortment: Assortment[Token, Token]): Either[Token, Zipper] = {
    zipper match {
      case ZAssortmentHead(zipper_) => zipFoward(zipper_, DAssortment(assortment))
      case ZAssortment(zipper_, element) => foldAssortment(zipper_, element match {
          case Assortment.Entry(key) => assortment + key
          case Mapping(key, value) => assortment + (key, value)
          case Item(value) => assortment :+ value
        })
    }
  }

  private def foldObject(zipper: AZObject,
      assortment: Assortment[QualifiedName, Token]): Either[Token, Zipper] = {
    zipper match {
      case ZObjectHead(zipper_, className) =>
        zipFoward(zipper_, DObject(className, assortment))
      case ZObject(zipper_, element) => foldObject(zipper_, element match {
          case Assortment.Entry(key) => assortment + key
          case Mapping(key, value) => assortment + (key, value)
          case Item(value) => assortment :+ value
        })
    }
  }

  private def handler(zipper: Zipper, packType: PackType):
      Either[Token, Zipper] = packType match {
    case TInt(int) => zipFoward(toAZValueHole(zipper), DInt(int))
    case TNil => zipFoward(toAZValueHole(zipper), DNil)
    case TTrue => zipFoward(toAZValueHole(zipper), DTrue)
    case TFalse => zipFoward(toAZValueHole(zipper), DFalse)
    case TFloat(float) => zipFoward(toAZValueHole(zipper), DFloat(float))
    case TDouble(double) => zipFoward(toAZValueHole(zipper), DDouble(double))

    case TSequence => Right(ZSequenceHead(toAZValueHole(zipper), None))
    case TAssortment => Right(ZAssortmentHead(toAZValueHole(zipper)))
    case TObject => Right(ZObjectHead(toAZValueHole(zipper), None))

    case TClassName => Right(ZClassName(toTZClassed(zipper)))

    case TNs(ns) => Right(zipper match {
        case z @ ZClassName(_) => ZQualifiedName(z, ns)
        case z @ ZObjectHead(_, _) => ZNamespaceName(z, ns)
        case z @ ZObject(_, _) => ZNamespaceName(z, ns)
        case _ =>
          throw new Exception("Programmatic error.  Flog the developer!")
      })

    case TBin(list) => {
      lazy val localName = new String(list.toArray, "utf-8")
      zipper match {
        case z : AZValueHole => zipFoward(z, DBin(list))
        case z @ ZObjectHead(_, _) => Right(ZName(z, Name(localName)))
        case z @ ZObject(_, _) => Right(ZName(z, Name(localName)))
        case ZClassName(zipper_) => Right(zipper_ match {
          case ZSequenceHead(zipper__, None) =>
            ZSequenceHead(zipper__, Some(Name(localName)))
          case ZObjectHead(zipper__, None) =>
            ZObjectHead(zipper__, Some(Name(localName)))
          case _ =>
            throw new Exception("Programmatic error.  Flog the developer!")
        })
        case ZQualifiedName(ZClassName(zipper_), namespaceName) =>
          Right(zipper_ match {
            case ZSequenceHead(zipper__, None) =>
              ZSequenceHead(zipper__, Some(FullName(namespaceName, localName)))
            case ZObjectHead(zipper__, None) =>
              ZObjectHead(zipper__, Some(FullName(namespaceName, localName)))
            case _ =>
              throw new Exception("Programmatic error.  Flog the developer!")
          })
        case ZNamespaceName(zipper_, namespaceName) =>
          Right(ZName(zipper_, FullName(namespaceName, localName)))
        case _ =>
          throw new Exception("Programmatic error.  Flog the developer!")
      }
    }

    case TNoKeyValue => Right(zipper match {
        case z @ ZAssortmentHead(_) => ZAssortmentItem(z)
        case z @ ZAssortment(_, _) => ZAssortmentItem(z)
        case z @ ZObjectHead(_, _) => ZObjectItem(z)
        case z @ ZObject(_, _) => ZObjectItem(z)
        case ZKey(zipper_, key) => ZAssortment(zipper_, Assortment.Entry(key))
        case ZName(zipper_, name) => ZObject(zipper_, Assortment.Entry(name))
        case ZAssortmentItem(zipper_) => zipper_
        case ZObjectItem(zipper_) => zipper_
        case _ =>
          throw new Exception("Programmatic error.  Flog the developer!")
      })

    case TCollectionEnd => zipper match {
      case z @ ZSequenceHead(_, _) => foldSequence(z, List())
      case z @ ZSequence(_, _) => foldSequence(z, List())
      case z @ ZAssortmentHead(_) => foldAssortment(z, Assortment())
      case z @ ZAssortment(_, _) => foldAssortment(z, Assortment())
      case z @ ZObjectHead(_, _) => foldObject(z, Assortment())
      case z @ ZObject(_, _) => foldObject(z, Assortment())
      case _ => throw new Exception("Programmatic error.  Flog the developer!")
    }

    case _ => throw new Exception("Programmatic error.  Flog the developer!")
  }

  def unpack(is: InputStream) = {
    var obj: Either[Token, Zipper] = Right(ZRoot)

    val reader = new Reader(is, packType => {
      obj = handler(obj match {
        case Right(zipper) => zipper
        case _ =>
          throw new Exception("Programmatic error.  Flog the developer!")
      }, packType)
    })

    def readToken: Unit = {
      reader.readValue

      if (reader.state == SRoot)
        ()
      else
        readToken
    }

    readToken
    obj match {
      case Right(_) => None
      case Left(data) => Some(data)
    }
  }
}

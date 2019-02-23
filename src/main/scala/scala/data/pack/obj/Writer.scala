package scala.data.pack.obj

import scala.data.pack.stream._
import scala.data.Assortment
import scala.data.Assortment._
import java.io.OutputStream

object Writer {
  private def packChildren(children: List[Token], writer: Writer): Unit =
    children match {
      case token :: tail => {
        _pack(token, writer)
        packChildren(tail, writer)
      }
      case Nil => ()
    }

  private def packElements(elements: List[Element[Token, Token]], writer: Writer): Unit = {
    import writer._

    elements match {
    case element :: tail => {
      element match {
        case Assortment.Entry(key) => {
          _pack(key, writer)
          writeNoKeyValue
        }
        case Mapping(key, value) => {
          _pack(key, writer)
          _pack(value, writer)
        }
        case Item(value) => {
          writeNoKeyValue
          _pack(value, writer)
        }
      }

      packElements(tail, writer)
    }
    case Nil => ()
  }}

  private def packProperties(elements: List[Element[QualifiedName, Token]], writer: Writer): Unit = {
    import writer._

    elements match {
    case element :: tail => {
      element match {
        case Assortment.Entry(name) => {
          name match {
            case Name(name) => writePropertyName(None, name)
            case FullName(namespaceName, localName) => writePropertyName(Some(namespaceName), localName)
          }
          writeNoKeyValue
        }
        case Mapping(name, value) => {
          name match {
            case Name(name) => writePropertyName(None, name)
            case FullName(namespaceName, localName) => writePropertyName(Some(namespaceName), localName)
          }
          _pack(value, writer)
        }
        case Item(value) => {
          writeNoKeyValue
          _pack(value, writer)
        }
      }

      packProperties(tail, writer)
    }
    case Nil => ()
  }}

  private def _pack(token: Token, writer: Writer): Unit = {
    import writer._

    token match {
      case DNil => writeNil
      case DFalse => writeFalse
      case DTrue => writeTrue
      case DInt(int) => writeBigInt(int)
      case DFloat(float) => writeFloat(float)
      case DDouble(float) => writeDouble(float)
      case DStr(str) => writeStr(str)
      case DBin(list) => writeBin(list.toArray)
      case DSequence(className, list) => {
        writeSequenceStart(className match {
          case None => None
          case Some(Name(name)) => Some((None, name))
          case Some(FullName(namespaceName, localName)) => Some(Some(namespaceName), localName)
        })
        packChildren(list, writer)
        writeCollectionEnd
      }
      case DAssortment(assortment) => {
        writeAssortmentStart
        packElements(assortment.insertOrder, writer)
        writeCollectionEnd
      }
      case DObject(className, assortment) => {
        writeObjectStart(className match {
          case None => None
          case Some(Name(name)) => Some((None, name))
          case Some(FullName(namespaceName, localName)) => Some(Some(namespaceName), localName)
        })
        packProperties(assortment.insertOrder, writer)
        writeCollectionEnd
      }
    }
  }

  def pack(token: Token, os: OutputStream) = _pack(token, new Writer(os))
}

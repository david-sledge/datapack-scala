package scala.data.pack

import FormatBytes._
import java.io.EOFException
import java.io.InputStream
import java.nio.ByteBuffer
import Reader._

final class Reader(is: InputStream, handlePackType: PackType => Any) {

  private var _state: State = SRoot

  def state = _state

  private var _position = 0

  def position = _position

  private def validateAndTransitionState(
      init: () => Unit,
      sequence: SValue => Unit,
      assortment: SValue => Unit,
      obj: SValue => Unit,
      sequenceStart: SSequence => Unit,
      objectStart: SObject => Unit,
      className: SClassable => Unit,
      entryValue: SMappable => Unit,
      localName: SQualified => Unit
    ) = _state match {
    case SRoot => init()
    case SSequence(parent) => sequence(parent)
    case SAssortment(parent) => assortment(parent)
    case SObject(parent) => obj(parent)
    case SClass(parent @ SSequence(_)) => sequenceStart(parent)
    case SClass(parent @ SObject(_)) => objectStart(parent)
    case SClassName(parent) => className(parent)
    case SMappingValue(parent) => entryValue(parent)
    case SLocalName(parent) => localName(parent)
    case _ => throw new Exception("Programmatic error.  Flog the developer!")
  }

  private val noKeyValueValueValitioner =
    (packType: Byte) => (handler: () => Unit) => {
      validateAndTransitionState(
          () => throw new ReadStateException(packType, _state),
          (_) => throw new ReadStateException(packType, _state),
          (_) => {
            handler()
            _state = SMappingValue(_state.asInstanceOf[SMappable])
          },
          (_) => {
            handler()
            _state = SMappingValue(_state.asInstanceOf[SMappable])
          },
          (_) => throw new ReadStateException(packType, _state),
          (parent) => {
            handler()
            _state = SMappingValue(parent)
          },
          (_) => throw new ReadStateException(packType, _state),
          (parent) => {
            handler()
            _state = parent
          },
          (_) => throw new ReadStateException(packType, _state)
        )
    }

  private val collectionEndValitioner =
    (packType: Byte) => (handler: () => Unit) => {
      validateAndTransitionState(
          () => throw new ReadStateException(packType, _state),
          (parent) => {
            handler()
            _state = parent
          },
          (parent) => {
            handler()
            _state = parent
          },
          (parent) => {
            handler()
            _state = parent
          },
          (parent) => parent match {
            case SSequence(gparent) => {
              handler()
              _state = gparent
            }
          },
          (parent) => parent match {
            case SObject(gparent) => {
              handler()
              _state = gparent
            }
          },
          (_) => throw new ReadStateException(packType, _state),
          (parent) => {
            handlePackType(TNoKeyValue)
             _state = parent
            handler()
            _state = parent match {
              case SAssortment(gparent) => gparent
              case SObject(gparent) => gparent
            }
          },
          (_) => throw new ReadStateException(packType, _state)
        )
      validateAndTransitionState(
          () => (),
          (_) => (),
          (_) => _state = SMappingValue(_state.asInstanceOf[SMappable]),
          (_) => throw new Exception(
              "Programmatic error.  Flog the developer!"),
          (_) => throw new Exception(
              "Programmatic error.  Flog the developer!"),
          (_) => throw new Exception(
              "Programmatic error.  Flog the developer!"),
          (_) => throw new Exception(
              "Programmatic error.  Flog the developer!"),
          (parent) => _state = parent,
          (_) => throw new Exception("Programmatic error.  Flog the developer!")
        )
    }

  private val nonStringValueValitioner =
    (packType: Byte) => (handler: () => Unit) => {
      validateAndTransitionState(
          () => handler(),
          (_) => handler(),
          (_) => {
            handler()
            _state = SMappingValue(_state.asInstanceOf[SMappable])
          },
          (_) => throw new ReadStateException(packType, _state),
          (parent) => {
            handler()
            _state = parent
          },
          (_) => throw new ReadStateException(packType, _state),
          (_) => throw new ReadStateException(packType, _state),
          (parent) => {
            handler()
            _state = parent
          },
          (_) => throw new ReadStateException(packType, _state)
        )
    }

  private val classNameValitioner =
    (packType: Byte) => (handler: () => Unit) => {
      validateAndTransitionState(
          () => throw new ReadStateException(packType, _state),
          (_) => throw new ReadStateException(packType, _state),
          (_) => throw new ReadStateException(packType, _state),
          (_) => throw new ReadStateException(packType, _state),
          (parent) => {
            handler()
            _state = SClassName(parent)
          },
          (parent) => {
            handler()
            _state = SClassName(parent)
          },
          (_) => throw new ReadStateException(packType, _state),
          (_) => throw new ReadStateException(packType, _state),
          (_) => throw new ReadStateException(packType, _state)
        )
    }

  private val objectSequenceValitioner =
    (nsCollectionState: State => SClassable) => (packType: Byte) => (handler: () => Unit) => {
      validateAndTransitionState(
          () => {
            handler()
            _state = SClass(nsCollectionState(_state))
          },
          (_) => {
            handler()
            _state = SClass(nsCollectionState(_state))
          },
          (_) => {
            handler()
            _state = SClass(nsCollectionState(_state))
          },
          (_) => throw new ReadStateException(packType, _state),
          (parent) => {
            handler()
            _state = SClass(nsCollectionState(parent))
          },
          (_) => throw new ReadStateException(packType, _state),
          (_) => throw new ReadStateException(packType, _state),
          (_) => {
            handler()
            _state = SClass(nsCollectionState(_state))
          },
          (_) => throw new ReadStateException(packType, _state)
        )
    }

  private val assortmentValitioner =
    (packType: Byte) => (handler: () => Unit) => {
      validateAndTransitionState(
          () => {
            handler()
            _state = SAssortment(_state.asInstanceOf[SValue])
          },
          (_) => {
            handler()
            _state = SAssortment(_state.asInstanceOf[SValue])
          },
          (_) => {
            handler()
            _state = SAssortment(_state.asInstanceOf[SValue])
          },
          (_) => throw new ReadStateException(packType, _state),
          (parent) => {
            handler()
            _state = SAssortment(parent)
          },
          (_) => throw new ReadStateException(packType, _state),
          (_) => throw new ReadStateException(packType, _state),
          (_) => {
            handler()
            _state = SAssortment(_state.asInstanceOf[SValue])
          },
          (_) => throw new ReadStateException(packType, _state)
        )
    }

  private val nsValitioner = (packType: Byte) => (handler: () => Unit) => {
      validateAndTransitionState(
          () => throw new ReadStateException(packType, _state),
          (_) => throw new ReadStateException(packType, _state),
          (_) => throw new ReadStateException(packType, _state),
          (_) => {
            handler()
            _state = SLocalName(_state.asInstanceOf[SQualified])
          },
          (_) => throw new ReadStateException(packType, _state),
          (parent) => {
            handler()
            _state = SLocalName(parent match {case gparent @ SObject(_) => gparent})
          },
          (_) => {
            handler()
            _state = SLocalName(_state.asInstanceOf[SQualified])
          },
          (_) => {
            handler()
            throw new ReadStateException(packType, _state)
          },
          (_) => {
            handler()
            throw new ReadStateException(packType, _state)
          }
        )
    }

  private val binValitioner = (packType: Byte) => (handler: () => Unit) => {
      validateAndTransitionState(
          () => handler(),
          (_) => handler(),
          (_) => {
            handler()
            _state = SMappingValue(_state.asInstanceOf[SMappable])
          },
          (_) => {
            handler()
            _state = SMappingValue(_state.asInstanceOf[SMappable])
          },
          (parent) => {
            handler()
            _state = parent
          },
          (parent) => {
            handler()
            _state = SMappingValue(parent)
          },
          (parent) => {
            handler()
            _state = parent
          },
          (parent) => {
            handler()
            _state = parent
          },
          (parent) => {
            handler()
            _state = parent match {
              case SClassName(gparent) => gparent
              case _ => SMappingValue(parent.asInstanceOf[SMappable])
            }
          }
        )
    }

  val buffer = new Array[Byte](8)

  private def readBytes(buffer: Array[Byte], start: Int, length: Int): Int = {
    val bytesRead = is.read(buffer, start, length)

    if (bytesRead == -1) 0
    else if (bytesRead < length)
      readBytes(buffer, start + bytesRead, length - bytesRead) + bytesRead
    else bytesRead
  }

  private def readBytes_(length: Int, buffer: Array[Byte] = buffer): Unit = {
    val bytesRead = readBytes(buffer, 0, length)
    _position += bytesRead

    if (bytesRead < length)
    {
      throw new EOFException
    }
  }

  private def read1ByteUInt = {
    val int = is.read

    if (int == -1)
      throw new EOFException
    else {
      _position += 1
      int
    }
  }

  private def read2ByteUInt = {
    readBytes_(2)
    ((buffer(0) & 0xFF) << 8) | buffer(1) & 0x00FF
  }

  private def read4ByteUInt = {
    readBytes_(4)
    ((buffer(0) & 0xFF) << 24) |
      ((buffer(1) & 0xFF) << 16) |
      ((buffer(2) & 0xFF) << 8) |
      buffer(3) & 0x00FF
  }

  def readValue = {
    // read byte
    val iByte = is.read

    if (iByte == -1) {
      _state match {
        case SRoot => false
        case _ => throw new EOFException
      }
    }
    else {
      _position += 1

      val byte = iByte.toByte

      // identify format and verify that the byte is valid in the current state
      idFormat(byte) match {
        case Fixint(int) =>
          nonStringValueValitioner(byte)(() => handlePackType(TInt(int)))
        case FNil =>
          nonStringValueValitioner(byte)(() => handlePackType(TNil))
        case FColEnd =>
          collectionEndValitioner(byte)(() => handlePackType(TCollectionEnd))
        case FFalse =>
          nonStringValueValitioner(byte)(() => handlePackType(TFalse))
        case FTrue =>
          nonStringValueValitioner(byte)(() => handlePackType(TTrue))
        case FInt8 => nonStringValueValitioner(byte)(
            () => handlePackType(TInt(read1ByteUInt.toByte)))
        case FInt16 =>
          nonStringValueValitioner(byte)(() => {
            readBytes_(2)
            handlePackType(TInt(ByteBuffer.wrap(buffer).getShort))
          })
        case FInt32 =>
          nonStringValueValitioner(byte)(() => {
            readBytes_(4)
            handlePackType(TInt(ByteBuffer.wrap(buffer).getInt))
          })
        case FInt64 =>
          nonStringValueValitioner(byte)(() => {
            readBytes_(8)
            handlePackType(TInt(ByteBuffer.wrap(buffer).getLong))
          })
        case FFloat32 =>
          nonStringValueValitioner(byte)(() => {
            readBytes_(4)
            handlePackType(TFloat(ByteBuffer.wrap(buffer).getFloat))
          })
        case FFloat64 =>
          nonStringValueValitioner(byte)(() => {
            readBytes_(8)
            handlePackType(TDouble(ByteBuffer.wrap(buffer).getDouble))
          })
        case FBin8 =>
          binValitioner(byte)(() => {
            val length = read1ByteUInt
            val buffer = new Array[Byte](length)
            readBytes_(length, buffer)
            handlePackType(TBin(buffer.toList))
          })
        case FBin16 =>
          binValitioner(byte)(() => {
            val length = read2ByteUInt
            val buffer = new Array[Byte](length)
            readBytes_(length, buffer)
            handlePackType(TBin(buffer.toList))
          })
        case FBin32 =>
          binValitioner(byte)(() => {
            val length = read4ByteUInt
            val buffer = new Array[Byte](length)
            readBytes_(length, buffer)
            handlePackType(TBin(buffer.toList))
          })
        case FNs8 =>
          nsValitioner(byte)(() => {
            val length = read1ByteUInt
            val buffer = new Array[Byte](length)
            readBytes_(length, buffer)
            handlePackType(TNs(new String(buffer, "utf-8")))
          })
        case FNs16 =>
          nsValitioner(byte)(() => {
            val length = read2ByteUInt
            val buffer = new Array[Byte](length)
            readBytes_(length, buffer)
            handlePackType(TNs(new String(buffer, "utf-8")))
          })
        case FNs32 =>
          nsValitioner(byte)(() => {
            val length = read4ByteUInt
            val buffer = new Array[Byte](length)
            readBytes_(length, buffer)
            handlePackType(TNs(new String(buffer, "utf-8")))
          })
        case FClassname => classNameValitioner(byte)(
            () => handlePackType(TClassName))
        case FSequence => objectSequenceValitioner((state: State) => SSequence(state.asInstanceOf[SValue]))(byte)(
            () => handlePackType(TSequence))
        case FAssortment => assortmentValitioner(byte)(() => handlePackType(TAssortment))
        case FObject => objectSequenceValitioner((state: State) => SObject(state.asInstanceOf[SValue]))(byte)(
            () => handlePackType(TObject))
        case FNoKeyValue => noKeyValueValueValitioner(byte)(
            () => handlePackType(TNoKeyValue))
        case FFixbin(length) =>
          binValitioner(byte)(() => {
            val buffer = new Array[Byte](length)
            readBytes_(length, buffer)
            handlePackType(TBin(buffer.toList))
          })
        case FFixns(length) =>
          nsValitioner(byte)(() => {
            val buffer = new Array[Byte](length)
            readBytes_(length, buffer)
            handlePackType(TNs(new String(buffer, "utf-8")))
          })
        case FUnused => throw new UnusedFormatByteException(byte)
        case _ =>
          throw new Exception("Programmatic error.  Flog the developer!")
      }

      true
    }
  }
}

object Reader {
  private[pack] abstract class Format

  private[pack] final case class Fixint(int: Int) extends Format
  private[pack] case object FNil extends Format
  private[pack] case object FColEnd extends Format
  private[pack] case object FFalse extends Format
  private[pack] case object FTrue extends Format
  private[pack] case object FInt8 extends Format
  private[pack] case object FInt16 extends Format
  private[pack] case object FInt32 extends Format
  private[pack] case object FInt64 extends Format
  private[pack] case object FFloat32 extends Format
  private[pack] case object FFloat64 extends Format
  private[pack] case object FBin8 extends Format
  private[pack] case object FBin16 extends Format
  private[pack] case object FBin32 extends Format
  private[pack] case object FNs8 extends Format
  private[pack] case object FNs16 extends Format
  private[pack] case object FNs32 extends Format
  private[pack] case object FClassname extends Format
  private[pack] case object FSequence extends Format
  private[pack] case object FAssortment extends Format
  private[pack] case object FObject extends Format
  private[pack] case object FNoKeyValue extends Format
  private[pack] final case class FFixbin(length: Int) extends Format
  private[pack] final case class FFixns(length: Int) extends Format
  private[pack] case object FUnused extends Format

  private val formatMap = Map[Byte, Format](
      nilByte           -> FNil,
      collectionEndByte -> FColEnd,
      falseByte         -> FFalse,
      trueByte          -> FTrue,
      int8Byte          -> FInt8,
      int16Byte         -> FInt16,
      int32Byte         -> FInt32,
      int64Byte         -> FInt64,
      float32Byte       -> FFloat32,
      float64Byte       -> FFloat64,
      bin8Byte          -> FBin8,
      bin16Byte         -> FBin16,
      bin32Byte         -> FBin32,
      ns8Byte           -> FNs8,
      ns16Byte          -> FNs16,
      ns32Byte          -> FNs32,
      classNameByte     -> FClassname,
      sequenceByte          -> FSequence,
      assortmentByte    -> FAssortment,
      objectByte        -> FObject,
      noKeyValueByte    -> FNoKeyValue
    )

  private val fixMap = Map[Byte, Byte => Format](
      fixbinMask -> (new FFixbin(_)),
      fixnsMask  -> (new FFixns(_))
    )

  private[pack] def idFormat(byte: Byte) = {

    if (formatMap contains byte) formatMap(byte)
    else {
      val value = byte & fixintMask

      if (value == 0 || value == fixintMask) new Fixint(byte)
      else {
        val mask = (byte & fixMask).toByte

        if (fixMap contains mask) fixMap(mask)((byte & lenMask).toByte)
        else FUnused
      }
    }
  }

  sealed trait State
  sealed trait SParent extends State
  abstract class SChild(parent: SParent) extends State
  sealed trait SValue extends SParent
  sealed trait SClassable extends SParent
  sealed trait SMappable extends SParent
  sealed trait SQualified extends SParent

  // can be a parent, is not a child
  case object SRoot extends SValue
  // can be a parent, is a child: parent can be root, sequence, assortment, and mapping value
  final case class SSequence(parent: SValue) extends SChild(parent) with SValue with SClassable
  // can be a parent, is a child: parent can be root, sequence, assortment, and mapping value
  final case class SAssortment(parent: SValue) extends SChild(parent) with SValue with SMappable
  // can be a parent, is a child: parent can be root, sequence, assortment, and mapping value
  final case class SObject(parent: SValue) extends SChild(parent) with SClassable with SMappable with SQualified
  // cannot be a parent, is a child: parent can be sequence, object
  final case class SClass(parent: SClassable) extends SChild(parent)
  // can be a parent, is a child: parent can be sequence, object
  final case class SClassName(parent: SClassable) extends SChild(parent) with SQualified
  // can be a parent, is a child: assortment, object
  final case class SMappingValue(parent: SMappable) extends SChild(parent) with SValue
  // cannot be a parent, is a child: parent can be object, class name
  final case class SLocalName(parent: SQualified) extends SChild(parent)

  sealed abstract class PackType
  final case class TInt(int: Long) extends PackType
  case object TNil extends PackType
  case object TCollectionEnd extends PackType
  case object TTrue extends PackType
  case object TFalse extends PackType
  final case class TFloat(float: Float) extends PackType
  final case class TDouble(double: Double) extends PackType
  case object TClassName extends PackType
  final case class TBin(bin: List[Byte]) extends PackType
  final case class TNs(ns: String) extends PackType
  case object TSequence extends PackType
  case object TAssortment extends PackType
  case object TObject extends PackType
  case object TNoKeyValue extends PackType
}

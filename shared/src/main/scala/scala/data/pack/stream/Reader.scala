package scala.data.pack.stream

  import Reader._
  import Read._
import scala.data.pack.FormatBytes._
import java.io.EOFException
import java.nio.ByteBuffer
import java.math.BigInteger

final class Reader[S](s: S, handlePackType: PackType => Unit)(implicit r: Read[S]) {

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

  // validation + transition = validition
  private val noKeyValueValueValitioner =
    (packType: Byte) => (handler: () => Unit) => {
      validateAndTransitionState(
          init = () => throw new ReadStateException(packType, _state),
          sequence = (_) => throw new ReadStateException(packType, _state),
          assortment = (_) => {
            handler()
            _state = SMappingValue(_state.asInstanceOf[SMappable])
          },
          obj = (_) => {
            handler()
            _state = SMappingValue(_state.asInstanceOf[SMappable])
          },
          sequenceStart = (_) => throw new ReadStateException(packType, _state),
          objectStart = (parent) => {
            handler()
            _state = SMappingValue(parent)
          },
          className = (_) => throw new ReadStateException(packType, _state),
          entryValue = (parent) => {
            handler()
            _state = parent
          },
          localName = (_) => throw new ReadStateException(packType, _state)
        )
    }

  private val collectionEndValitioner =
    (packType: Byte) => (handler: () => Unit) => {
      validateAndTransitionState(
          init = () => throw new ReadStateException(packType, _state),
          sequence = (parent) => {
            handler()
            _state = parent
          },
          assortment = (parent) => {
            handler()
            _state = parent
          },
          obj = (parent) => {
            handler()
            _state = parent
          },
          sequenceStart = (parent) => parent match {
            case SSequence(gparent) => {
              handler()
              _state = gparent
            }
          },
          objectStart = (parent) => parent match {
            case SObject(gparent) => {
              handler()
              _state = gparent
            }
          },
          className = (_) => throw new ReadStateException(packType, _state),
          entryValue = (parent) => {
            handlePackType(TNoKeyValue)
             _state = parent
            handler()
            _state = parent match {
              case SAssortment(gparent) => gparent
              case SObject(gparent) => gparent
            }
          },
          localName = (_) => throw new ReadStateException(packType, _state)
        )
      validateAndTransitionState(
          init = () => (),
          sequence = (_) => (),
          assortment = (_) => _state = SMappingValue(_state.asInstanceOf[SMappable]),
          obj = (_) => throw new Exception(
              "Programmatic error.  Flog the developer!"),
          sequenceStart = (_) => throw new Exception(
              "Programmatic error.  Flog the developer!"),
          objectStart = (_) => throw new Exception(
              "Programmatic error.  Flog the developer!"),
          className = (_) => throw new Exception(
              "Programmatic error.  Flog the developer!"),
          entryValue = (parent) => _state = parent,
          localName = (_) => throw new Exception("Programmatic error.  Flog the developer!")
        )
    }

  private val nonStringValueValitioner =
    (packType: Byte) => (handler: () => Unit) => {
      validateAndTransitionState(
          init = () => handler(),
          sequence = (_) => handler(),
          assortment = (_) => {
            handler()
            _state = SMappingValue(_state.asInstanceOf[SMappable])
          },
          obj = (_) => throw new ReadStateException(packType, _state),
          sequenceStart = (parent) => {
            handler()
            _state = parent
          },
          objectStart = (_) => throw new ReadStateException(packType, _state),
          className = (_) => throw new ReadStateException(packType, _state),
          entryValue = (parent) => {
            handler()
            _state = parent
          },
          localName = (_) => throw new ReadStateException(packType, _state)
        )
    }

  private val classNameValitioner =
    (packType: Byte) => (handler: () => Unit) => {
      validateAndTransitionState(
          init = () => throw new ReadStateException(packType, _state),
          sequence = (_) => throw new ReadStateException(packType, _state),
          assortment = (_) => throw new ReadStateException(packType, _state),
          obj = (_) => throw new ReadStateException(packType, _state),
          sequenceStart = (parent) => {
            handler()
            _state = SClassName(parent)
          },
          objectStart = (parent) => {
            handler()
            _state = SClassName(parent)
          },
          className = (_) => throw new ReadStateException(packType, _state),
          entryValue = (_) => throw new ReadStateException(packType, _state),
          localName = (_) => throw new ReadStateException(packType, _state)
        )
    }

  private val objectSequenceValitioner =
    (nsCollectionState: State => SClassable) => (packType: Byte) => (handler: () => Unit) => {
      validateAndTransitionState(
          init = () => {
            handler()
            _state = SClass(nsCollectionState(_state))
          },
          sequence = (_) => {
            handler()
            _state = SClass(nsCollectionState(_state))
          },
          assortment = (_) => {
            handler()
            _state = SClass(nsCollectionState(_state))
          },
          obj = (_) => throw new ReadStateException(packType, _state),
          sequenceStart = (parent) => {
            handler()
            _state = SClass(nsCollectionState(parent))
          },
          objectStart = (_) => throw new ReadStateException(packType, _state),
          className = (_) => throw new ReadStateException(packType, _state),
          entryValue = (_) => {
            handler()
            _state = SClass(nsCollectionState(_state))
          },
          localName = (_) => throw new ReadStateException(packType, _state)
        )
    }

  private val assortmentValitioner =
    (packType: Byte) => (handler: () => Unit) => {
      validateAndTransitionState(
          init = () => {
            handler()
            _state = SAssortment(_state.asInstanceOf[SValue])
          },
          sequence = (_) => {
            handler()
            _state = SAssortment(_state.asInstanceOf[SValue])
          },
          assortment = (_) => {
            handler()
            _state = SAssortment(_state.asInstanceOf[SValue])
          },
          obj = (_) => throw new ReadStateException(packType, _state),
          sequenceStart = (parent) => {
            handler()
            _state = SAssortment(parent)
          },
          objectStart = (_) => throw new ReadStateException(packType, _state),
          className = (_) => throw new ReadStateException(packType, _state),
          entryValue = (_) => {
            handler()
            _state = SAssortment(_state.asInstanceOf[SValue])
          },
          localName = (_) => throw new ReadStateException(packType, _state)
        )
    }

  private val nsValitioner = (packType: Byte) => (handler: () => Unit) => {
      validateAndTransitionState(
          init = () => throw new ReadStateException(packType, _state),
          sequence = (_) => throw new ReadStateException(packType, _state),
          assortment = (_) => throw new ReadStateException(packType, _state),
          obj = (_) => {
            handler()
            _state = SLocalName(_state.asInstanceOf[SQualified])
          },
          sequenceStart = (_) => throw new ReadStateException(packType, _state),
          objectStart = (parent) => {
            handler()
            _state = SLocalName(parent match {case gparent @ SObject(_) => gparent})
          },
          className = (_) => {
            handler()
            _state = SLocalName(_state.asInstanceOf[SQualified])
          },
          entryValue = (_) => {
            handler()
            throw new ReadStateException(packType, _state)
          },
          localName = (_) => {
            handler()
            throw new ReadStateException(packType, _state)
          }
        )
    }

  private val strValitioner = (packType: Byte) => (handler: () => Unit) => {
      validateAndTransitionState(
          init = () => handler(),
          sequence = (_) => handler(),
          assortment = (_) => {
            handler()
            _state = SMappingValue(_state.asInstanceOf[SMappable])
          },
          obj = (_) => {
            handler()
            _state = SMappingValue(_state.asInstanceOf[SMappable])
          },
          sequenceStart = (parent) => {
            handler()
            _state = parent
          },
          objectStart = (parent) => {
            handler()
            _state = SMappingValue(parent)
          },
          className = (parent) => {
            handler()
            _state = parent
          },
          entryValue = (parent) => {
            handler()
            _state = parent
          },
          localName = (parent) => {
            handler()
            _state = parent match {
              case SClassName(gparent) => gparent
              case _ => SMappingValue(parent.asInstanceOf[SMappable])
            }
          }
        )
    }

  private val binValitioner = (packType: Byte) => (handler: () => Unit) => {
      validateAndTransitionState(
          init = () => handler(),
          sequence = (_) => handler(),
          assortment = (_) => {
            handler()
            _state = SMappingValue(_state.asInstanceOf[SMappable])
          },
          obj = (_) => {
            handler()
            _state = SMappingValue(_state.asInstanceOf[SMappable])
          },
          sequenceStart = (parent) => {
            handler()
            _state = parent
          },
          objectStart = (_) => throw new ReadStateException(packType, _state),
          className = (_) => throw new ReadStateException(packType, _state),
          entryValue = (parent) => {
            handler()
            _state = parent
          },
          localName = (_) => throw new ReadStateException(packType, _state)
        )
    }

  val buffer = new Array[Byte](8)

  private def readBytes(buffer: Array[Byte], start: Int, length: Int): Int = {
    val bytesRead = read(s, buffer, start, length)

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
    val int = readByte(s)

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

  private val formatMap = Map[Byte, () => Unit](
      NilByte           -> (() =>
        nonStringValueValitioner(NilByte)(() => handlePackType(TNil))),
      CollectionEndByte -> (() =>
        collectionEndValitioner(CollectionEndByte)(() => handlePackType(TCollectionEnd))),
      FalseByte         -> (() =>
        nonStringValueValitioner(FalseByte)(() => handlePackType(TFalse))),
      TrueByte          -> (() =>
        nonStringValueValitioner(TrueByte)(() => handlePackType(TTrue))),
      Int8Byte          -> (() =>
        nonStringValueValitioner(Int8Byte)(() => handlePackType(toTInt(read1ByteUInt.toByte)))),
      Int16Byte         -> (() =>
        nonStringValueValitioner(Int16Byte)(() => {
          readBytes_(2)
          handlePackType(toTInt(ByteBuffer.wrap(buffer).getShort))
        })),
      Int32Byte         -> (() =>
        nonStringValueValitioner(Int32Byte)(() => {
          readBytes_(4)
          handlePackType(toTInt(ByteBuffer.wrap(buffer).getInt))
        })),
      Int64Byte         -> (() =>
        nonStringValueValitioner(Int64Byte)(() => {
          readBytes_(8)
          handlePackType(toTInt(ByteBuffer.wrap(buffer).getLong))
        })),
      Uint8Byte         -> (() => nonStringValueValitioner(Uint8Byte)(
        () => handlePackType(toTInt(read1ByteUInt)))),
      Uint16Byte        -> (() =>
        nonStringValueValitioner(Uint16Byte)(() => {
          readBytes_(2)
          handlePackType(toTInt(ByteBuffer.wrap(buffer).getShort & 0x0000ffff))
        })),
      Uint32Byte        -> (() =>
        nonStringValueValitioner(Uint32Byte)(() => {
          readBytes_(4)
          handlePackType(toTInt(ByteBuffer.wrap(buffer).getInt & 0x00000000ffffffffL))
        })),
      Uint64Byte        -> (() =>
        nonStringValueValitioner(Uint64Byte)(() => {
          readBytes_(8)
          handlePackType(
          		TInt(BigInteger.valueOf(ByteBuffer.wrap(buffer).getLong + Long.MaxValue + 1L).setBit(63)))
        })),
      Float32Byte       -> (() =>
        nonStringValueValitioner(Float32Byte)(() => {
          readBytes_(4)
          handlePackType(TFloat(ByteBuffer.wrap(buffer).getFloat))
        })),
      Float64Byte       -> (() =>
        nonStringValueValitioner(Float64Byte)(() => {
          readBytes_(8)
          handlePackType(TDouble(ByteBuffer.wrap(buffer).getDouble))
        })),
      Bin8Byte          -> (() =>
        binValitioner(Bin8Byte)(() => {
          val length = read1ByteUInt
          val buffer = new Array[Byte](length)
          readBytes_(length, buffer)
          handlePackType(TBin(buffer.toList))
        })),
      Bin16Byte         -> (() =>
        binValitioner(Bin16Byte)(() => {
          val length = read2ByteUInt
          val buffer = new Array[Byte](length)
          readBytes_(length, buffer)
          handlePackType(TBin(buffer.toList))
        })),
      Bin32Byte         -> (() =>
        binValitioner(Bin32Byte)(() => {
          val length = read4ByteUInt
          val buffer = new Array[Byte](length)
          readBytes_(length, buffer)
          handlePackType(TBin(buffer.toList))
        })),
      Str8Byte          -> (() =>
        strValitioner(Str8Byte)(() => {
          val length = read1ByteUInt
          val buffer = new Array[Byte](length)
          readBytes_(length, buffer)
          handlePackType(TStr(new String(buffer, "utf-8")))
        })),
      Str16Byte         -> (() =>
        strValitioner(Str16Byte)(() => {
          val length = read2ByteUInt
          val buffer = new Array[Byte](length)
          readBytes_(length, buffer)
          handlePackType(TStr(new String(buffer, "utf-8")))
        })),
      Str32Byte         -> (() =>
        strValitioner(Str32Byte)(() => {
          val length = read4ByteUInt
          val buffer = new Array[Byte](length)
          readBytes_(length, buffer)
          handlePackType(TStr(new String(buffer, "utf-8")))
        })),
      Ns8Byte           -> (() =>
        nsValitioner(Ns8Byte)(() => {
          val length = read1ByteUInt
          val buffer = new Array[Byte](length)
          readBytes_(length, buffer)
          handlePackType(TNs(new String(buffer, "utf-8")))
        })),
      Ns16Byte          -> (() =>
        nsValitioner(Ns16Byte)(() => {
          val length = read2ByteUInt
          val buffer = new Array[Byte](length)
          readBytes_(length, buffer)
          handlePackType(TNs(new String(buffer, "utf-8")))
        })),
      Ns32Byte          -> (() =>
        nsValitioner(Ns32Byte)(() => {
          val length = read4ByteUInt
          val buffer = new Array[Byte](length)
          readBytes_(length, buffer)
          handlePackType(TNs(new String(buffer, "utf-8")))
        })),
      ClassNameByte     -> (() => classNameValitioner(ClassNameByte)(
        () => handlePackType(TClassName))),
      SequenceByte      -> (() =>
        objectSequenceValitioner((state: State) => SSequence(state.asInstanceOf[SValue]))(SequenceByte)(
            () => handlePackType(TSequence))),
      AssortmentByte    -> (() =>
        assortmentValitioner(AssortmentByte)(() => handlePackType(TAssortment))),
      ObjectByte        -> (() =>
        objectSequenceValitioner((state: State) => SObject(state.asInstanceOf[SValue]))(ObjectByte)(
            () => handlePackType(TObject))),
      NoKeyValueByte    -> (() => noKeyValueValueValitioner(NoKeyValueByte)(
            () => handlePackType(TNoKeyValue))),
    )

  private val fixMap = Map[Byte, (Byte, Byte) => Unit](
     FixbinMask -> ((byte, length) =>
        binValitioner(byte)(() => {
          val buffer = new Array[Byte](length)
          readBytes_(length, buffer)
          handlePackType(TBin(buffer.toList))
        })),
      FixstrMask -> ((byte, length) =>
        strValitioner(byte)(() => {
          val buffer = new Array[Byte](length)
          readBytes_(length, buffer)
          handlePackType(TStr(new String(buffer, "utf-8")))
        })),
      FixnsMask  -> ((byte, length) =>
        nsValitioner(byte)(() => {
          val buffer = new Array[Byte](length)
          readBytes_(length, buffer)
          handlePackType(TNs(new String(buffer, "utf-8")))
        })),
    )

  def readValue = {
    // read byte
    val iByte = readByte(s)

    if (iByte == -1) {
      _state match {
        case SRoot => false
        case _ => throw new EOFException
      }
    }
    else {
      _position += 1

      val byte = iByte.toByte

      if (formatMap contains byte)
        formatMap(byte)()
      else {
        val value = byte & FixintMask
  
        if (value == 0 || value == FixintMask)
          nonStringValueValitioner(byte)(() => handlePackType(toTInt(byte)))
        else {
          val mask = (byte & FixMask).toByte
  
          if (fixMap contains mask)
            fixMap(mask)(byte, (byte & LenMask).toByte)
          else
            throw new UnusedFormatByteException(byte)
        }
      }

      true
    }
  }

}

object Reader {

  sealed trait State
  sealed trait SParent extends State
  sealed abstract class SChild(parent: SParent) extends State
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
  final case class TInt(int: BigInteger) extends PackType
  case object TNil extends PackType
  case object TCollectionEnd extends PackType
  case object TTrue extends PackType
  case object TFalse extends PackType
  final case class TFloat(float: Float) extends PackType
  final case class TDouble(double: Double) extends PackType
  case object TClassName extends PackType
  final case class TBin(bin: List[Byte]) extends PackType
  final case class TStr(str: String) extends PackType
  final case class TNs(ns: String) extends PackType
  case object TSequence extends PackType
  case object TAssortment extends PackType
  case object TObject extends PackType
  case object TNoKeyValue extends PackType

  def toTInt(int: Long) = TInt(BigInteger.valueOf(int))
}

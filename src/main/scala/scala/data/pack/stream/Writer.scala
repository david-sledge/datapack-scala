package scala.data.pack.stream

import scala.data.pack._
  import FormatBytes._
import java.io.OutputStream
import java.nio.ByteBuffer
import java.math.BigInteger

class Writer(os: OutputStream) {
  import Writer._

/*
SItem(parent): possible parents; after entry, assortment, object
  value => pop state, check current state (if after entry write no value, pop state), write no key, push mapping value, write value
  no value => pop state
  collection end => pop state, apply end collection
SMappingValue(parent): possible parents; assortment, object
  value => write value
  no value => pop state, push after entry state
  collection end => apply end collection
SAfterEntry(parent): possible parents: assortment, object
  key => write no value, pop state, write key, push mapping value
  no key => push item state
  collection end => pop state, check current state (if after entry pop state), apply end collection
SAssortment(parent), SObject(parent): possible parents: mapping value, assortment, object, list, root
  key => write key, push mapping value state
  no key => push item state
  collection end => apply collection end
SList(parent): possible parents: mapping value, assortment, object, list, root
  value => write value
  no key/value => exception!!
  collection end => apply end collection
SRoot: no parent
  value => write value
  no key/value => exception!!
  collection end => exception!!
 */
  private var _state: State = SRoot

  def state = _state

  private def valueTransition: Unit = _state match {
    case s @ SAssortment(_) => _state = SMappingValue(s)
    case SMappingValue(s) => _state = s
    case SRoot | SList(_) => ()
    case _ => throw new Exception("programatic error.  Flog the developer!")
  }

  private def validateValueState = _state match {
    case SItem(parent) => {
      _state = SMappingValue(parent match {
        case SAfterEntry(gparent) => {
          os.write(NoKeyValueByte)
          gparent
        }
        case s:SMappable => s
        case _ => throw new WriteStateException(_state)
      })

      os.write(NoKeyValueByte)
    }
    case SMappingValue(parent) => ()
    case SAfterEntry(parent @ SAssortment(_)) => {
      // write no value, pop state
      os.write(NoKeyValueByte)
      _state = parent
    }
    case SAfterEntry(SObject(_)) | SObject(_) =>
      throw new WriteStateException(_state)
    case SRoot | SList(_) | SAssortment(_) => ()
  }

  def writeNil = {
    validateValueState
    os.write(NilByte)
    valueTransition
  }

  def writeFalse = {
    validateValueState
    os.write(FalseByte)
    valueTransition
  }

  def writeTrue = {
    validateValueState
    os.write(TrueByte)
    valueTransition
  }

  def writeCollectionEnd = {
    _state = _state match {
      case SItem(SAfterEntry(SAssortment(parent))) => parent
      case SItem(SAfterEntry(SObject(parent))) => parent
      case SItem(SAssortment(parent)) => parent
      case SItem(SObject(parent)) => parent
      case SMappingValue(SAssortment(parent)) => parent
      case SMappingValue(SObject(parent)) => parent
      case SAfterEntry(SAssortment(parent)) => parent
      case SAfterEntry(SObject(parent)) => parent
      case SAssortment(parent) => parent
      case SObject(parent) => parent
      case SList(parent) => parent
      case SRoot => throw new WriteStateException(_state)
    }

    os.write(CollectionEndByte)
    valueTransition
  }

  /*
   * cache the no key value bytes because if the next byte is a collection
   * end byte this byte does not get written.  Additionally, if the current
   * state is an assortment or object, then this doesn't get written if the next
   * byte to be written is another no key value byte.
   */
  def writeNoKeyValue = _state = _state match {
    case SRoot | SList(_) => throw new WriteStateException(_state)
    case SItem(parent) => parent
    case SMappingValue(parent) => SAfterEntry(parent)
    case s @ SAfterEntry(_) => SItem(s)
    case s @ SAssortment(_) => SItem(s)
    case s @ SObject(_) => SItem(s)
  }

  def writeFloat(float: Float) = {
    validateValueState

    os.write(Float32Byte)
    os.write(ByteBuffer.allocate(4).putFloat(float).array)
  }

  def writeDouble(double: Double) = {
    validateValueState

    os.write(Float64Byte)
    os.write(ByteBuffer.allocate(8).putDouble(double).array)
  }

  def writeBigInt(int: BigInteger) = {
    validateValueState

    if (BigInteger.valueOf(FixintMask).compareTo(int) <= 0
        && int.compareTo(BigInteger.valueOf(NilByte)) < 0) os.write(int.intValue)
    else if (BigInteger.valueOf(Byte.MinValue).compareTo(int) <= 0
        && int.compareTo(BigInteger.valueOf(Byte.MaxValue)) <= 0) {
      os.write(Int8Byte)
      os.write(int.intValue)
    }
    else if (BigInteger.valueOf(Byte.MaxValue).compareTo(int) < 0
        && int.compareTo(BigInteger.valueOf(MaxUint8)) <= 0) {
      os.write(Uint8Byte)
      os.write(int.intValue)
    }
    else if (BigInteger.valueOf(Short.MinValue).compareTo(int) <= 0
        && int.compareTo(BigInteger.valueOf(Short.MaxValue)) <= 0) {
      os.write(Int16Byte)
      os.write(ByteBuffer.allocate(2).putShort(int.shortValue).array)
    }
    else if (BigInteger.valueOf(Short.MaxValue).compareTo(int) < 0
        && int.compareTo(BigInteger.valueOf(MaxUint16)) <= 0) {
      os.write(Uint16Byte)
      os.write(ByteBuffer.allocate(2).putShort(int.shortValue).array)
    }
    else if (BigInteger.valueOf(Int.MinValue).compareTo(int) <= 0
        && int.compareTo(BigInteger.valueOf(Int.MaxValue)) <= 0) {
      os.write(Int32Byte)
      os.write(ByteBuffer.allocate(4).putInt(int.intValue).array)
    }
    else if (BigInteger.valueOf(Int.MaxValue).compareTo(int) < 0
        && int.compareTo(BigInteger.valueOf(MaxUint32)) <= 0) {
      os.write(Uint32Byte)
      os.write(ByteBuffer.allocate(4).putInt(int.intValue).array)
    }
    else if (BigInteger.valueOf(Long.MinValue).compareTo(int) <= 0
        && int.compareTo(BigInteger.valueOf(Long.MaxValue)) <= 0) {
      os.write(Int64Byte)
      os.write(ByteBuffer.allocate(8).putLong(int.longValue).array)
    }
    else if (BigInteger.valueOf(Long.MaxValue).compareTo(int) < 0
        && int.compareTo(MaxUint64) <= 0) {
      os.write(Uint64Byte)
      os.write(ByteBuffer.allocate(8).putLong(int.longValue).array)
    }
    else {
      throw new IntMagnitudeTooLargeException(int)
    }
  }

  def writeInt(int: Long) = {
    validateValueState

    if (FixintMask <= int && int < NilByte) os.write(int.toInt)
    else if (Byte.MinValue <= int && int <= Byte.MaxValue) {
      os.write(Int8Byte)
      os.write(int.toInt)
    }
    else if (Byte.MaxValue < int && int <= MaxUint8) {
      os.write(Uint8Byte)
      os.write(int.toInt)
    }
    else if (Short.MinValue <= int && int <= Short.MaxValue) {
      os.write(Int16Byte)
      os.write(ByteBuffer.allocate(2).putShort(int.toShort).array)
    }
    else if (Short.MaxValue < int && int <= MaxUint16) {
      os.write(Uint16Byte)
      os.write(ByteBuffer.allocate(2).putShort(int.toShort).array)
    }
    else if (Int.MinValue <= int && int <= Int.MaxValue) {
      os.write(Int32Byte)
      os.write(ByteBuffer.allocate(4).putInt(int.toInt).array)
    }
    else if (Int.MaxValue < int && int <= MaxUint32) {
      os.write(Uint32Byte)
      os.write(ByteBuffer.allocate(4).putInt(int.toInt).array)
    }
    else {
      os.write(Int64Byte)
      os.write(ByteBuffer.allocate(8).putLong(int).array)
    }
  }

  def writeBin(data: Array[Byte]) = {
    validateValueState

    writeByteArray(binByteSizes, data)
  }

  private val binByteSizes =
    Array[Byte](FixbinMask, Bin8Byte, Bin16Byte, Bin32Byte)
  private val nsByteSizes =
    Array[Byte](FixnsMask, Ns8Byte, Ns16Byte, Ns32Byte)

  private def writeByteArray(byteSizes: Array[Byte], data: Array[Byte]) = {
    if (data.length < 0x20) os.write(byteSizes(0) | data.length)
    else {
      if (data.length < 0x100) os.write(byteSizes(1))
      else if (data.length < 0x10000) os.write(byteSizes(2))
      else os.write(byteSizes(3))

      os.write(data.length)
    }

    os.write(data)
  }

  private def writeName(namespaceName: Option[String] = None, localName: String) = {

    namespaceName match {
      case None => ()
      case Some(name) => writeByteArray(nsByteSizes, name.getBytes("utf-8"))
    }

    writeByteArray(binByteSizes, localName.getBytes("utf-8"))
  }

  private def writeClassName(className: Option[(Option[String], String)]) = {
    className match {
      case None => ()
      case Some((namespaceName, localName)) => {
        os.write(ClassNameByte)
        writeName(namespaceName, localName)
      }
    }
  }

  def writePropertyName(namespaceName: Option[String] = None, localName: String) = {
    val parent = _state match {
      case SAfterEntry(s @ SObject(_)) => {
        // write no value, pop state
        os.write(NoKeyValueByte)
        s
      }
      case s @ SObject(_) => s
      case _ => throw new WriteStateException(_state)
    }

    writeName(namespaceName, localName)
    _state = SMappingValue(parent)
  }

  def writeSequenceStart(className: Option[(Option[String], String)]) = {
    validateValueState
    os.write(SequenceByte)
    writeClassName(className)
    _state = SList(_state.asInstanceOf[SParent])
  }

  def writeAssortmentStart = {
    validateValueState
    os.write(AssortmentByte)
    _state = SAssortment(_state.asInstanceOf[SParent])
  }

  def writeObjectStart(className: Option[(Option[String], String)]) = {
    validateValueState
    os.write(ObjectByte)
    writeClassName(className)
    _state = SObject(_state.asInstanceOf[SParent])
  }

  /**
   * close all open collections
   */
  def endData: Unit = {
    _state match {
      case SRoot => ()
      case _ => {
        writeCollectionEnd
        endData
      }
    }
  }
}

object Writer {
  sealed trait State
  sealed trait SParent extends State
  sealed abstract class SChild(parent: SParent) extends State
  sealed trait SItemParent extends SParent
  sealed abstract class SMappable(parent: SParent) extends SChild(parent) with SItemParent

  case object SRoot extends SParent
  final case class SList(parent: SParent) extends SChild(parent) with SParent
  final case class SMappingValue(parent: SMappable) extends SChild(parent) with SParent
  final case class SObject(parent: SParent) extends SMappable(parent)
  final case class SAssortment(parent: SParent) extends SMappable(parent)
  final case class SAfterEntry(parent: SMappable) extends SChild(parent) with SItemParent
  final case class SItem(parent: SItemParent) extends SChild(parent) with State
}

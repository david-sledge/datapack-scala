package scala.data.pack

import org.scalatest._
import java.io.ByteArrayOutputStream
import java.math.BigInteger

import Writer._
import FormatBytes._

class WriterSpec extends FlatSpec with Matchers {
  "A data pack writer" should "write a Nil" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte](NilByte)
    noException should be thrownBy writer.writeNil
    buffer.toByteArray.toList shouldBe expected
  }

  it should "throw an exception when an attempt is made to write a construct in an unsuitable state" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte]()
    an [WriteStateException] should be thrownBy writer.writeCollectionEnd
    buffer.toByteArray.toList shouldBe expected
  }

  it should "write a Boolean false" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte](FalseByte)
    noException should be thrownBy writer.writeFalse
    buffer.toByteArray.toList shouldBe expected
  }

  it should "write a Boolean true" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte](TrueByte)
    noException should be thrownBy writer.writeTrue
    buffer.toByteArray.toList shouldBe expected
  }

  it should "write a masked non-negative integer" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte](0x0f.toByte)
    noException should be thrownBy writer.writeInt(15)
    buffer.toByteArray.toList shouldBe expected
  }

  it should "write a masked non-negative integer (from a BigInteger instance)" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte](0x0f.toByte)
    noException should be thrownBy writer.writeBigInt(BigInteger.valueOf(15))
    buffer.toByteArray.toList shouldBe expected
  }

  it should "write a masked negative integer" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte](0xff.toByte)
    noException should be thrownBy writer.writeInt(-1)
    buffer.toByteArray.toList shouldBe expected
  }

  it should "write a masked negative integer (from a BigInteger instance)" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte](0xff.toByte)
    noException should be thrownBy writer.writeBigInt(BigInteger.valueOf(-1))
    buffer.toByteArray.toList shouldBe expected
  }

  it should "write a one-byte signed integer" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte](Int8Byte, 0x81.toByte)
    noException should be thrownBy writer.writeInt(-127)
    buffer.toByteArray.toList shouldBe expected
  }

  it should "write a one-byte signed integer (from a BigInteger instance)" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte](Int8Byte, 0x81.toByte)
    noException should be thrownBy writer.writeBigInt(BigInteger.valueOf(-127))
    buffer.toByteArray.toList shouldBe expected
  }

  it should "write a one-byte unsigned integer" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte](Uint8Byte, 0x81.toByte)
    noException should be thrownBy writer.writeInt(129)
    buffer.toByteArray.toList shouldBe expected
  }

  it should "write a one-byte unsigned integer (from a BigInteger instance)" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte](Uint8Byte, 0x81.toByte)
    noException should be thrownBy writer.writeBigInt(BigInteger.valueOf(129))
    buffer.toByteArray.toList shouldBe expected
  }

  it should "write a two-byte signed integer" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte](Int16Byte, 0x80.toByte, 0)
    noException should be thrownBy writer.writeInt(-32768)
    buffer.toByteArray.toList shouldBe expected
  }

  it should "write a two-byte signed integer (from a BigInteger instance)" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte](Int16Byte, 0x80.toByte, 0)
    noException should be thrownBy writer.writeBigInt(BigInteger.valueOf(-32768))
    buffer.toByteArray.toList shouldBe expected
  }

  it should "write a two-byte unsigned integer" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte](Uint16Byte, 0x80.toByte, 0)
    noException should be thrownBy writer.writeInt(32768)
    buffer.toByteArray.toList shouldBe expected
  }

  it should "write a two-byte unsigned integer (from a BigInteger instance)" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte](Uint16Byte, 0x80.toByte, 0)
    noException should be thrownBy writer.writeBigInt(BigInteger.valueOf(32768))
    buffer.toByteArray.toList shouldBe expected
  }

  it should "write a four-byte signed integer" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte](Int32Byte, 0x80.toByte, 0, 0, 0)
    noException should be thrownBy writer.writeInt(-2147483648)
    buffer.toByteArray.toList shouldBe expected
  }

  it should "write a four-byte signed integer (from a BigInteger instance)" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte](Int32Byte, 0x80.toByte, 0, 0, 0)
    noException should be thrownBy writer.writeBigInt(BigInteger.valueOf(-2147483648))
    buffer.toByteArray.toList shouldBe expected
  }

  it should "write a four-byte unsigned integer" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte](Uint32Byte, 0x80.toByte, 0, 0, 0)
    noException should be thrownBy writer.writeInt(2147483648L)
    buffer.toByteArray.toList shouldBe expected
  }

  it should "write a four-byte unsigned integer (from a BigInteger instance)" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte](Uint32Byte, 0x80.toByte, 0, 0, 0)
    noException should be thrownBy writer.writeBigInt(BigInteger.valueOf(2147483648L))
    buffer.toByteArray.toList shouldBe expected
  }

  it should "write an eight-byte signed integer" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte](Int64Byte, 0x80.toByte, 0, 0, 0, 0, 0, 0, 0)
    noException should be thrownBy writer.writeInt(Long.MinValue)
    buffer.toByteArray.toList shouldBe expected
  }

  it should "write an eight-byte signed integer (from a BigInteger instance)" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte](Int64Byte, 0x80.toByte, 0, 0, 0, 0, 0, 0, 0)
    noException should be thrownBy writer.writeBigInt(BigInteger.valueOf(Long.MinValue))
    buffer.toByteArray.toList shouldBe expected
  }

  it should "write an eight-byte unsigned integer (from a BigInteger instance)" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte](Uint64Byte, 0x80.toByte, 0, 0, 0, 0, 0, 0, 0)
    noException should be thrownBy writer.writeBigInt(new java.math.BigInteger(1,
      Array[Byte](
          -128, 0, 0, 0, 0, 0, 0, 0
        )
    ))
    buffer.toByteArray.toList shouldBe expected
  }

  it should "throw an exception when trying to write an unsigned integer that requires more than eight bytes" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte]()
    an [IntMagnitudeTooLargeException] should be thrownBy writer.writeBigInt(new java.math.BigInteger(1,
      Array[Byte](
          1, 0, 0, 0, 0, 0, 0, 0, 0
        )
    ))
    buffer.toByteArray.toList shouldBe expected
  }

  it should "throw an exception when trying to write a negative integer that requires more than eight bytes" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte]()
    an [IntMagnitudeTooLargeException] should be thrownBy writer.writeBigInt(new java.math.BigInteger(-1,
      Array[Byte](
          1, 0, 0, 0, 0, 0, 0, 0, 0
        )
    ))
    buffer.toByteArray.toList shouldBe expected
  }

  it should "write a four-byte floating point decimal" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte](Float32Byte, 0xc0.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte)
    noException should be thrownBy writer.writeFloat(-2f)
    buffer.toByteArray.toList shouldBe expected
  }

  it should "write an eight-byte floating point decimal" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List[Byte](Float64Byte, 0xc0.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte)
    noException should be thrownBy writer.writeDouble(-2)
    buffer.toByteArray.toList shouldBe expected
  }

  it should "write brief binary data" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val out = List.fill(5)(0x21.toByte)
    val expected = (FixbinMask | out.length) :: out
    noException should be thrownBy writer.writeBin(out.toArray)
    buffer.toByteArray.toList shouldBe expected
  }

  it should "write (seemingly) not-so-brief binary data" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val out = List.fill(64)(0x21.toByte)
    val expected = Bin8Byte :: out.length.toByte :: out
    noException should be thrownBy writer.writeBin(out.toArray)
    buffer.toByteArray.toList shouldBe expected
  }

  it should "not allow property names outside objects" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val out = List.fill(5)(0x21.toByte)
    val expected = Nil
    an [WriteStateException] should be thrownBy writer.writePropertyName(None, "!!!!!")
    buffer.toByteArray.toList shouldBe expected
  }

  it should "allow an empty unclassed sequence" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List(SequenceByte, CollectionEndByte)
    noException should be thrownBy {
        writer.writeSequenceStart(None)
        writer.writeCollectionEnd
      }
    buffer.toByteArray.toList shouldBe expected
  }

  it should "allow an empty classed (local name) sequence" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List(
        SequenceByte,
        ClassNameByte,
        (FixbinMask | 5).toByte,
        0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        CollectionEndByte
      )
    noException should be thrownBy {
        writer.writeSequenceStart(Some((None, "!!!!!")))
        writer.writeCollectionEnd
      }
    buffer.toByteArray.toList shouldBe expected
  }

  it should "allow an empty classed (fully-qualified name) sequence" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List(
        SequenceByte,
        ClassNameByte,
        (FixnsMask | 5).toByte,
        0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        (FixbinMask | 5).toByte,
        0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        CollectionEndByte
      )
    noException should be thrownBy {
        writer.writeSequenceStart(Some((Some("!!!!!"), "!!!!!")))
        writer.writeCollectionEnd
      }
    buffer.toByteArray.toList shouldBe expected
  }

  it should "allow an empty assortment" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List(AssortmentByte, CollectionEndByte)
    noException should be thrownBy {
        writer.writeAssortmentStart
        writer.writeNoKeyValue
        writer.writeCollectionEnd
      }
    buffer.toByteArray.toList shouldBe expected
  }

  it should "allow an empty unclassed object (collection of qualified name/value pairs)" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List(ObjectByte, CollectionEndByte)
    noException should be thrownBy {
        writer.writeObjectStart(None)
        writer.writeNoKeyValue
        writer.writeCollectionEnd
      }
    buffer.toByteArray.toList shouldBe expected
  }

  it should "allow an empty classed (local name) object" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List(
        ObjectByte,
        ClassNameByte,
        (FixbinMask | 5).toByte,
        0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        CollectionEndByte
      )
    noException should be thrownBy {
        writer.writeObjectStart(Some((None, "!!!!!")))
        writer.writeCollectionEnd
      }
    buffer.toByteArray.toList shouldBe expected
  }

  it should "allow an empty classed (fully-qualified name) object" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List(
        ObjectByte,
        ClassNameByte,
        (FixnsMask | 5).toByte,
        0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        (FixbinMask | 5).toByte,
        0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        CollectionEndByte
      )
    noException should be thrownBy {
        writer.writeObjectStart(Some((Some("!!!!!"), "!!!!!")))
        writer.writeCollectionEnd
      }
    buffer.toByteArray.toList shouldBe expected
  }

  it should "allow a non-empty assortment" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List(
        AssortmentByte,
        // key
        0x0f.toByte,
        // value
        0xff.toByte,
        // key without value
        0xff.toByte,
        CollectionEndByte
      )
    noException should be thrownBy {
        writer.writeAssortmentStart
        writer.writeInt(15)
        writer.writeInt(-1)
        writer.writeInt(-1)
        writer.writeNoKeyValue
        writer.writeCollectionEnd
      }
    buffer.toByteArray.toList shouldBe expected
  }

  it should "allow a non-empty unclassed sequence" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List(
        SequenceByte,
        0x0f.toByte,
        0xff.toByte,
        0xff.toByte,
        AssortmentByte,
        // key
        0x0f.toByte,
        // value
        0xff.toByte,
        // key without value
        0xff.toByte,
        CollectionEndByte,
        CollectionEndByte
      )
    noException should be thrownBy {
        writer.writeSequenceStart(None)
        writer.writeInt(15)
        writer.writeInt(-1)
        writer.writeInt(-1)
        writer.writeAssortmentStart
        writer.writeInt(15)
        writer.writeInt(-1)
        writer.writeInt(-1)
        writer.endData
      }
    buffer.toByteArray.toList shouldBe expected
  }

  it should "allow a non-empty classed (local name) sequence" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List(
        SequenceByte,
        ClassNameByte,
        (FixbinMask | 5).toByte,
        0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        0x0f.toByte,
        0xff.toByte,
        0xff.toByte,
        AssortmentByte,
        // key
        0x0f.toByte,
        // value
        0xff.toByte,
        // key without value
        0xff.toByte,
        CollectionEndByte,
        CollectionEndByte
      )
    noException should be thrownBy {
        writer.writeSequenceStart(Some((None, "!!!!!")))
        writer.writeInt(15)
        writer.writeInt(-1)
        writer.writeInt(-1)
        writer.writeAssortmentStart
        writer.writeInt(15)
        writer.writeInt(-1)
        writer.writeInt(-1)
        writer.endData
      }
    buffer.toByteArray.toList shouldBe expected
  }

  it should "allow a non-empty classed (fully-qualified name) sequence" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List(
        SequenceByte,
        ClassNameByte,
        (FixnsMask | 5).toByte,
        0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        (FixbinMask | 5).toByte,
        0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        0x0f.toByte,
        0xff.toByte,
        0xff.toByte,
        AssortmentByte,
        // key
        0x0f.toByte,
        // value
        0xff.toByte,
        // key without value
        0xff.toByte,
        CollectionEndByte,
        CollectionEndByte
      )
    noException should be thrownBy {
        writer.writeSequenceStart(Some((Some("!!!!!"), "!!!!!")))
        writer.writeInt(15)
        writer.writeInt(-1)
        writer.writeInt(-1)
        writer.writeAssortmentStart
        writer.writeInt(15)
        writer.writeInt(-1)
        writer.writeInt(-1)
        writer.endData
      }
    buffer.toByteArray.toList shouldBe expected
  }

  it should "allow an non-empty unclassed object" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List(
        ObjectByte,
        (FixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        SequenceByte,
        ClassNameByte,
        (FixnsMask | 5).toByte,
        0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        (FixbinMask | 5).toByte,
        0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        0x0f.toByte,
        0xff.toByte,
        0xff.toByte,
        AssortmentByte,
        // key
        0x0f.toByte,
        // value
        0xff.toByte,
        // key without value
        0xff.toByte,
        CollectionEndByte,
        CollectionEndByte,
        CollectionEndByte
      )

    writer.writeObjectStart(None)
    writer.writePropertyName(None, "!!!!!")
    writer.writeSequenceStart(Some((Some("!!!!!"), "!!!!!")))
    writer.writeInt(15)
    writer.writeInt(-1)
    writer.writeInt(-1)
    writer.writeAssortmentStart
    writer.writeInt(15)
    writer.writeInt(-1)
    writer.writeInt(-1)
    writer.endData

    buffer.toByteArray.toList shouldBe expected
  }

  it should "allow a non-empty classed (local name) object" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List(
        ObjectByte,
        ClassNameByte,
        (FixbinMask | 5).toByte,
        0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        (FixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        SequenceByte,
        ClassNameByte,
        (FixnsMask | 5).toByte,
        0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        (FixbinMask | 5).toByte,
        0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        0x0f.toByte,
        0xff.toByte,
        0xff.toByte,
        AssortmentByte,
        // key
        0x0f.toByte,
        // value
        0xff.toByte,
        // key without value
        0xff.toByte,
        CollectionEndByte,
        CollectionEndByte,
        CollectionEndByte
      )

    writer.writeObjectStart(Some((None, "!!!!!")))
    writer.writePropertyName(None, "!!!!!")
    writer.writeSequenceStart(Some((Some("!!!!!"), "!!!!!")))
    writer.writeInt(15)
    writer.writeInt(-1)
    writer.writeInt(-1)
    writer.writeAssortmentStart
    writer.writeInt(15)
    writer.writeInt(-1)
    writer.writeInt(-1)
    writer.endData

    buffer.toByteArray.toList shouldBe expected
  }

  it should "allow a non-empty classed (fully-qualified name) object" in {
    val buffer = new ByteArrayOutputStream
    val writer = new Writer(buffer)
    val expected = List(
        ObjectByte,
        ClassNameByte,
        (FixnsMask | 5).toByte,
        0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        (FixbinMask | 5).toByte,
        0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        (FixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        SequenceByte,
        ClassNameByte,
        (FixnsMask | 5).toByte,
        0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        (FixbinMask | 5).toByte,
        0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        0x0f.toByte,
        0xff.toByte,
        0xff.toByte,
        AssortmentByte,
        // key
        0x0f.toByte,
        // value
        0xff.toByte,
        // key without value
        0xff.toByte,
        CollectionEndByte,
        CollectionEndByte,
        CollectionEndByte
      )

    writer.writeObjectStart(Some((Some("!!!!!"), "!!!!!")))
    writer.writePropertyName(None, "!!!!!")
    writer.writeSequenceStart(Some((Some("!!!!!"), "!!!!!")))
    writer.writeInt(15)
    writer.writeInt(-1)
    writer.writeInt(-1)
    writer.writeAssortmentStart
    writer.writeInt(15)
    writer.writeInt(-1)
    writer.writeInt(-1)
    writer.endData

    buffer.toByteArray.toList shouldBe expected
  }
}

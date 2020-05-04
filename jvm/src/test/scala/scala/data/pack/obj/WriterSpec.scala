package scala.data.pack.obj

  import Writer._

import scala.data.pack.stream.io._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.data.Assortment
import scala.data.Assortment._
import java.io.ByteArrayOutputStream
import java.io.OutputStream

import scala.data.pack.FormatBytes._

class WriterSpec extends AnyFlatSpec with Matchers {
  "The object writer" should "write a Nil" in {
    val data = DNil
    val expected = Array[Byte](NilByte)
    val output = new ByteArrayOutputStream
    pack(data, output.asInstanceOf[OutputStream])
    output.toByteArray shouldBe expected
  }

  it should "write a Boolean false" in {
    val data = DFalse
    val expected = Array[Byte](FalseByte)
    val output = new ByteArrayOutputStream
    pack(data, output.asInstanceOf[OutputStream])
    output.toByteArray shouldBe expected
  }

  it should "write a Boolean true" in {
    val data = DTrue
    val expected = Array[Byte](TrueByte)
    val output = new ByteArrayOutputStream
    pack(data, output.asInstanceOf[OutputStream])
    output.toByteArray shouldBe expected
  }

  it should "write a masked non-negative integer" in {
    val data = toDInt(15)
    val expected = Array[Byte](0x0f)
    val output = new ByteArrayOutputStream
    pack(data, output.asInstanceOf[OutputStream])
    output.toByteArray shouldBe expected
  }

  it should "write a masked non-positive integer" in {
    val data = toDInt(-1)
    val expected = Array[Byte](0xff.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output.asInstanceOf[OutputStream])
    output.toByteArray shouldBe expected
  }

  it should "write a one-byte signed integer" in {
    val data = toDInt(127)
    val expected = Array[Byte](Int8Byte, 0x7f)
    val output = new ByteArrayOutputStream
    pack(data, output.asInstanceOf[OutputStream])
    output.toByteArray shouldBe expected
  }

  it should "write a two-byte signed integer" in {
    val data = toDInt(-32768)
    val expected = Array[Byte](Int16Byte, 0x80.toByte, 0)
    val output = new ByteArrayOutputStream
    pack(data, output.asInstanceOf[OutputStream])
    output.toByteArray shouldBe expected
  }

  it should "write a four-byte signed integer" in {
    val data = toDInt(-32769)
    val expected = Array[Byte](Int32Byte, 0xff.toByte, 0xff.toByte, 0x7f, 0xff.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output.asInstanceOf[OutputStream])
    output.toByteArray shouldBe expected
  }

  it should "write an eight-byte signed integer" in {
    val data = toDInt(-1085102592571150096L)
    val expected = Array[Byte](Int64Byte, 0xf0.toByte, 0xf0.toByte, 0xf0.toByte, 0xf0.toByte, 0xf0.toByte, 0xf0.toByte, 0xf0.toByte, 0xf0.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output.asInstanceOf[OutputStream])
    output.toByteArray shouldBe expected
  }

  it should "write a four-byte floating point decimal" in {
    val data = DFloat(-2f)
    val expected = Array[Byte](Float32Byte, 0xc0.toByte, 0x00, 0x00, 0x00)
    val output = new ByteArrayOutputStream
    pack(data, output.asInstanceOf[OutputStream])
    output.toByteArray shouldBe expected
  }

  it should "write an eight-byte floating point decimal" in {
    val data = DDouble(-2)
    val expected = Array[Byte](Float64Byte, 0xc0.toByte, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)
    val output = new ByteArrayOutputStream
    pack(data, output.asInstanceOf[OutputStream])
    output.toByteArray shouldBe expected
  }

  it should "write brief binary data" in {
    val data = DBin(List[Byte](0x21, 0x21, 0x21, 0x21, 0x21))
    val expected = Array[Byte]((FixbinMask | 0x05).toByte, 0x21, 0x21, 0x21, 0x21, 0x21)
    val output = new ByteArrayOutputStream
    pack(data, output.asInstanceOf[OutputStream])
    output.toByteArray shouldBe expected
  }

  it should "write not-so-brief binary data" in {
    val data = DBin(List.fill(32)(0x21))
    val expected = (Bin8Byte :: 32 :: List.fill(32)(0x21)).toArray
    val output = new ByteArrayOutputStream
    pack(data, output.asInstanceOf[OutputStream])
    output.toByteArray shouldBe expected
  }

  it should "allow an empty assortment" in {
    val data = DAssortment(Assortment())
    val expected = Array[Byte](AssortmentByte, CollectionEndByte)
    val output = new ByteArrayOutputStream
    pack(data, output.asInstanceOf[OutputStream])
    output.toByteArray shouldBe expected

//    val input = Array[Byte](AssortmentByte, CollectionEndByte)
//    val expected = Some(DAssortment(Assortment()))
//    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow a non-empty assortment" in {
    val data = DAssortment(Assortment[Token, Token]() + (toDInt(15), toDInt(-1)) + toDInt(-1))
    val expected = Array[Byte](
        AssortmentByte,
        // key
        0x0f,
        // value
        0xff.toByte,
        // key without value
        0xff.toByte,
        CollectionEndByte)
    val output = new ByteArrayOutputStream
    pack(data, output.asInstanceOf[OutputStream])
    output.toByteArray shouldBe expected
  }

  it should "allow an unclassed empty sequence" in {
    val data = DSequence(None, List())
    val expected = Array[Byte](SequenceByte, CollectionEndByte)
    val output = new ByteArrayOutputStream
    pack(data, output.asInstanceOf[OutputStream])
    output.toByteArray shouldBe expected
  }

  it should "allow an unclassed non-empty sequence" in {
    val data = DSequence(None,
        toDInt(15) ::
        toDInt(-1) ::
        toDInt(-1) ::
        DAssortment(Assortment[Token, Token]() +
            (toDInt(15), toDInt(-1)) +
            toDInt(-1)) :: Nil)
    val expected = Array[Byte](
        SequenceByte,
        0x0f,
        0xff.toByte,
        0xff.toByte,
        AssortmentByte,
        // key
        0x0f,
        // value
        0xff.toByte,
        // key without value
        0xff.toByte,
        CollectionEndByte,
        CollectionEndByte)
    val output = new ByteArrayOutputStream
    pack(data, output.asInstanceOf[OutputStream])
    output.toByteArray shouldBe expected
  }

  it should "allow a classed (local name only) empty sequence" in {
    val data = DSequence(Some(Name("!!!!!")))
    val expected = Array[Byte](SequenceByte, ClassNameByte,
        (FixstrMask | 0x05).toByte, 0x21, 0x21, 0x21, 0x21, 0x21,
        CollectionEndByte)
    val output = new ByteArrayOutputStream
    pack(data, output.asInstanceOf[OutputStream])
    output.toByteArray shouldBe expected
  }

  it should "allow a classed (fully qualified name) empty sequence" in {
    val data = DSequence(Some(FullName("!!!!!", "!!!!!")))
    val expected = Array[Byte](SequenceByte, ClassNameByte,
        (FixnsMask | 0x05).toByte, 0x21, 0x21, 0x21, 0x21, 0x21,
        (FixstrMask | 0x05).toByte, 0x21, 0x21, 0x21, 0x21, 0x21,
        CollectionEndByte)
    val output = new ByteArrayOutputStream
    pack(data, output.asInstanceOf[OutputStream])
    output.toByteArray shouldBe expected
  }

  it should "allow a classed (local name only) non-empty sequence" in {
    val data = DSequence(Some(Name("!!!!!")),
        toDInt(15) ::
        toDInt(-1) ::
        toDInt(-1) ::
        DAssortment(Assortment[Token, Token]() +
            (toDInt(15), toDInt(-1)) +
            toDInt(-1)) :: Nil)
    val expected = Array[Byte](SequenceByte, ClassNameByte,
        (FixstrMask | 0x05).toByte, 0x21, 0x21, 0x21, 0x21, 0x21,
        0x0f,
        0xff.toByte,
        0xff.toByte,
        AssortmentByte,
        // key
        0x0f,
        // value
        0xff.toByte,
        // key without value
        0xff.toByte,
        CollectionEndByte,
        CollectionEndByte)
    val output = new ByteArrayOutputStream
    pack(data, output.asInstanceOf[OutputStream])
    output.toByteArray shouldBe expected
  }

  it should "allow a classed (fully qualified name) non-empty sequence" in {
    val data = DSequence(Some(FullName("!!!!!", "!!!!!")),
        toDInt(15) ::
        toDInt(-1) ::
        toDInt(-1) ::
        DAssortment(Assortment[Token, Token]() +
            (toDInt(15), toDInt(-1)) +
            toDInt(-1)) :: Nil)
    val expected = Array[Byte](SequenceByte, ClassNameByte,
        (FixnsMask | 0x05).toByte, 0x21, 0x21, 0x21, 0x21, 0x21,
        (FixstrMask | 0x05).toByte, 0x21, 0x21, 0x21, 0x21, 0x21,
        0x0f,
        0xff.toByte,
        0xff.toByte,
        AssortmentByte,
        // key
        0x0f,
        // value
        0xff.toByte,
        // key without value
        0xff.toByte,
        CollectionEndByte,
        CollectionEndByte)
    val output = new ByteArrayOutputStream
    pack(data, output.asInstanceOf[OutputStream])
    output.toByteArray shouldBe expected
  }

  it should "allow an empty object (collection of qualified name/value pairs)" in {
    val data = DObject(None)
    val expected = Array[Byte](ObjectByte, CollectionEndByte)
    val output = new ByteArrayOutputStream
    pack(data, output.asInstanceOf[OutputStream])
    output.toByteArray shouldBe expected
  }

  it should "allow a non-empty object" in {
    val data = DObject(None, Assortment[QualifiedName, Token]() + (Name("!!!!!"), DBin(List(33, 33, 33, 33, 33))))
    val expected = Array[Byte](ObjectByte,
      (FixstrMask | 0x05).toByte,
      0x21.toByte, 0x21.toByte, 0x21, 0x21, 0x21,
      (FixbinMask | 0x05).toByte, 0x21, 0x21, 0x21, 0x21, 0x21,
      CollectionEndByte)
    val output = new ByteArrayOutputStream
    pack(data, output.asInstanceOf[OutputStream])
    output.toByteArray shouldBe expected
  }

  it should "allow a classed (local name only) empty object" in {
    val data = DObject(Some(Name("")))
    val expected = Array[Byte](ObjectByte, ClassNameByte, (FixstrMask | 0x0).toByte, CollectionEndByte)
    val output = new ByteArrayOutputStream
    pack(data, output.asInstanceOf[OutputStream])
    output.toByteArray shouldBe expected
  }
}

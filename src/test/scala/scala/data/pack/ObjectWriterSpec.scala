package scala.data.pack

import org.scalatest._
import scala.data.Assortment
import scala.data.Assortment._
import java.io.ByteArrayOutputStream

import FormatBytes._
import ObjectWriter._

import Writer._

class ObjectWriterSpec extends FlatSpec with Matchers {
  "The object writer" should "write a Nil" in {
    val data = DNil
    val expected = Array[Byte](nilByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected
  }

  it should "write a Boolean false" in {
    val data = DFalse
    val expected = Array[Byte](falseByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected
  }

  it should "write a Boolean true" in {
    val data = DTrue
    val expected = Array[Byte](trueByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected
  }

  it should "write a masked non-negative integer" in {
    val data = DInt(15)
    val expected = Array[Byte](0x0f.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected
  }

  it should "write a masked non-positive integer" in {
    val data = DInt(-1)
    val expected = Array[Byte](0xff.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected
  }

  it should "write a one-byte signed integer" in {
    val data = DInt(127)
    val expected = Array[Byte](int8Byte, 0x7f.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected
  }

  it should "write a two-byte signed integer" in {
    val data = DInt(-32768)
    val expected = Array[Byte](int16Byte, 0x80.toByte, 0.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected
  }

  it should "write a four-byte signed integer" in {
    val data = DInt(-32769)
    val expected = Array[Byte](int32Byte, 0xff.toByte, 0xff.toByte, 0x7f.toByte, 0xff.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected
  }

  it should "write an eight-byte signed integer" in {
    val data = DInt(-1085102592571150096l)
    val expected = Array[Byte](int64Byte, 0xf0.toByte, 0xf0.toByte, 0xf0.toByte, 0xf0.toByte, 0xf0.toByte, 0xf0.toByte, 0xf0.toByte, 0xf0.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected
  }

  it should "write a four-byte floating point decimal" in {
    val data = DFloat(-2f)
    val expected = Array[Byte](float32Byte, 0xc0.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected
  }

  it should "write an eight-byte floating point decimal" in {
    val data = DDouble(-2)
    val expected = Array[Byte](float64Byte, 0xc0.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected
  }

  it should "write brief binary data" in {
    val data = DBin(List[Byte](0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte))
    val expected = Array[Byte]((fixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected
  }

  it should "write not-so-brief binary data" in {
    val data = DBin(List.fill(32)(0x21.toByte))
    val expected = (bin8Byte :: 32.toByte :: List.fill(32)(0x21.toByte)).toArray
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected
  }

  it should "allow an empty assortment" in {
    val data = DAssortment(Assortment())
    val expected = Array[Byte](assortmentByte, collectionEndByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected

//    val input = Array[Byte](assortmentByte, collectionEndByte)
//    val expected = Some(DAssortment(Assortment()))
//    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow a non-empty assortment" in {
    val data = DAssortment(Assortment[Token, Token]() + (DInt(15), DInt(-1)) + DInt(-1))
    val expected = Array[Byte](
        assortmentByte,
        // key
        0x0f.toByte,
        // value
        0xff.toByte,
        // key without value
        0xff.toByte,
        collectionEndByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected
  }

  it should "allow an unclassed empty sequence" in {
    val data = DSequence(None, List())
    val expected = Array[Byte](sequenceByte, collectionEndByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected
  }

  it should "allow an unclassed non-empty sequence" in {
    val data = DSequence(None,
        DInt(15) ::
        DInt(-1) ::
        DInt(-1) ::
        DAssortment(Assortment[Token, Token]() +
            (DInt(15), DInt(-1)) +
            DInt(-1)) :: Nil)
    val expected = Array[Byte](
        sequenceByte,
        0x0f.toByte,
        0xff.toByte,
        0xff.toByte,
        assortmentByte,
        // key
        0x0f.toByte,
        // value
        0xff.toByte,
        // key without value
        0xff.toByte,
        collectionEndByte,
        collectionEndByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected
  }

  it should "allow a classed (local name only) empty sequence" in {
    val data = DSequence(Some(Name("!!!!!")))
    val expected = Array[Byte](sequenceByte, classNameByte,
        (fixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        collectionEndByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected
  }

  it should "allow a classed (fully qualified name) empty sequence" in {
    val data = DSequence(Some(FullName("!!!!!", "!!!!!")))
    val expected = Array[Byte](sequenceByte, classNameByte,
        (fixnsMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        (fixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        collectionEndByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected
  }

  it should "allow a classed (local name only) non-empty sequence" in {
    val data = DSequence(Some(Name("!!!!!")),
        DInt(15) ::
        DInt(-1) ::
        DInt(-1) ::
        DAssortment(Assortment[Token, Token]() +
            (DInt(15), DInt(-1)) +
            DInt(-1)) :: Nil)
    val expected = Array[Byte](sequenceByte, classNameByte,
        (fixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        0x0f.toByte,
        0xff.toByte,
        0xff.toByte,
        assortmentByte,
        // key
        0x0f.toByte,
        // value
        0xff.toByte,
        // key without value
        0xff.toByte,
        collectionEndByte,
        collectionEndByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected
  }

  it should "allow a classed (fully qualified name) non-empty sequence" in {
    val data = DSequence(Some(FullName("!!!!!", "!!!!!")),
        DInt(15) ::
        DInt(-1) ::
        DInt(-1) ::
        DAssortment(Assortment[Token, Token]() +
            (DInt(15), DInt(-1)) +
            DInt(-1)) :: Nil)
    val expected = Array[Byte](sequenceByte, classNameByte,
        (fixnsMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        (fixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        0x0f.toByte,
        0xff.toByte,
        0xff.toByte,
        assortmentByte,
        // key
        0x0f.toByte,
        // value
        0xff.toByte,
        // key without value
        0xff.toByte,
        collectionEndByte,
        collectionEndByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected
  }

  it should "allow an empty object (collection of qualified name/value pairs)" in {
    val data = DObject(None)
    val expected = Array[Byte](objectByte, collectionEndByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected
  }

  it should "allow a non-empty object" in {
    val data = DObject(None, Assortment[QualifiedName, Token]() + (Name("!!!!!"), DBin(List(33, 33, 33, 33, 33))))
    val expected = Array[Byte](objectByte,
      (fixbinMask | 0x05).toByte,
      0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
      (fixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
      collectionEndByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected
  }

  it should "allow a classed (local name only) empty object" in {
    val data = DObject(Some(Name("")))
    val expected = Array[Byte](objectByte, classNameByte, (fixbinMask | 0x0).toByte, collectionEndByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected
  }
}

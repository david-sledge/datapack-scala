package scala.data.pack

import org.scalatest._
import scala.data.Assortment
import java.io.ByteArrayInputStream

import FormatBytes._
import ObjectReader._

class ObjectReaderSpec extends FlatSpec with Matchers {
  "The object reader" should "produce None with an empty input stream" in {
    val input = Array[Byte]()
    val expected = None
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read a Nil" in {
    val input = Array[Byte](nilByte)
    val expected = Some(DNil)
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read a Boolean false" in {
    val input = Array[Byte](falseByte)
    val expected = Some(DFalse)
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read a Boolean true" in {
    val input = Array[Byte](trueByte)
    val expected = Some(DTrue)
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read a masked non-negative integer" in {
    val input = Array[Byte](0x0f.toByte)
    val expected = Some(DInt(15))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read a masked non-positive integer" in {
    val input = Array[Byte](0xff.toByte)
    val expected = Some(DInt(-1))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read a one-byte signed integer" in {
    val input = Array[Byte](int8Byte, 0xff.toByte)
    val expected = Some(DInt(-1))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read a two-byte signed integer" in {
    val input = Array[Byte](int16Byte, 0xff.toByte, 0xff.toByte)
    val expected = Some(DInt(-1))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read a four-byte signed integer" in {
    val input = Array[Byte](int32Byte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte)
    val expected = Some(DInt(-1))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read an eight-byte signed integer" in {
    val input = Array[Byte](int64Byte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte)
    val expected = Some(DInt(-1))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read a four-byte floating point decimal" in {
    val input = Array[Byte](float32Byte, 0xc0.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte)
    val expected = Some(DFloat(-2f))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read an eight-byte floating point decimal" in {
    val input = Array[Byte](float64Byte, 0xc0.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte)
    val expected = Some(DDouble(-2))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read brief binary data" in {
    val input = Array[Byte]((fixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte)
    val expected = Some(DBin(List[Byte](0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte)))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read (seemingly) not-so-brief binary data" in {
    val input = Array[Byte](bin8Byte, 5.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte)
    val expected = Some(DBin(List[Byte](0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte)))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow an empty assortment" in {
    val input = Array[Byte](assortmentByte, collectionEndByte)
    val expected = Some(DAssortment(Assortment()))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow a non-empty assortment" in {
    val input = Array[Byte](
        assortmentByte,
        // key
        0x0f.toByte,
        // value
        0xff.toByte,
        // key without value
        0xff.toByte,
        collectionEndByte)
    val expected = Some(DAssortment(Assortment[Token, Token]() + (DInt(15), DInt(-1)) + DInt(-1)))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow an unclassed empty sequence" in {
    val input = Array[Byte](sequenceByte, collectionEndByte)
    val expected = Some(DSequence(None, List()))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow an unclassed non-empty sequence" in {
    val input = Array[Byte](
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
    val expected = Some(DSequence(None,
        DInt(15) ::
        DInt(-1) ::
        DInt(-1) ::
        DAssortment(Assortment[Token, Token]() +
            (DInt(15), DInt(-1)) +
            DInt(-1)) :: Nil))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow a classed (local name only) empty sequence" in {
    val input = Array[Byte](sequenceByte, classNameByte,
        (fixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        collectionEndByte)
    val expected = Some(DSequence(Some(Name("!!!!!"))))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow a classed (fully qualified name) empty sequence" in {
    val input = Array[Byte](sequenceByte, classNameByte,
        (fixnsMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        (fixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        collectionEndByte)
    val expected = Some(DSequence(Some(FullName("!!!!!", "!!!!!"))))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow a classed (local name only) non-empty sequence" in {
    val input = Array[Byte](sequenceByte, classNameByte,
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
    val expected = Some(DSequence(Some(Name("!!!!!")),
        DInt(15) ::
        DInt(-1) ::
        DInt(-1) ::
        DAssortment(Assortment[Token, Token]() +
            (DInt(15), DInt(-1)) +
            DInt(-1)) :: Nil))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow a classed (fully qualified name) non-empty sequence" in {
    val input = Array[Byte](sequenceByte, classNameByte,
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
    val expected = Some(DSequence(Some(FullName("!!!!!", "!!!!!")),
        DInt(15) ::
        DInt(-1) ::
        DInt(-1) ::
        DAssortment(Assortment[Token, Token]() +
            (DInt(15), DInt(-1)) +
            DInt(-1)) :: Nil))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow an empty object (collection of qualified name/value pairs)" in {
    val input = Array[Byte](objectByte, collectionEndByte)
    val expected = Some(DObject(None))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow a non-empty object" in {
    val input = Array[Byte](objectByte, (fixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, (fixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, collectionEndByte)
    val expected = Some(DObject(None, Assortment[QualifiedName, Token]() + (Name("!!!!!"), DBin(List(33, 33, 33, 33, 33)))))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow a classed (local name only) empty object" in {
    val input = Array[Byte](objectByte, classNameByte, (fixbinMask | 0x0).toByte, collectionEndByte)
    val expected = Some(DObject(Some(Name(""))))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }
}

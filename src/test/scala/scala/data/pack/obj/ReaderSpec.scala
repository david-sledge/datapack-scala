package scala.data.pack.obj

import org.scalatest._
import scala.data.Assortment
import java.io.ByteArrayInputStream

import scala.data.pack.FormatBytes._
import Reader._
import java.math.BigInteger

class ReaderSpec extends FlatSpec with Matchers {
  "The object reader" should "produce None with an empty input stream" in {
    val input = Array[Byte]()
    val expected = None
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read a Nil" in {
    val input = Array[Byte](NilByte)
    val expected = Some(DNil)
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read a Boolean false" in {
    val input = Array[Byte](FalseByte)
    val expected = Some(DFalse)
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read a Boolean true" in {
    val input = Array[Byte](TrueByte)
    val expected = Some(DTrue)
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read a masked non-negative integer" in {
    val input = Array[Byte](0x0f.toByte)
    val expected = Some(toDInt(15))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read a masked non-positive integer" in {
    val input = Array[Byte](0xff.toByte)
    val expected = Some(toDInt(-1))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read a one-byte signed integer" in {
    val input = Array[Byte](Int8Byte, 0xff.toByte)
    val expected = Some(toDInt(-1))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read a two-byte signed integer" in {
    val input = Array[Byte](Int16Byte, 0xff.toByte, 0xff.toByte)
    val expected = Some(toDInt(-1))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read a four-byte signed integer" in {
    val input = Array[Byte](Int32Byte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte)
    val expected = Some(toDInt(-1))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read an eight-byte signed integer" in {
    val input = Array[Byte](Int64Byte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte)
    val expected = Some(toDInt(-1))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read a four-byte floating point decimal" in {
    val input = Array[Byte](Float32Byte, 0xc0.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte)
    val expected = Some(DFloat(-2f))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read an eight-byte floating point decimal" in {
    val input = Array[Byte](Float64Byte, 0xc0.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte)
    val expected = Some(DDouble(-2))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read brief binary data" in {
    val input = Array[Byte]((FixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte)
    val expected = Some(DBin(List[Byte](0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte)))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "read (seemingly) not-so-brief binary data" in {
    val input = Array[Byte](Bin8Byte, 5.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte)
    val expected = Some(DBin(List[Byte](0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte)))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow an empty assortment" in {
    val input = Array[Byte](AssortmentByte, CollectionEndByte)
    val expected = Some(DAssortment(Assortment()))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow a non-empty assortment" in {
    val input = Array[Byte](
        AssortmentByte,
        // key
        0x0f.toByte,
        // value
        0xff.toByte,
        // key without value
        0xff.toByte,
        CollectionEndByte)
    val expected = Some(DAssortment(Assortment[Token, Token]() + (toDInt(15), toDInt(-1)) + toDInt(-1)))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow an unclassed empty sequence" in {
    val input = Array[Byte](SequenceByte, CollectionEndByte)
    val expected = Some(DSequence(None, List()))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow an unclassed non-empty sequence" in {
    val input = Array[Byte](
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
        CollectionEndByte)
    val expected = Some(DSequence(None,
        toDInt(15) ::
        toDInt(-1) ::
        toDInt(-1) ::
        DAssortment(Assortment[Token, Token]() +
            (toDInt(15), toDInt(-1)) +
            toDInt(-1)) :: Nil))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow a classed (local name only) empty sequence" in {
    val input = Array[Byte](SequenceByte, ClassNameByte,
        (FixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        CollectionEndByte)
    val expected = Some(DSequence(Some(Name("!!!!!"))))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow a classed (fully qualified name) empty sequence" in {
    val input = Array[Byte](SequenceByte, ClassNameByte,
        (FixnsMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        (FixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        CollectionEndByte)
    val expected = Some(DSequence(Some(FullName("!!!!!", "!!!!!"))))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow a classed (local name only) non-empty sequence" in {
    val input = Array[Byte](SequenceByte, ClassNameByte,
        (FixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
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
        CollectionEndByte)
    val expected = Some(DSequence(Some(Name("!!!!!")),
        toDInt(15) ::
        toDInt(-1) ::
        toDInt(-1) ::
        DAssortment(Assortment[Token, Token]() +
            (toDInt(15), toDInt(-1)) +
            toDInt(-1)) :: Nil))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow a classed (fully qualified name) non-empty sequence" in {
    val input = Array[Byte](SequenceByte, ClassNameByte,
        (FixnsMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        (FixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
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
        CollectionEndByte)
    val expected = Some(DSequence(Some(FullName("!!!!!", "!!!!!")),
        toDInt(15) ::
        toDInt(-1) ::
        toDInt(-1) ::
        DAssortment(Assortment[Token, Token]() +
            (toDInt(15), toDInt(-1)) +
            toDInt(-1)) :: Nil))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow an empty object (collection of qualified name/value pairs)" in {
    val input = Array[Byte](ObjectByte, CollectionEndByte)
    val expected = Some(DObject(None))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow a non-empty object" in {
    val input = Array[Byte](ObjectByte, (FixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, (FixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, CollectionEndByte)
    val expected = Some(DObject(None, Assortment[QualifiedName, Token]() + (Name("!!!!!"), DBin(List(33, 33, 33, 33, 33)))))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow a classed (local name only) empty object" in {
    val input = Array[Byte](ObjectByte, ClassNameByte, (FixbinMask | 0x0).toByte, CollectionEndByte)
    val expected = Some(DObject(Some(Name(""))))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }
}

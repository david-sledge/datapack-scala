package scala.data.pack.stream

import org.scalatest._
import java.io.ByteArrayInputStream
import java.io.EOFException

import scala.data.pack.FormatBytes._
import Reader._

class ReaderSpec extends FlatSpec with Matchers {
  "A data pack reader" should "read" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte]()), (_) => ())
    reader.readValue
    reader.position shouldBe 0
  }

  it should "read a Nil" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](NilByte)),
        (_ shouldBe TNil))
    reader.readValue
    reader.position shouldBe 1
  }

  it should "throw an exception when a format byte is read in an invalid state" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](CollectionEndByte)),
        (_ shouldBe TNil))
    an [ReadStateException] should be thrownBy reader.readValue
    reader.position shouldBe 1
  }

  it should "read a Boolean false" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](FalseByte)),
        (_ shouldBe TFalse))
    reader.readValue
    reader.position shouldBe 1
  }

  it should "read a Boolean true" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](TrueByte)),
        (_ shouldBe TTrue))
    reader.readValue
    reader.position shouldBe 1
  }

  it should "read a masked non-negative integer" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](0x0f.toByte)),
        (_ shouldBe toTInt(15)))
    reader.readValue
    reader.position shouldBe 1
  }

  it should "read a masked non-positive integer" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](0xff.toByte)),
        (_ shouldBe toTInt(-1)))
    reader.readValue
    reader.position shouldBe 1
  }

  it should "read a one-byte signed integer" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](Int8Byte, 0xff.toByte)),
        (_ shouldBe toTInt(0xffffffff)))
    reader.readValue
    reader.position shouldBe 2
  }

  it should "read a two-byte signed integer" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](Int16Byte, 0xff.toByte, 0xff.toByte)),
        (_ shouldBe toTInt(0xffffffff)))
    reader.readValue
    reader.position shouldBe 3
  }

  it should "read a four-byte signed integer" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](Int32Byte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte)),
        (_ shouldBe toTInt(0xffffffff)))
    reader.readValue
    reader.position shouldBe 5
  }

  it should "read an eight-byte signed integer" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](Int64Byte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte)),
        (_ shouldBe toTInt(0xffffffffffffffffl)))
    reader.readValue
    reader.position shouldBe 9
  }

  it should "read a one-byte unsigned integer" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](Uint8Byte, 0xff.toByte)),
        (_ shouldBe toTInt(0x000000ff)))
    reader.readValue
    reader.position shouldBe 2
  }

  it should "read a two-byte unsigned integer" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](Uint16Byte, 0xff.toByte, 0xff.toByte)),
        (_ shouldBe toTInt(0x0000ffff)))
    reader.readValue
    reader.position shouldBe 3
  }

  it should "read a four-byte unsigned integer" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](Uint32Byte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte)),
        (_ shouldBe toTInt(0x00000000ffffffffL)))
    reader.readValue
    reader.position shouldBe 5
  }

  it should "read an eight-byte unsigned integer" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](Uint64Byte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte)),
        (_ shouldBe TInt(new java.math.BigInteger(1,
            Array[Byte](-1, -1, -1, -1, -1, -1, -1, -1)
            ))))
    reader.readValue
    reader.position shouldBe 9
  }
  /*
 *  */
  it should "read a four-byte floating point decimal" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](Float32Byte, 0xc0.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte)),
        (_ shouldBe TFloat(-2f)))
    reader.readValue
    reader.position shouldBe 5
  }

  it should "read an eight-byte floating point decimal" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](Float64Byte, 0xc0.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte)),
        (_ shouldBe TDouble(-2)))
    reader.readValue
    reader.position shouldBe 9
  }

  it should "read brief binary data" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte]((FixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte)),
        (_ shouldBe TBin(List[Byte](0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte))))
    reader.readValue
    reader.position shouldBe 6
  }

  it should "read (seemingly) not-so-brief binary data" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](Bin8Byte, 5.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte)),
        (_ shouldBe TBin(List[Byte](0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte))))
    reader.readValue
    reader.position shouldBe 7
  }

  it should "not allow namespaces outside of class names and property names" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte]((FixnsMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte)),
        (_ shouldBe TBin(List[Byte](0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte))))
    an [ReadStateException] should be thrownBy reader.readValue
    reader.position shouldBe 1
  }

  it should "not allow class names outside of the beginnings of objects and arrays" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](ClassNameByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte)),
        (_ shouldBe TBin(List[Byte](0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte))))
    an [ReadStateException] should be thrownBy reader.readValue
    reader.position shouldBe 1
  }

  it should "not allow no key/value bytes outside of assortments and objects" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](NoKeyValueByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte)),
        (_ shouldBe TBin(List[Byte](0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte))))
    an [ReadStateException] should be thrownBy reader.readValue
    reader.position shouldBe 1
  }

  it should "allow an empty assortment" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](AssortmentByte, CollectionEndByte)),
        (_) => ())
    reader.readValue
    reader.readValue
    reader.position shouldBe 2
  }

  it should "allow a non-empty assortment" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](
        AssortmentByte,
        // key
        0x0f.toByte,
        // value
        0xff.toByte,
        // key without value
        0xff.toByte,
        CollectionEndByte)),
      (_) => ())
    reader.readValue
    reader.readValue
    reader.readValue
    reader.readValue
    reader.readValue
    reader.position shouldBe 5
  } 

  it should "allow an empty set of objects (collection of qualified name/value pairs)" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](ObjectByte, CollectionEndByte)),
        (_) => ())
    reader.readValue
    reader.readValue
    reader.position shouldBe 2
  }

  it should "allow a non-empty set of objects" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](ObjectByte, (FixstrMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, (FixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, CollectionEndByte)),
        (_) => ())
    reader.readValue
    reader.readValue
    reader.readValue
    reader.readValue
    reader.position shouldBe 14
  }

  it should "allow a classed empty set of objects" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](ObjectByte, ClassNameByte, (FixstrMask | 0x0).toByte, CollectionEndByte)),
        (_) => ())
    reader.readValue
    reader.readValue
    reader.readValue
    reader.readValue
    reader.position shouldBe 4
  }
}

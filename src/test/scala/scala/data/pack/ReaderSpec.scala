package scala.data.pack

import org.scalatest._
import java.io.ByteArrayInputStream
import java.io.EOFException

import FormatBytes._
import Reader._

class ReaderSpec extends FlatSpec with Matchers {
  "A data pack reader" should "read" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte]()), (_) => ())
    noException should be thrownBy reader.readValue
    reader.position shouldBe 0
  }

  it should "read a Nil" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](nilByte)),
        (_ shouldBe TNil))
    noException should be thrownBy reader.readValue
    reader.position shouldBe 1
  }

  it should "throw an exception when a format byte is read in an invalid state" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](collectionEndByte)),
        (_ shouldBe TNil))
    an [ReadStateException] should be thrownBy reader.readValue
    reader.position shouldBe 1
  }

  it should "read a Boolean false" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](falseByte)),
        (_ shouldBe TFalse))
    noException should be thrownBy reader.readValue
    reader.position shouldBe 1
  }

  it should "read a Boolean true" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](trueByte)),
        (_ shouldBe TTrue))
    noException should be thrownBy reader.readValue
    reader.position shouldBe 1
  }

  it should "read a masked non-negative integer" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](0x0f.toByte)),
        (_ shouldBe TInt(15)))
    noException should be thrownBy reader.readValue
    reader.position shouldBe 1
  }

  it should "read a masked non-positive integer" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](0xff.toByte)),
        (_ shouldBe TInt(-1)))
    noException should be thrownBy reader.readValue
    reader.position shouldBe 1
  }

  it should "read a one-byte signed integer" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](int8Byte, 0xff.toByte)),
        (_ shouldBe TInt(0xffffffff)))
    noException should be thrownBy reader.readValue
    reader.position shouldBe 2
  }

  it should "read a two-byte signed integer" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](int16Byte, 0xff.toByte, 0xff.toByte)),
        (_ shouldBe TInt(0xffffffff)))
    noException should be thrownBy reader.readValue
    reader.position shouldBe 3
  }

  it should "read a four-byte signed integer" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](int32Byte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte)),
        (_ shouldBe TInt(0xffffffff)))
    noException should be thrownBy reader.readValue
    reader.position shouldBe 5
  }

  it should "read an eight-byte signed integer" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](int64Byte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte)),
        (_ shouldBe TInt(0xffffffffffffffffl)))
    noException should be thrownBy reader.readValue
    reader.position shouldBe 9
  }

  it should "read a four-byte floating point decimal" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](float32Byte, 0xc0.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte)),
        (_ shouldBe TFloat(-2f)))
    noException should be thrownBy reader.readValue
    reader.position shouldBe 5
  }

  it should "read an eight-byte floating point decimal" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](float64Byte, 0xc0.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte)),
        (_ shouldBe TDouble(-2)))
    noException should be thrownBy reader.readValue
    reader.position shouldBe 9
  }

  it should "read brief binary data" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte]((fixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte)),
        (_ shouldBe TBin(List[Byte](0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte))))
    noException should be thrownBy reader.readValue
    reader.position shouldBe 6
  }

  it should "read (seemingly) not-so-brief binary data" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](bin8Byte, 5.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte)),
        (_ shouldBe TBin(List[Byte](0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte))))
    noException should be thrownBy reader.readValue
    reader.position shouldBe 7
  }

  it should "not allow namespaces outside of class names and property names" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte]((fixnsMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte)),
        (_ shouldBe TBin(List[Byte](0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte))))
    an [ReadStateException] should be thrownBy reader.readValue
    reader.position shouldBe 1
  }

  it should "not allow class names outside of the beginnings of objects and arrays" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](classNameByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte)),
        (_ shouldBe TBin(List[Byte](0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte))))
    an [ReadStateException] should be thrownBy reader.readValue
    reader.position shouldBe 1
  }

  it should "not allow no key/value bytes outside of assortments and objects" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](noKeyValueByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte)),
        (_ shouldBe TBin(List[Byte](0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte))))
    an [ReadStateException] should be thrownBy reader.readValue
    reader.position shouldBe 1
  }

  it should "allow an empty assortment" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](assortmentByte, collectionEndByte)),
        (_) => ())
    noException should be thrownBy reader.readValue
    noException should be thrownBy reader.readValue
    reader.position shouldBe 2
  }

  it should "allow a non-empty assortment" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](
        assortmentByte,
        // key
        0x0f.toByte,
        // value
        0xff.toByte,
        // key without value
        0xff.toByte,
        collectionEndByte)),
      (_) => ())
    noException should be thrownBy reader.readValue
    noException should be thrownBy reader.readValue
    noException should be thrownBy reader.readValue
    noException should be thrownBy reader.readValue
    noException should be thrownBy reader.readValue
    reader.position shouldBe 5
  } 

  it should "allow an empty set of objects (collection of qualified name/value pairs)" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](objectByte, collectionEndByte)),
        (_) => ())
    noException should be thrownBy reader.readValue
    noException should be thrownBy reader.readValue
    reader.position shouldBe 2
  }

  it should "allow a non-empty set of objects" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](objectByte, (fixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, (fixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, collectionEndByte)),
        (_) => ())
    noException should be thrownBy reader.readValue
    noException should be thrownBy reader.readValue
    noException should be thrownBy reader.readValue
    noException should be thrownBy reader.readValue
    reader.position shouldBe 14
  }

  it should "allow a classed empty set of objects" in {
    val reader = new Reader(new ByteArrayInputStream(Array[Byte](objectByte, classNameByte, (fixbinMask | 0x0).toByte, collectionEndByte)),
        (_) => ())
    noException should be thrownBy reader.readValue
    noException should be thrownBy reader.readValue
    noException should be thrownBy reader.readValue
    noException should be thrownBy reader.readValue
    reader.position shouldBe 4
  }
}

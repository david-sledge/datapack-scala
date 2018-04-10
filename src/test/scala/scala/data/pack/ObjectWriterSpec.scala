package scala.data.pack

import org.scalatest._
import scala.data.Assortment
import scala.data.Assortment._
import java.io.OutputStream
import java.io.ByteArrayOutputStream

import FormatBytes._
import ObjectWriter._

import Writer._

object ObjectWriter {
  private def packChildren(children: List[Token], writer: Writer): Unit =
    children match {
      case token :: tail => {
        _pack(token, writer)
        packChildren(tail, writer)
      }
      case Nil => ()
    }

  private def packElements(elements: List[Element[Token, Token]], writer: Writer): Unit = {
    import writer._

    elements match {
    case element :: tail => {
      element match {
        case Assortment.Entry(key) => {
          _pack(key, writer)
          writeNoKeyValue
        }
        case Mapping(key, value) => {
          _pack(key, writer)
          _pack(value, writer)
        }
        case Item(value) => {
          writeNoKeyValue
          _pack(value, writer)
        }
      }

      packElements(tail, writer)
    }
    case Nil => ()
  }}

  private def _pack(token: Token, writer: Writer): Unit = {
    import writer._

    token match {
      case DNil => writeNil
      case DFalse => writeFalse
      case DTrue => writeTrue
      case DInt(int) => writeInt(int)
      case DFloat(float) => writeFloat(float)
      case DDouble(float) => writeDouble(float)
      case DBin(list) => writeBin(list.toArray)
      case DSequence(className, list) => {
        writeSequenceStart(className match {
          case None => None
          case Some(Name(name)) => Some((None, name))
          case Some(FullName(namespaceName, localName)) => Some(Some(namespaceName), localName)
        })
        packChildren(list, writer)
      }
      case DAssortment(assortment) => packElements(assortment.insertOrder, writer)
    }
  }

  def pack(token: Token, os: OutputStream) = _pack(token, new Writer(os))
}

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

/*
  it should "write a one-byte signed integer" in {
    val data = DInt(15)
    val expected = Array[Byte](0x0f.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected

    val input = Array[Byte](int8Byte, 0xff.toByte)
    val expected = Some(DInt(-1))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "write a two-byte signed integer" in {
    val data = DInt(15)
    val expected = Array[Byte](0x0f.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected

    val input = Array[Byte](int16Byte, 0xff.toByte, 0xff.toByte)
    val expected = Some(DInt(-1))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "write a four-byte signed integer" in {
    val data = DInt(15)
    val expected = Array[Byte](0x0f.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected

    val input = Array[Byte](int32Byte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte)
    val expected = Some(DInt(-1))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "write an eight-byte signed integer" in {
    val data = DInt(15)
    val expected = Array[Byte](0x0f.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected

    val input = Array[Byte](int64Byte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte)
    val expected = Some(DInt(-1))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "write a four-byte floating point decimal" in {
    val data = DInt(15)
    val expected = Array[Byte](0x0f.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected

    val input = Array[Byte](float32Byte, 0xc0.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte)
    val expected = Some(DFloat(-2f))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "write an eight-byte floating point decimal" in {
    val data = DInt(15)
    val expected = Array[Byte](0x0f.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected

    val input = Array[Byte](float64Byte, 0xc0.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte)
    val expected = Some(DDouble(-2))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "write brief binary data" in {
    val data = DInt(15)
    val expected = Array[Byte](0x0f.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected

    val input = Array[Byte]((fixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte)
    val expected = Some(DBin(List[Byte](0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte)))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "write (seemingly) not-so-brief binary data" in {
    val data = DInt(15)
    val expected = Array[Byte](0x0f.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected

    val input = Array[Byte](bin8Byte, 5.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte)
    val expected = Some(DBin(List[Byte](0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte)))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow an empty assortment" in {
    val data = DInt(15)
    val expected = Array[Byte](0x0f.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected

    val input = Array[Byte](assortmentByte, collectionEndByte)
    val expected = Some(DAssortment(Assortment()))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow a non-empty assortment" in {
    val data = DInt(15)
    val expected = Array[Byte](0x0f.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected

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
    val data = DInt(15)
    val expected = Array[Byte](0x0f.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected

    val input = Array[Byte](sequenceByte, collectionEndByte)
    val expected = Some(DSequence(None, List()))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow an unclassed non-empty sequence" in {
    val data = DInt(15)
    val expected = Array[Byte](0x0f.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected

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
    val data = DInt(15)
    val expected = Array[Byte](0x0f.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected

    val input = Array[Byte](sequenceByte, classNameByte,
        (fixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        collectionEndByte)
    val expected = Some(DSequence(Some(Name("!!!!!"))))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow a classed (fully qualified name) empty sequence" in {
    val data = DInt(15)
    val expected = Array[Byte](0x0f.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected

    val input = Array[Byte](sequenceByte, classNameByte,
        (fixnsMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        (fixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte,
        collectionEndByte)
    val expected = Some(DSequence(Some(FullName("!!!!!", "!!!!!"))))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow a classed (local name only) non-empty sequence" in {
    val data = DInt(15)
    val expected = Array[Byte](0x0f.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected

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
    val data = DInt(15)
    val expected = Array[Byte](0x0f.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected

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
    val data = DInt(15)
    val expected = Array[Byte](0x0f.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected

    val input = Array[Byte](objectByte, collectionEndByte)
    val expected = Some(DObject(None))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow a non-empty object" in {
    val data = DInt(15)
    val expected = Array[Byte](0x0f.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected

    val input = Array[Byte](objectByte, (fixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, (fixbinMask | 0x05).toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, 0x21.toByte, collectionEndByte)
    val expected = Some(DObject(None, Assortment[QualifiedName, Token]() + (Name("!!!!!"), DBin(List(33, 33, 33, 33, 33)))))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }

  it should "allow a classed (local name only) empty object" in {
    val data = DInt(15)
    val expected = Array[Byte](0x0f.toByte)
    val output = new ByteArrayOutputStream
    pack(data, output)
    output.toByteArray shouldBe expected

    val input = Array[Byte](objectByte, classNameByte, (fixbinMask | 0x0).toByte, collectionEndByte)
    val expected = Some(DObject(Some(Name(""))))
    unpack(new ByteArrayInputStream(input)) shouldBe expected
  }
*/
}

package sandbox

import org.scalatest._
import scala.meta._
import scala.util._
import scala.collection.concurrent.TrieMap
import java.nio.ByteBuffer

class Spec extends FlatSpec with Matchers {
  "Tests" should "have a sandbox area to play around with code" in {
    val byte = 0xff.toByte
    val int = byte
    byte shouldBe int
    println(byte, int)

    println(0xff)
    println(0xff.toByte)
    val buffer16 = Array[Byte](0xff.toByte, 0xff.toByte)
    0x0000ffff shouldBe (((buffer16(0) & 0xFF) << 8) | buffer16(1) & 0x00FF)

    val buffer32 = Array[Byte](0x80.toByte, 0xff.toByte, 0xff.toByte, 0x80.toByte)
    0x0000000080ffff80 shouldBe (
        ((buffer32(0) & 0xFF) << 24)
        | ((buffer32(1) & 0xFF) << 16)
        | ((buffer32(2) & 0xFF) << 8)
        | buffer32(3) & 0x00FF
      )

    0x7FFFFFFFFFFFFFFFL.toHexString shouldBe "7fffffffffffffff"
  }
}

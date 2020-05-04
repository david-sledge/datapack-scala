package scala.data

import java.math.BigInteger

package object pack {

  val MaxUint8: Short = 0xff
  val MaxUint16: Int = 0xffff
  val MaxUint32: Long = 0xffffffffL
  val MaxUint64: BigInteger = new BigInteger(1,
      Array[Byte](
          -1, -1, -1, -1,
          -1, -1, -1, -1
        )
    )
}

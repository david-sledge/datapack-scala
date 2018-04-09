package scala.data.pack

class IntTooLargeException (val int: Long)
    extends Exception(s"Value 0x${int.toHexString} is too large as an unsigned 64-bit integer.", null)
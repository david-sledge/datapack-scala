package scala.data.pack.stream

class UnusedFormatByteException (val formatByte: Byte)
    extends Exception(s"Value 0x${formatByte.toHexString} is unused as a format.")

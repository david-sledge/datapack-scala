package scala.data.pack.stream

final class Read[S](
  readByte: S => Int,
  read: (S, Array[Byte], Int, Int) => Int,
) {

  def readByte(s: S): Int = readByte.apply(s)

  def read(s: S, buf: Array[Byte], off: Int, len: Int): Int = read.apply(s, buf, off, len)

}

object Read {

  def apply[S](
    readByte: S => Int,
    read: (S, Array[Byte], Int, Int) => Int,
  ) = new Read(readByte, read)

  def readByte[S](s: S)(implicit r: Read[S]) = r.readByte(s)

  def read[S](s: S, buf: Array[Byte], off: Int, len: Int)(implicit r: Read[S]) =
    r.read(s, buf, off, len)

}

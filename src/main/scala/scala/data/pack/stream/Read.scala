package scala.data.pack.stream

trait Read[S] {

  def readByte(s: S): Int

  def read(s: S, buf: Array[Byte], off: Int, len: Int): Int

}

object Read {

  def readByte[S](s: S)(implicit r: Read[S]) = r.readByte(s)

  def read[S](s: S, buf: Array[Byte], off: Int, len: Int)(implicit r: Read[S]) =
    r.read(s, buf, off, len)

}

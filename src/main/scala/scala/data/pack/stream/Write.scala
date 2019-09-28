package scala.data.pack.stream

trait Write[D] {

  def writeByte(d: D, byte: Int): Unit

  def write(d: D, bytes: Array[Byte]): Unit

}

object Write {

  def writeByte[D](d: D, byte: Int)(implicit w: Write[D]) = w.writeByte(d, byte)

  def write[D](d: D, bytes: Array[Byte])(implicit w: Write[D]) = w.write(d, bytes)

}

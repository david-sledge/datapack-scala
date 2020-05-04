package scala.data.pack.stream

final class Write[D](
  writeByte: (D, Int) => Unit,
  write: (D, Array[Byte]) => Unit,
) {

  def writeByte(d: D, byte: Int): Unit = writeByte.apply(d, byte)

  def write(d: D, bytes: Array[Byte]): Unit = write.apply(d, bytes)

}

object Write {

  def apply[D](
    writeByte: (D, Int) => Unit,
    write: (D, Array[Byte]) => Unit,
  ) = new Write(writeByte, write)

  def writeByte[D](d: D, byte: Int)(implicit w: Write[D]) = w.writeByte(d, byte)

  def write[D](d: D, bytes: Array[Byte])(implicit w: Write[D]) = w.write(d, bytes)

}

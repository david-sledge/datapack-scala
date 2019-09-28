package scala.data.pack.stream

import java.io.InputStream
import java.io.OutputStream

package object io {

  implicit object InputStreamRead extends Read[InputStream] {

    override def readByte(s: InputStream) = s.read()

    override def read(s: InputStream, buf: Array[Byte], off: Int, len: Int) = s.read(buf, off, len)

  }

  implicit object OutputStreamWrite extends Write[OutputStream] {

    override def writeByte(d: OutputStream, byte: Int) = d.write(byte)

    override def write(d: OutputStream, buf: Array[Byte]) = d.write(buf)

  }

}

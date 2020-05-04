package scala.data.pack.stream

import java.io.InputStream
import java.io.OutputStream

package object io {

  implicit val InputStreamRead = Read[InputStream](
    s => s.read(),
    (s, buf, off, len) => s.read(buf, off, len),
  )

  implicit val OutputStreamWrite = Write[OutputStream](
    (d, byte) => d.write(byte),
    (d, buf) => d.write(buf),
  )

}

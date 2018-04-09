package scala.data.pack

import Reader._

// TODO:  more descriptive message
final case class ReadStateException(
    val formatByte: Byte,
    val state: State
  )
    extends Exception("Invalid state for the given format type", null)

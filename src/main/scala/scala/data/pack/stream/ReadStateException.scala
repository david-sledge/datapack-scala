package scala.data.pack.stream

// TODO:  more descriptive message
final case class ReadStateException(
  val formatByte: Byte,
  val state: Reader.State
)
  extends Exception(s"Invalid state $state for the given format type $formatByte")

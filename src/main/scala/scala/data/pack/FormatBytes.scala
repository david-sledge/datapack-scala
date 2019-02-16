package scala.data.pack

object FormatBytes {
  val FixintMask: Byte        = 0xc0.toByte
  val NilByte: Byte           = 0x40.toByte
  val CollectionEndByte: Byte = 0x41.toByte
  val FalseByte: Byte         = 0x42.toByte
  val TrueByte: Byte          = 0x43.toByte
  val Uint8Byte: Byte         = 0x44.toByte
  val Uint16Byte: Byte        = 0x45.toByte
  val Uint32Byte: Byte        = 0x46.toByte
  val Uint64Byte: Byte        = 0x47.toByte
  val Int8Byte: Byte          = 0x48.toByte
  val Int16Byte: Byte         = 0x49.toByte
  val Int32Byte: Byte         = 0x4a.toByte
  val Int64Byte: Byte         = 0x4b.toByte
  val Float32Byte: Byte       = 0x4c.toByte
  val Float64Byte: Byte       = 0x4d.toByte
  val Bin8Byte: Byte          = 0x4e.toByte
  val Bin16Byte: Byte         = 0x4f.toByte
  val Bin32Byte: Byte         = 0x50.toByte
  val Str8Byte: Byte          = 0x51.toByte
  val Str16Byte: Byte         = 0x52.toByte
  val Str32Byte: Byte         = 0x53.toByte
  val Ns8Byte: Byte           = 0x54.toByte
  val Ns16Byte: Byte          = 0x55.toByte
  val Ns32Byte: Byte          = 0x56.toByte
  val ClassNameByte: Byte     = 0x57.toByte
  val SequenceByte: Byte      = 0x58.toByte
  val AssortmentByte: Byte    = 0x59.toByte
  val ObjectByte: Byte        = 0x5a.toByte
  val NoKeyValueByte: Byte    = 0x5b.toByte
  //unusedBytes = [0x5c..0x5f]
  val FixbinMask: Byte        = 0x60.toByte
  val FixstrMask: Byte        = 0x80.toByte
  val FixnsMask: Byte         = 0xa0.toByte
  val FixMask: Byte           = 0xe0.toByte
  val LenMask: Byte           = 0x1f.toByte
}

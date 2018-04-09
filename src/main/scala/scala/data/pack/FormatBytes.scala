package scala.data.pack

object FormatBytes {
  val fixintMask: Byte        = 0xc0.toByte
  val nilByte: Byte           = 0x40.toByte
  val collectionEndByte: Byte = 0x41.toByte
  val falseByte: Byte         = 0x42.toByte
  val trueByte: Byte          = 0x43.toByte
  val int8Byte: Byte          = 0x44.toByte
  val int16Byte: Byte         = 0x45.toByte
  val int32Byte: Byte         = 0x46.toByte
  val int64Byte: Byte         = 0x47.toByte
  val float32Byte: Byte       = 0x48.toByte
  val float64Byte: Byte       = 0x49.toByte
  val bin8Byte: Byte          = 0x4a.toByte
  val bin16Byte: Byte         = 0x4b.toByte
  val bin32Byte: Byte         = 0x4c.toByte
  val ns8Byte: Byte           = 0x4d.toByte
  val ns16Byte: Byte          = 0x4e.toByte
  val ns32Byte: Byte          = 0x4f.toByte
  val classNameByte: Byte     = 0x50.toByte
  val sequenceByte: Byte      = 0x51.toByte
  val assortmentByte: Byte    = 0x52.toByte
  val objectByte: Byte        = 0x53.toByte
  val noKeyValueByte: Byte    = 0x54.toByte
  //unusedBytes = [0x55..0x7f]
  val fixbinMask: Byte        = 0x80.toByte
  val fixnsMask: Byte         = 0xa0.toByte
  val fixMask: Byte           = 0xe0.toByte
  val lenMask: Byte           = 0x1f.toByte
}

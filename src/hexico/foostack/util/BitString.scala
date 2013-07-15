package hexico.foostack.util

case class BitString(val value: Int, val width: Int) {
  val mask: Int = BitString.createMask(width)

  if (width < 1 || width > 32) throw new IllegalArgumentException("width outside range [1, 32]")
  if ((~mask & value) != 0) throw new IllegalArgumentException("value wider than width")

  def this(bin: String) = this(Integer.parseInt(bin, 0), bin.length)

  def ++(other: BitString) = new BitString((value << other.width) | other.value, width + other.width)

  def splitAt(pos: Int): (BitString, BitString) = {
    if (pos < 1 || pos > width - 1)
      throw new IllegalArgumentException("pos outside range [0, width - 1]")
    val bs1 = new BitString(value >> (width - pos), pos)
    val newWidth = width - pos
    val bs2 = new BitString(value & ((1 << newWidth) - 1), newWidth)
    (bs1, bs2)
  }

  override def toString() = {
    val bin = value.toBinaryString
    "BitString(" + "0" * (width - bin.length) + bin + ")"
  }
}

object BitString {
  def createMask(width: Int): Int = (1 << width) - 1
}
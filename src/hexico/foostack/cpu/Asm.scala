package hexico.foostack.cpu

import hexico.foostack.util.BitString

object Asm {
  type Word = Short
  type LongWord = Int
  type QuadWord = Long

  class BitStringEnumeration(val width: Int) extends Enumeration {
    class Val extends super.Val {
      def toBitString = BitString(id, width)
    }

    def apply(bits: BitString): Value = apply(bits.value)
  }

  /**
   * General-purpose data registers.
   */
  object DataRegister extends BitStringEnumeration(3) {
    val D0, D1, D2, D3, D4, D5, D6, D7 = Value
  }

  /**
   * Address registers:
   * A0 = program counter
   * A1-A5 = general purpose
   * A6 = frame pointer by convention, but could be general-purpose
   * A7 = stack pointer
   */
  object AddrRegister extends BitStringEnumeration(3) {
    val A0, A1, A2, A3, A4, A5, A6, A7 = Value
  }

  /**
   * Operand sizes/scales/etc.:
   * B (byte) = 1 byte
   * W (word) = 2 bytes
   * L (long) = 4 bytes
   * Q (quad) = 8 bytes
   */
  object Size extends BitStringEnumeration(2) {
    val B, W, L, Q = Value
  }

  // Addressing modes, design borrows somewhat from m68k
  sealed trait EffectiveAddress
  sealed trait AlterableEffectiveAddress extends EffectiveAddress
  // Value in data register. ASM = "Dn", encoding = 000rrr
  case class DataDirect(r: DataRegister.Value) extends AlterableEffectiveAddress
  // Value in address register. ASM = "An", encoding = 001rrr
  case class AddrDirect(r: AddrRegister.Value) extends AlterableEffectiveAddress
  // Value at address specified by address register. ASM = "(An)", encoding = 010rrr
  case class AddrIndirect(r: AddrRegister.Value) extends AlterableEffectiveAddress
  // Value at address register + offset. ASM = "(d16,An)", encoding = 101rrr, 1 data word
  case class AddrIndirectOffset(r: AddrRegister.Value, offset: Word) extends AlterableEffectiveAddress
  // Value at absolute address. ASM = "(d32)", encoding = 111000, 2 data words
  case class Absolute(offset: LongWord) extends EffectiveAddress
  // Immediate value. ASM = #d32, encoding = 111100, 2 data words
  case class Immediate(offset: Word) extends EffectiveAddress

  sealed trait Instruction
  case class ADD(size: Size.Value, dest: DataRegister.Value, source: EffectiveAddress) extends Instruction
  case class MOVE(size: Size.Value, dest: AlterableEffectiveAddress, source: EffectiveAddress) extends Instruction
}
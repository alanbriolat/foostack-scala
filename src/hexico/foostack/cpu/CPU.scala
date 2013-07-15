package hexico.foostack.cpu

import hexico.foostack.cpu.Asm._

class CPU {
  object Condition extends Enumeration {
    val Extend, Negative, Zero, Overflow, Carry = Value
  }

  /**
    * Condition code register, copied from m68k
    */
  val ccr: Map[Condition.Value, Boolean] = Condition.values.map(x => (x, false)).toMap

  /**
   * Data registers
   */
  val data: Map[DataRegister.Value, LongWord] = DataRegister.values.map(x => (x, 0)).toMap

  /**
   * Address registers
   */
  val addr: Map[AddrRegister.Value, LongWord] = AddrRegister.values.map(x => (x, 0)).toMap
}

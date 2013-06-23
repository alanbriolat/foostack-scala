package hexico.foostack

object Asm {
  object Register extends Enumeration {
    val PC, SP, AX, BX, CX, DX, IX, IY, IZ = Value
  }

  object HalfRegister extends Enumeration {
    val AH, AL, BH, BL, CH, CL, DH, DL = Value
  }

  sealed trait Op
  case class Op_(opcode: String) extends Op
  case class OpR(opcode: String, r: Register.Value) extends Op
  case class OpI(opcode: String, i: Byte) extends Op
  case class OpRR(opcode: String, a: Register.Value, b: Register.Value) extends Op
  case class OpRI(opcode: String, r: Register.Value, i: Byte) extends Op
  case class OpHH(opcode: String, a: HalfRegister.Value, b: HalfRegister.Value) extends Op
  case class OpHI(opcode: String, r: HalfRegister.Value) extends Op

  val ops = Map(
    "LDBI" -> OpHI,
    "HALT" -> Op_
  )

  sealed abstract class Instruction
  case class HALT() extends Instruction
  case class LDBI(r: HalfRegister.Value, b: Byte) extends Instruction
  case class LDI(r: Register.Value, b: Byte) extends Instruction
  case class LD(r1: Register.Value , r2: Register.Value) extends Instruction
  case class LDB(r1: HalfRegister.Value, r2: HalfRegister.Value) extends Instruction
  case class PUSH(r: Register.Value) extends Instruction
  case class POP(r: Register.Value) extends Instruction
  case class PEEK(r: Register.Value) extends Instruction
  case class ADD(r1: Register.Value, r2: Register.Value) extends Instruction
  case class ADDI(r: Register.Value, b: Byte) extends Instruction
  case class ADDC(r1: Register.Value, r2: Register.Value) extends Instruction
  case class SUB(r1: Register.Value, r2: Register.Value) extends Instruction
  case class SUBI(r: Register.Value, b: Byte) extends Instruction
  case class SUBB(r1: Register.Value, r2: Register.Value) extends Instruction
  case class NEG(r: Register.Value) extends Instruction
  case class AND(r1: Register.Value, r2: Register.Value) extends Instruction
  case class OR(r1: Register.Value, r2: Register.Value) extends Instruction
  case class XOR(r1: Register.Value, r2: Register.Value) extends Instruction
  case class NOT(r: Register.Value) extends Instruction
  case class SHL(r1: Register.Value, r2: Register.Value) extends Instruction
  case class SHLI(r: Register.Value, b: Byte) extends Instruction
  case class SHR(r1: Register.Value, r2: Register.Value) extends Instruction
  case class SHRI(r: Register.Value, b: Byte) extends Instruction
  case class SSL(r1: Register.Value, r2: Register.Value) extends Instruction
  case class SSLI(r: Register.Value, b: Byte) extends Instruction
  case class SSR(r1: Register.Value, r2: Register.Value) extends Instruction
  case class SSRI(r: Register.Value, b: Byte) extends Instruction
  case class JMP(r: Register.Value) extends Instruction
  case class JR(r: Register.Value) extends Instruction
  case class JRI(b: Byte) extends Instruction
  case class JMPNZ(r: Register.Value) extends Instruction
  case class JRNZ(r: Register.Value) extends Instruction
  case class JRINZ(b: Byte) extends Instruction
}

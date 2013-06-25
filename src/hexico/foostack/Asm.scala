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
    "LDI" -> OpRI,
    "LD" -> OpRR,
    "LDB" -> OpHH,
    "PUSH" -> OpR,
    "POP" -> OpR,
    "PEEK" -> OpR,
    "ADD" -> OpRR,
    "ADDI" -> OpRI,
    "ADDC" -> OpRR,
    "SUB" -> OpRR,
    "SUBI" -> OpRI,
    "SUBB" -> OpRR,
    "NEG" -> OpR,
    "AND" -> OpRR,
    "OR" -> OpRR,
    "XOR" -> OpRR,
    "NOT" -> OpR,
    "SHL" -> OpRR,
    "SHLI" -> OpRI,
    "SHR" -> OpRR,
    "SHRI" -> OpRI,
    "SSL" -> OpRR,
    "SSLI" -> OpRI,
    "SSR" -> OpRR,
    "SSRI" -> OpRI,
    "JMP" -> OpR,
    "JR" -> OpR,
    "JRI" -> OpI,
    "JMPNZ" -> OpR,
    "JRNZ" -> OpR,
    "JRINZ" -> OpI,
    "HALT" -> Op_
  )
}

sealed class Inst
data class Push(val literal: Long): Inst()
data class ApplyUnary(val op: UnaryOp): Inst()
data class ApplyBinary(val op: BinaryOp): Inst()
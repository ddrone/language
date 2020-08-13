sealed class Inst
data class Push(val literal: Int): Inst()
data class ApplyUnary(val op: UnaryOp): Inst()
data class ApplyBinary(val op: BinaryOp): Inst()
data class MarkNode(val id: Int): Inst()
data class LookupLocal(val id: Int): Inst()
data class StoreLocal(val id: Int): Inst()
data class Or(val rhs: List<Inst>): Inst()
data class And(val rhs: List<Inst>): Inst()
data class Fork(val consequent: List<Inst>, val alternative: List<Inst>): Inst()
object StartMarking: Inst() {
    override fun toString(): String {
        return StartMarking.javaClass.simpleName
    }
}
data class EndMarking(val rootId: Int): Inst()
object Pop: Inst() {
    override fun toString(): String {
        return Pop.javaClass.simpleName
    }
}
object ReturnOp: Inst() {
    override fun toString(): String {
        return ReturnOp.javaClass.simpleName
    }
}
data class CallOp(val funName: String, val argsCount: Int): Inst()

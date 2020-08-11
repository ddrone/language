sealed class Inst
data class Push(val literal: Long): Inst()
data class ApplyUnary(val op: UnaryOp): Inst()
data class ApplyBinary(val op: BinaryOp): Inst()
data class MarkNode(val id: Int): Inst()
data class LookupLocal(val id: Int): Inst()
data class StoreLocal(val id: Int): Inst()
object StartMarking: Inst() {
    override fun toString(): String {
        return StartMarking.javaClass.simpleName
    }
}
object EndMarking: Inst() {
    override fun toString(): String {
        return EndMarking.javaClass.simpleName
    }
}
object Pop: Inst() {
    override fun toString(): String {
        return Pop.javaClass.simpleName
    }
}

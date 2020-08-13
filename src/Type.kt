sealed class Type {
    abstract val isComparable: Boolean
}

object IntType: Type() {
    override val isComparable = true
    override fun toString(): String {
        return IntType.javaClass.simpleName
    }
}

object BoolType: Type() {
    override val isComparable = true
    override fun toString(): String {
        return BoolType.javaClass.simpleName
    }
}

data class ListType(val elem: Type): Type() {
    override val isComparable = false
}

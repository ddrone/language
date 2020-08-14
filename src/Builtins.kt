abstract class BuiltinFunction(
        val name: String
) {
    abstract fun eval(args: List<Int>, heap: Heap): Int

    abstract fun typecheck(nodeId: Int, args: List<TypedNode>): Type
}

data class TypedNode(
        val nodeId: Int,
        val type: Type
)

object ListGet: BuiltinFunction("get") {
    override fun eval(args: List<Int>, heap: Heap): Int {
        val list = heap.items[args[0]] as ListValue
        return list.items[args[1]]
    }

    override fun typecheck(nodeId: Int, args: List<TypedNode>): Type {
        if (args.size != 2) {
            throw TypingFailure(nodeId, "get takes two arguments")
        }

        val first = args[0]
        if (first.type !is ListType) {
            throw TypingFailure(first.nodeId, "first argument of get should be a list")
        }

        val second = args[1]
        if (second.type !is IntType) {
            throw TypingFailure(second.nodeId, "second argument of get should be an integer")
        }

        return first.type.elem
    }
}

val builtins: List<BuiltinFunction> = listOf(
        ListGet
)

val builtinByName = builtins.map { it.name to it }.toMap()

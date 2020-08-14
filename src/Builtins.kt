import java.lang.RuntimeException

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
            throw TypingFailure(nodeId, "$name takes two arguments")
        }

        val first = args[0]
        if (first.type !is ListType) {
            throw TypingFailure(first.nodeId, "first argument of $name should be a list")
        }

        val second = args[1]
        if (second.type !is IntType) {
            throw TypingFailure(second.nodeId, "second argument of $name should be an integer")
        }

        return first.type.elem
    }
}

object VmStep: BuiltinFunction("step") {
    override fun eval(args: List<Int>, heap: Heap): Int {
        val arg = heap.items[args[0]] as ChildVmValue
        arg.vm.step()
        return arg.vm.isDone().asInt()
    }

    override fun typecheck(nodeId: Int, args: List<TypedNode>): Type {
        if (args.size != 1) {
            throw TypingFailure(nodeId, "$name takes one parameter")
        }

        val first = args[0]
        if (first.type !is VmType) {
            throw TypingFailure(first.nodeId, "first argument of step should be child VM")
        }

        return BoolType
    }
}

object VmExtract: BuiltinFunction("extract") {
    override fun eval(args: List<Int>, heap: Heap): Int {
        val arg = heap.items[args[0]] as ChildVmValue
        if (!arg.vm.isDone()) {
            throw RuntimeException("trying to extract value before it's finished")
        }

        if (arg.vm.stack.size() != 1) {
            throw RuntimeException("finished stack should contain only one value")
        }

        return arg.vm.stack[0]
    }

    override fun typecheck(nodeId: Int, args: List<TypedNode>): Type {
        if (args.size != 1) {
            throw TypingFailure(nodeId, "$name takes one parameter")
        }

        val first = args[0]
        if (first.type !is VmType) {
            throw TypingFailure(first.nodeId, "first argument of $name should be child VM")
        }

        return first.type.elem
    }
}

object VmStack: BuiltinFunction("stack") {
    override fun eval(args: List<Int>, heap: Heap): Int {
        val arg = heap.items[args[0]] as ChildVmValue
        val stack = arg.vm.stack
        val stackCopy = (0 until stack.size()).map { stack[it] }
        return heap.put(ListValue(stackCopy))
    }

    override fun typecheck(nodeId: Int, args: List<TypedNode>): Type {
        if (args.size != 1) {
            throw TypingFailure(nodeId, "$name takes one parameter")
        }

        val first = args[0]
        if (first.type !is VmType) {
            throw TypingFailure(first.nodeId, "first argument of $name should be child VM")
        }

        return ListType(IntType)
    }
}

val builtins: List<BuiltinFunction> = listOf(
        ListGet,
        VmStep,
        VmExtract,
        VmStack
)

val builtinByName = builtins.map { it.name to it }.toMap()

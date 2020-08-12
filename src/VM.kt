import java.lang.RuntimeException

class ExecutionException(override val message: String): RuntimeException(message)

class VM(val code: List<Inst>) {
    var currentPos: Int = 0
    var stack: Stack<Long> = Stack()
    var marks: Stack<MutableMap<Int, Long>> = Stack()
    val localsStart: Int = 0

    fun isDone(): Boolean {
        return currentPos >= code.size
    }

    fun step() {
        if (isDone()) {
            throw ExecutionException("trying to execute step after machine is done")
        }

        @Suppress("UNUSED_VARIABLE") val unused: Any = when (val curr = code[currentPos]) {
            is Push -> {
                stack.push(curr.literal)
            }
            is ApplyUnary -> {
                val argument = stack.pop()
                stack.push(curr.op.apply(argument))
            }
            is ApplyBinary -> {
                val right = stack.pop()
                val left = stack.pop()
                stack.push(curr.op.apply(left, right))
            }
            is MarkNode -> {
                marks.peek()[curr.id] = stack.peek()
            }
            is LookupLocal -> {
                stack.push(stack[localsStart + curr.id])
            }
            is StoreLocal -> {
                stack[localsStart + curr.id] = stack.pop()
            }
            StartMarking -> {
                marks.push(mutableMapOf())
            }
            EndMarking -> {
                val value = stack.peek()
                println("$value <= ${marks.pop()}")
            }
            Pop -> {
                stack.pop()
            }
        }
        currentPos++
    }

    fun loop() {
        while (!isDone()) {
            step()
        }
    }
}
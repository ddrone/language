import java.lang.RuntimeException

class ExecutionException(override val message: String): RuntimeException(message)

class VM(val code: List<Inst>) {
    var currentPos: Int = 0
    var stack: Stack<Long> = Stack()
    var marks: MutableMap<Int, Long> = mutableMapOf()
    val localsStart: Int = 0

    fun isDone(): Boolean {
        return currentPos >= code.size
    }

    fun step() {
        if (isDone()) {
            throw ExecutionException("trying to execute step after machine is done")
        }

        when (val curr = code[currentPos]) {
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
                marks[curr.id] = stack.peek()
            }
            is LookupLocal -> {
                stack.push(stack[localsStart + curr.id])
            }
            is StoreLocal -> {
                stack[localsStart + curr.id] = stack.pop()
            }
            StartMarking -> {
                marks.clear()
            }
            EndMarking -> {
                // Pop the result of evaluation.
                val value = stack.pop()
                println("$value <= $marks")
            }
            Pop -> {
                stack.pop()
            }
            else -> {
                throw ExecutionException("unknown instruction $curr")
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
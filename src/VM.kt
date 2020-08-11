import java.lang.RuntimeException
import java.util.ArrayDeque

class ExecutionException(override val message: String): RuntimeException(message)

class VM(val code: List<Inst>) {
    var currentPos: Int = 0
    var stack: ArrayDeque<Long> = ArrayDeque()

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
        }
        currentPos++
    }

    fun loop() {
        while (!isDone()) {
            step()
        }
    }
}
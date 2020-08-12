import java.lang.RuntimeException

class ExecutionException(override val message: String): RuntimeException(message)

class CodeFrame(val code: List<Inst>, val localsStart: Int, private var currentPos: Int) {
    fun isDone(): Boolean {
        return currentPos >= code.size
    }

    fun current(): Inst {
        return code[currentPos]
    }

    fun advance() {
        currentPos++
    }
}

class VM(code: List<Inst>, val debugger: Debugger) {
    var codeStack: Stack<CodeFrame> = Stack()
    var stack: Stack<Long> = Stack()
    var marksStack: Stack<MutableMap<Int, Long>> = Stack()
    val localsStart: Int = 0

    init {
        codeStack.push(CodeFrame(code, 0, 0))
    }

    fun isDone(): Boolean {
        return codeStack.isEmpty()
    }

    fun step() {
        if (isDone()) {
            throw ExecutionException("trying to execute step after machine is done")
        }

        val currentFrame = codeStack.peek()
        if (currentFrame.isDone()) {
            codeStack.pop()
            return
        }

        @Suppress("UNUSED_VARIABLE") val unused: Any = when (val curr = currentFrame.current()) {
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
                marksStack.peek()[curr.id] = stack.peek()
            }
            is LookupLocal -> {
                stack.push(stack[localsStart + curr.id])
            }
            is StoreLocal -> {
                stack[localsStart + curr.id] = stack.pop()
            }
            StartMarking -> {
                marksStack.push(linkedMapOf())
            }
            EndMarking -> {
                val value = stack.peek()
                val marks = marksStack.pop()
                println(value)
                for ((id, nodeValue) in marks.entries) {
                    debugger.exprById[id]?.let {
                        println("  ${Printer.printExpr(it)} => $nodeValue")
                    }
                }
            }
            Pop -> {
                stack.pop()
            }
            is Or -> {
                if (!stack.peek().asBoolean()) {
                    stack.pop()
                    codeStack.push(CodeFrame(curr.rhs, currentFrame.localsStart, 0))
                } else {
                    @Suppress("RedundantUnitExpression")
                    Unit
                }
            }
            is And -> {
                if (stack.peek().asBoolean()) {
                    stack.pop()
                    codeStack.push(CodeFrame(curr.rhs, currentFrame.localsStart, 0))
                } else {
                    @Suppress("RedundantUnitExrpession")
                    Unit
                }
            }
        }
        currentFrame.advance()
    }

    fun loop() {
        while (!isDone()) {
            step()
        }
    }
}
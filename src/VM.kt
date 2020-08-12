import java.lang.RuntimeException
import kotlin.jvm.internal.FunctionReference

class ExecutionException(override val message: String): RuntimeException(message)

class FunctionFrame(code: List<Inst>, val localsStart: Int) {
    val codeStack: Stack<CodeFrame> = Stack()

    init {
        codeStack.push(CodeFrame(code, 0))
    }

    fun current(): CodeFrame {
        while (codeStack.peek().isDone()) {
            codeStack.pop()
        }

        return codeStack.peek()
    }

    fun push(code: List<Inst>) {
        codeStack.push(CodeFrame(code, 0))
    }
}

class CodeFrame(val code: List<Inst>, private var currentPos: Int) {
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

class VM(code: List<Inst>, val functions: Map<String, List<Inst>>, val debugger: Debugger) {
    var functionsStack: Stack<FunctionFrame> = Stack()
    var stack: Stack<Long> = Stack()
    var marksStack: Stack<MutableMap<Int, Long>> = Stack()

    init {
        functionsStack.push(FunctionFrame(code, 0))
    }

    fun isDone(): Boolean {
        return functionsStack.isEmpty()
    }

    fun step() {
        if (isDone()) {
            throw ExecutionException("trying to execute step after machine is done")
        }

        val currentFunction = functionsStack.peek()
        val currentFrame = currentFunction.current()
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
                stack.push(stack[currentFunction.localsStart + curr.id])
            }
            is StoreLocal -> {
                stack[currentFunction.localsStart + curr.id] = stack.pop()
            }
            StartMarking -> {
                marksStack.push(linkedMapOf())
            }
            is EndMarking -> {
                val value = stack.peek()
                val marks = marksStack.pop()
                println(debugger.printValue(curr.rootId, value))
                for ((id, nodeValue) in marks.entries) {
                    debugger.exprById[id]?.let {
                        println("  ${Printer.printExpr(it)} => ${debugger.printValue(id, nodeValue)}")
                    }
                }
            }
            Pop -> {
                stack.pop()
            }
            is Or -> {
                if (!stack.peek().asBoolean()) {
                    stack.pop()
                    currentFunction.push(curr.rhs)
                } else {
                    @Suppress("RedundantUnitExpression")
                    Unit
                }
            }
            is And -> {
                if (stack.peek().asBoolean()) {
                    stack.pop()
                    currentFunction.push(curr.rhs)
                } else {
                    @Suppress("RedundantUnitExpression")
                    Unit
                }
            }
            is Fork -> {
                val condition = stack.pop().asBoolean()
                if (condition) {
                    currentFunction.push(curr.consequent)
                } else {
                    currentFunction.push(curr.alternative)
                }
            }
            ReturnOp -> {
                val value = stack.pop()
                stack.downsize(currentFunction.localsStart)
                stack.push(value)
                functionsStack.pop()
            }
            is CallOp -> {
                val localsStart = stack.size() - curr.argsCount
                val code = functions[curr.funName] ?: throw RuntimeException("unknown function ${curr.funName}")
                functionsStack.push(FunctionFrame(code, localsStart))
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
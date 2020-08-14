import java.lang.RuntimeException

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
    var stack: Stack<Int> = Stack()
    var marksStack: Stack<MutableMap<Int, Int>> = Stack()
    val heap = Heap()

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
                val printedIdentifiers = mutableSetOf<String>()
                println(debugger.printValue(curr.rootId, value, heap))
                for ((id, nodeValue) in marks.entries) {
                    val type = debugger.types[id]
                            ?: throw RuntimeException("unknown type for node id=$id")
                    val expr = debugger.exprById[id]
                            ?: throw RuntimeException("unknown node with id=$id")

                    var doPrint = true

                    when {
                        expr is Reference && printedIdentifiers.contains(expr.token.getText()) -> {
                            doPrint = false
                        }
                        expr is Reference && !type.isAllocated -> {
                            printedIdentifiers.add(expr.token.getText())
                        }
                        expr is Literal -> {
                            doPrint = false
                        }
                    }

                    if (doPrint) {
                        println("  ${Printer.printExpr(expr)} => ${debugger.printValue(id, nodeValue, heap)}")
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
                val builtin = builtinByName[curr.funName]
                val localsStart = stack.size() - curr.argsCount

                if (builtin != null) {
                    val args = (0 until curr.argsCount).map { stack[localsStart + it] }
                    val result = builtin.eval(args, heap)
                    stack.downsize(localsStart)
                    stack.push(result)
                }
                else {
                    val code = functions[curr.funName] ?: throw RuntimeException("unknown function ${curr.funName}")
                    functionsStack.push(FunctionFrame(code, localsStart))
                }
            }
            is BuildList -> {
                val newSize = stack.size() - curr.itemsCount
                val resultList = (0 until curr.itemsCount).map { stack[newSize + it] }
                stack.downsize(newSize)
                stack.push(heap.put(ListValue(resultList)))
            }
            is SpawnOp -> {
                val childVm = VM(curr.code, functions, debugger)
                val result = heap.put(ChildVmValue(childVm))
                stack.push(result)
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
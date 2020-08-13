import java.lang.RuntimeException

class TypingFailure(val nodeId: Int, override val message: String): RuntimeException(message)

class TypeChecker {
    val typeById = mutableMapOf<Int, Type>()
    val localTypes = mutableMapOf<String, Type>()
    val functionByName = mutableMapOf<String, Function>()

    fun Expr.type(): Type {
        return typeById[this.id] ?: throw RuntimeException("trying to access uncomputed type of $this")
    }

    fun expectType(type: Type, expr: Expr, message: String) {
        val got = inferType(expr)
        if (type != got) {
            throw TypingFailure(expr.id, message)
        }
    }

    fun inferType(expr: Expr): Type {
        val cachedType = typeById[expr.id]
        if (cachedType != null) {
            return cachedType
        }

        val result: Type = when (expr) {
            is Literal -> IntType
            is Unary -> {
                expectType(expr.op.argType(), expr.child, "unary operator argument mismatch")
                expr.op.resultType()
            }
            is Binary -> {
                when (val op = expr.op) {
                    BinaryOp.EQ -> {
                        val leftType = inferType(expr.left)
                        val rightType = inferType(expr.right)
                        if (leftType == rightType && leftType.isComparable) {
                            op.resultType()
                        } else {
                            throw TypingFailure(expr.id, "different types of comparison: $leftType and $rightType")
                        }
                    }
                    else -> {
                        expectType(op.argType(), expr.left, "binary operator argument mismatch")
                        expectType(op.argType(), expr.right, "binary operator argument mismatch")
                        op.resultType()
                    }
                }
            }
            is Reference -> {
                val varName = expr.token.getText()
                localTypes[varName] ?:
                        throw TypingFailure(expr.id, "unbound variable $varName")
            }
            is Debug -> {
                inferType(expr.child)
            }
            is Call -> {
                val funName = expr.funName.getText()
                val function = functionByName[funName] ?: throw TypingFailure(expr.id, "unknown function $funName")

                if (function.args.size != expr.args.size) {
                    throw TypingFailure(expr.id, "$funName: expected ${function.args.size} arguments, got ${expr.args.size}")
                }

                for (i in function.args.indices) {
                    expectType(function.args[i].type, expr.args[i], "$funName: wrong type of argument ${i + 1}")
                }
                function.returnType
            }
            is ListLiteral -> {
                if (expr.items.isEmpty()) {
                    throw TypingFailure(expr.id, "can't infer type of empty list")
                }

                val type = inferType(expr.items.first())
                for (i in 1 until expr.items.size) {
                    expectType(type, expr.items[i], "list items should have the same type")
                }

                ListType(type)
            }
        }

        typeById[expr.id] = result
        return result
    }

    fun typecheckStmt(stmt: Stmt, returnType: Type) {
        @Suppress("UNUSED_VARIABLE") val unused: Any = when (stmt) {
            is Assign -> {
                if (stmt.target !is Reference) {
                    throw TypingFailure(stmt.id, "target of assignment shoud be reference")
                }

                val varName = stmt.target.token.getText()
                if (stmt.isDeclaration) {
                    if (localTypes.containsKey(varName)) {
                        throw TypingFailure(stmt.id, "duplicate declaration of $varName")
                    }

                    val type = inferType(stmt.expr)
                    localTypes[varName] = type
                }
                else {
                    val type = localTypes[varName]
                            ?: throw TypingFailure(stmt.id, "assignment to undeclared variable $varName")

                    expectType(type, stmt.expr, "assigned expression should have type $type")
                }
            }
            is ExprWrap -> inferType(stmt.expr)
            is If -> {
                expectType(BoolType, stmt.condition, "condition should be boolean")
                stmt.consequent.forEach {
                    typecheckStmt(it, returnType)
                }
                stmt.alternative.forEach {
                    typecheckStmt(it, returnType)
                }
            }
            is Return -> {
                expectType(returnType, stmt.expr, "wrong type of return statement")
            }
        }
    }

    fun typecheckFunction(function: Function) {
        localTypes.clear()
        for (arg in function.args) {
            localTypes[arg.name.getText()] = arg.type
        }

        function.body.forEach {
            typecheckStmt(it, function.returnType)
        }
    }

    fun typecheckProgram(functions: List<Function>) {
        functions.forEach {
            functionByName[it.name.getText()] = it
        }

        functions.forEach(::typecheckFunction)
    }
}
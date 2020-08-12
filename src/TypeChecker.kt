import java.lang.RuntimeException

class TypingFailure(val nodeId: Int, override val message: String): RuntimeException(message)

class TypeChecker {
    val typeById = mutableMapOf<Int, Type>()
    val localTypes = mutableMapOf<String, Type>()

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
        }

        typeById[expr.id] = result
        return result
    }

    fun typecheckStmt(stmt: Stmt) {
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
                stmt.consequent.forEach(::typecheckStmt)
                stmt.alternative.forEach(::typecheckStmt)
            }
            is Return -> {
                TODO("implement me")
            }
        }
    }

    fun typecheckProgram(code: List<Stmt>) {
        code.forEach(::typecheckStmt)
    }
}
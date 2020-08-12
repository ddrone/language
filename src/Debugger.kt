import java.lang.RuntimeException

class Debugger(program: List<Stmt>, val types: Map<Int, Type>) {
    val exprById = mutableMapOf<Int, Expr>()
    val stmtById = mutableMapOf<Int, Stmt>()

    private fun addStmt(stmt: Stmt) {
        stmtById[stmt.id] = stmt
        @Suppress("UNUSED_VARIABLE") val unused: Any = when (stmt) {
            is Assign -> {
                addExpr(stmt.target)
                addExpr(stmt.expr)
            }
            is ExprWrap -> {
                addExpr(stmt.expr)
            }
            is If -> {
                addExpr(stmt.condition)
                stmt.consequent.forEach(::addStmt)
                stmt.alternative.forEach(::addStmt)
            }
        }
    }

    private fun addExpr(expr: Expr) {
        exprById[expr.id] = expr
        @Suppress("UNUSED_VARIABLE") val unused: Any = when (expr) {
            is Literal -> {
                // Do nothing
            }
            is Unary -> {
                addExpr(expr.child)
            }
            is Binary -> {
                addExpr(expr.left)
                addExpr(expr.right)
            }
            is Reference -> {
                // Do nothing
            }
            is Debug -> {
                addExpr(expr.child)
            }
        }
    }

    fun printValue(value: Long, type: Type): String {
        return when (type) {
            IntType -> value.toString()
            BoolType -> value.asBoolean().toString()
        }
    }

    fun printValue(nodeId: Int, value: Long): String {
        val type = types[nodeId]

        return if (type == null) {
            throw RuntimeException("do not have type information for node $nodeId")
        } else {
            printValue(value, type)
        }
    }

    init {
        program.forEach(::addStmt)
    }
}
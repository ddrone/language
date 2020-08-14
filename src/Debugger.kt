import java.lang.RuntimeException
import java.lang.StringBuilder

class Debugger(program: List<Function>, val types: Map<Int, Type>) {
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
            is Return -> {
                addExpr(stmt.expr)
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
            is Call -> {
                expr.args.forEach(::addExpr)
            }
            is ListLiteral -> {
                expr.items.forEach(::addExpr)
            }
            is Spawn -> {
                addExpr(expr.child)
            }
        }
    }

    fun printValue(value: Int, type: Type, heap: Heap): String {
        return when (type) {
            IntType -> value.toString()
            BoolType -> value.asBoolean().toString()
            is ListType -> {
                val list = heap.items[value]
                if (list !is ListValue) {
                    throw RuntimeException("expected list value when printing list, got $list")
                }

                val result = StringBuilder("[")
                result.separating(list.items) {
                    result.append(printValue(it, type.elem, heap))
                }
                result.append("]")
                result.toString()
            }
            is VmType -> {
                "vm<addr=$value>"
            }
        }
    }

    fun printValue(nodeId: Int, value: Int, heap: Heap): String {
        val type = types[nodeId]

        return if (type == null) {
            throw RuntimeException("do not have type information for node $nodeId")
        } else {
            printValue(value, type, heap)
        }
    }

    init {
        for (function in program) {
            function.body.forEach(::addStmt)
        }
    }
}
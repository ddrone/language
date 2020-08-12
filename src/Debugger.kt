class Debugger(program: List<Stmt>) {
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

    init {
        program.forEach(::addStmt)
    }
}
sealed class Stmt {
    abstract val id: Int
}

data class Assign(override val id: Int, val isDeclaration: Boolean, val target: Expr, val expr: Expr) : Stmt()
data class ExprWrap(override val id: Int, val expr: Expr) : Stmt()
data class If(override val id: Int, val condition: Expr, val consequent: List<Stmt>, val alternative: List<Stmt>) : Stmt()

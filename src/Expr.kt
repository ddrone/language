sealed class Expr {
    abstract val id: Int
}
data class Literal(override val id: Int, val literal: Long): Expr()
data class Unary(override val id: Int, val op: UnaryOp, val child: Expr): Expr()
data class Binary(override val id: Int, val left: Expr, val op: BinaryOp, val right: Expr): Expr()
data class Reference(override val id: Int, val token: Token): Expr()
data class Debug(override val id: Int, val child: Expr): Expr()

enum class UnaryOp(val string: String) {
    NEGATE("-");

    fun apply(input: Long): Long {
        return when (this) {
            NEGATE -> -input
        }
    }
}

enum class BinaryOp(val string: String, val priority: Int) {
    PLUS("+", 1),
    MINUS("-", 1),
    MULT("*", 2);

    fun apply(left: Long, right: Long): Long {
        return when (this) {
            PLUS -> left + right
            MINUS -> left - right
            MULT -> left * right
        }
    }
}
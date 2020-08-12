import java.lang.RuntimeException

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

    fun argType(): Type {
        return when (this) {
            NEGATE -> IntType
        }
    }

    fun resultType(): Type {
        return when (this) {
            NEGATE -> IntType
        }
    }
}

enum class BinaryOp(val string: String, val priority: Int) {
    OR("||", 5),
    AND("&&", 4),
    EQ("==", 3),
    PLUS("+", 2),
    MINUS("-", 2),
    MULT("*", 1);

    fun apply(left: Long, right: Long): Long {
        return when (this) {
            PLUS -> left + right
            MINUS -> left - right
            MULT -> left * right
            EQ -> (left == right).asLong()
            AND, OR -> {
                throw RuntimeException("AND and OR are special cases")
            }
        }
    }

    fun argType(): Type {
        return when (this) {
            OR -> BoolType
            AND -> BoolType
            EQ -> {
                throw RuntimeException("EQ is special case")
            }
            PLUS -> IntType
            MINUS -> IntType
            MULT -> IntType
        }
    }

    fun resultType(): Type {
        return when (this) {
            OR -> BoolType
            AND -> BoolType
            EQ -> BoolType
            PLUS -> IntType
            MINUS -> IntType
            MULT -> IntType
        }
    }
}

fun Long.asBoolean(): Boolean {
    return when {
        this == 0L -> {
            false
        }
        this == 1L -> {
            true
        }
        else -> {
            throw RuntimeException("can't convert $this to boolean")
        }
    }
}

fun Boolean.asLong(): Long {
    return if (this) {
        1L
    } else {
        0L
    }
}

import java.lang.StringBuilder

class Printer {
    private val builder = StringBuilder()

    private fun bracket(doBracket: Boolean = true, fn: () -> Unit) {
        if (doBracket) {
            builder.append("(")
        }
        fn()
        if (doBracket) {
            builder.append(")")
        }
    }

    private fun print(expr: Expr) {
        @Suppress("UNUSED_VARIABLE") val unused: Any = when (expr) {
            is Literal -> builder.append(expr.literal)
            is Unary -> {
                builder.append(expr.op.string)
                bracket(expr.child is Binary || expr.child is Debug) {
                    print(expr.child)
                }
            }
            is Binary -> {
                bracket(expr.left is Debug || expr.left is Binary && expr.left.op.priority > expr.op.priority) {
                    print(expr.left)
                }
                builder.append(" ${expr.op.string} ")
                bracket(expr.right is Binary && expr.right.op.priority >= expr.op.priority) {
                    print(expr.right)
                }
            }
            is Reference -> {
                builder.append(expr.token.getText())
            }
            is Debug -> {
                builder.append("debug ")
                print(expr.child)
            }
            is Call -> {
                builder.append(expr.funName.getText())
                builder.append("(")
                var first = true
                for (arg in expr.args) {
                    if (!first) {
                        builder.append(", ")
                    }
                    first = false
                    print(arg)
                }
                builder.append(")")
            }
        }
    }

    override fun toString(): String {
        return builder.toString()
    }

    companion object {
        fun printExpr(expr: Expr): String {
            val printer = Printer()
            printer.print(expr)
            return printer.toString()
        }
    }
}
class Compiler {
    val output = mutableListOf<Inst>()

    private fun compileExpr(expr: Expr) {
        when (expr) {
            is Literal -> {
                output.add(Push(expr.literal))
            }
            is Unary -> {
                compileExpr(expr.child)
                output.add(ApplyUnary(expr.op))
            }
            is Binary -> {
                compileExpr(expr.left)
                compileExpr(expr.right)
                output.add(ApplyBinary(expr.op))
            }
        }
    }

    companion object {
        fun compile(expr: Expr): List<Inst> {
            val compiler = Compiler()
            compiler.compileExpr(expr)
            return compiler.output
        }
    }
}
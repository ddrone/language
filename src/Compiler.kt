import java.lang.RuntimeException

class CompilerException(val nodeId: Int, override val message: String): RuntimeException(message)

class Compiler {
    val output = mutableListOf<Inst>()
    val locals = Stack<String>()

    private fun compileExpr(expr: Expr, isDebug: Boolean) {
        @Suppress("UNUSED_VARIABLE") val unused = when (expr) {
            is Literal -> {
                output.add(Push(expr.literal))
            }
            is Unary -> {
                compileExpr(expr.child, isDebug)
                output.add(ApplyUnary(expr.op))
            }
            is Binary -> {
                compileExpr(expr.left, isDebug)
                compileExpr(expr.right, isDebug)
                output.add(ApplyBinary(expr.op))
            }
            is Reference -> {
                val name = expr.token.getText()
                val id = locals.indexOf(name)
                if (id == -1) {
                    throw CompilerException(expr.id, "unbound identifier")
                }
                output.add(LookupLocal(id))
            }
            is Debug -> {
                output.add(StartMarking)
                compileExpr(expr.expr, true)
                output.add(EndMarking)
            }
        }
        if (isDebug) {
            output.add(MarkNode(expr.id))
        }
    }

    private fun compileStmt(stmt: Stmt) {
        return when (stmt) {
            is Assign -> {
                if (stmt.target !is Reference) {
                    throw CompilerException(stmt.id, "only references can be assignment targets, got ${stmt.target.javaClass.simpleName}")
                }

                val name = stmt.target.token.getText()
                val id = locals.indexOf(name)
                if (stmt.isDeclaration && id != -1) {
                    throw CompilerException(stmt.id, "multiple declaration")
                } else if (!stmt.isDeclaration && id == -1) {
                    throw CompilerException(stmt.id, "assignment to undeclared variable")
                }

                compileExpr(stmt.expr, false)
                if (stmt.isDeclaration) {
                    locals.push(name)
                } else {
                    output.add(StoreLocal(id))
                }
                Unit
            }
            is ExprWrap -> {
                compileExpr(stmt.expr, false)
                output.add(Pop)
                Unit
            }
        }
    }

    companion object {
        fun compile(program: List<Stmt>): List<Inst> {
            val compiler = Compiler()
            program.forEach(compiler::compileStmt)
            return compiler.output
        }
    }
}
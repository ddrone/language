// Copyright 2020 Andrew Shulaev
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// 
//     http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

import java.lang.RuntimeException

class CompilerException(val nodeId: Int, override val message: String): RuntimeException(message)

class Compiler() {
    var output = mutableListOf<Inst>()
    val locals = Stack<String>()

    private fun replacingOutput(callback: () -> Unit): List<Inst> {
        val saved = output
        val result = mutableListOf<Inst>()
        output = result
        callback()
        output = saved
        return result
    }

    private fun compileExpr(expr: Expr, isDebug: Boolean) {
        @Suppress("UNUSED_VARIABLE") val unused: Any = when (expr) {
            is Literal -> {
                output.add(Push(expr.literal))
            }
            is Unary -> {
                compileExpr(expr.child, isDebug)
                output.add(ApplyUnary(expr.op))
            }
            is Binary -> {
                if (expr.op == BinaryOp.OR || expr.op == BinaryOp.AND) {
                    compileExpr(expr.left, isDebug)
                    val rhs = replacingOutput {
                        compileExpr(expr.right, isDebug)
                    }
                    when (expr.op) {
                        BinaryOp.OR -> {
                            output.add(Or(rhs))
                        }
                        BinaryOp.AND -> {
                            output.add(And(rhs))
                        }
                        else -> {
                            throw RuntimeException("unexpected operator ${expr.op}")
                        }
                    }
                } else {
                    compileExpr(expr.left, isDebug)
                    compileExpr(expr.right, isDebug)
                    output.add(ApplyBinary(expr.op))
                }
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
                compileExpr(expr.child, true)
                output.add(EndMarking(expr.id))
            }
            is Call -> {
                expr.args.forEach {
                    compileExpr(it, isDebug)
                }
                output.add(CallOp(expr.funName.getText(), expr.args.size))
            }
            is ListLiteral -> {
                expr.items.forEach {
                    compileExpr(it, isDebug)
                }
                output.add(BuildList(expr.items.size))
            }
            is Spawn -> {
                val code = replacingOutput {
                    compileExpr(expr.child, false)
                }
                output.add(SpawnOp(code))
            }
        }
        if (isDebug) {
            output.add(MarkNode(expr.id))
        }
    }

    private fun compileStmt(stmt: Stmt) {
        @Suppress("UNUSED_VARIABLE") val unused: Any = when (stmt) {
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
            }
            is ExprWrap -> {
                compileExpr(stmt.expr, false)
                output.add(Pop)
            }
            is If -> {
                compileExpr(stmt.condition, false)
                val consequent = replacingOutput {
                    stmt.consequent.forEach(::compileStmt)
                }
                val alternative = replacingOutput {
                    stmt.alternative.forEach(::compileStmt)
                }
                output.add(Fork(consequent, alternative))
            }
            is Return -> {
                compileExpr(stmt.expr, false)
                output.add(ReturnOp)
            }
        }
    }

    private fun compileFunction(function: Function): List<Inst> {
        return replacingOutput {
            locals.clear()
            for (arg in function.args) {
                locals.push(arg.name.getText())
            }

            function.body.forEach(::compileStmt)
        }
    }

    companion object {
        fun compile(program: List<Function>): Map<String, List<Inst>> {
            val result = mutableMapOf<String, List<Inst>>()
            val compiler = Compiler()
            for (function in program) {
                result[function.name.getText()] = compiler.compileFunction(function)
            }
            return result
        }
    }
}
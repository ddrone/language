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
                builder.separating(expr.args) {
                    print(it)
                }
                builder.append(")")
            }
            is ListLiteral -> {
                builder.append("[")
                builder.separating(expr.items) {
                    print(it)
                }
                builder.append("]")
            }
            is Spawn -> {
                builder.append("spawn ")
                print(expr.child)
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
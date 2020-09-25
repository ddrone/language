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

sealed class Expr {
    abstract val id: Int
}
data class Literal(override val id: Int, val literal: Int): Expr()
data class Unary(override val id: Int, val op: UnaryOp, val child: Expr): Expr()
data class Binary(override val id: Int, val left: Expr, val op: BinaryOp, val right: Expr): Expr()
data class Reference(override val id: Int, val token: Token): Expr()
data class Debug(override val id: Int, val child: Expr): Expr()
data class Call(override val id: Int, val funName: Token, val args: List<Expr>): Expr()
data class ListLiteral(override val id: Int, val items: List<Expr>): Expr()
data class Spawn(override val id: Int, val child: Expr): Expr()

enum class UnaryOp(val string: String) {
    NEGATE("-");

    fun apply(input: Int): Int {
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
    LT("<", 3),
    GT(">", 3),
    LEQ("<=", 3),
    GEQ(">=", 3),
    PLUS("+", 2),
    MINUS("-", 2),
    MULT("*", 1);

    fun apply(left: Int, right: Int): Int {
        return when (this) {
            PLUS -> left + right
            MINUS -> left - right
            MULT -> left * right
            EQ -> (left == right).asInt()
            AND, OR -> {
                throw RuntimeException("AND and OR are special cases")
            }
            LT -> (left < right).asInt()
            GT -> (left > right).asInt()
            LEQ -> (left <= right).asInt()
            GEQ -> (left >= right).asInt()
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
            LT -> IntType
            GT -> IntType
            LEQ -> IntType
            GEQ -> IntType
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
            LT -> BoolType
            GT -> BoolType
            LEQ -> BoolType
            GEQ -> BoolType
        }
    }
}

fun Int.asBoolean(): Boolean {
    return when {
        this == 0 -> {
            false
        }
        this == 1 -> {
            true
        }
        else -> {
            throw RuntimeException("can't convert $this to boolean")
        }
    }
}

fun Boolean.asInt(): Int {
    return if (this) {
        1
    } else {
        0
    }
}

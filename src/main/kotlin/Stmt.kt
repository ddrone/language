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

sealed class Stmt {
    abstract val id: Int
}

data class Assign(override val id: Int, val isDeclaration: Boolean, val target: Expr, val expr: Expr) : Stmt()
data class ExprWrap(override val id: Int, val expr: Expr) : Stmt()
data class If(override val id: Int, val condition: Expr, val consequent: List<Stmt>, val alternative: List<Stmt>) : Stmt()
data class Return(override val id: Int, val expr: Expr): Stmt()

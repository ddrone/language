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

sealed class Inst
data class Push(val literal: Int): Inst()
data class ApplyUnary(val op: UnaryOp): Inst()
data class ApplyBinary(val op: BinaryOp): Inst()
data class MarkNode(val id: Int): Inst()
data class LookupLocal(val id: Int): Inst()
data class StoreLocal(val id: Int): Inst()
data class Or(val rhs: List<Inst>): Inst()
data class And(val rhs: List<Inst>): Inst()
data class Fork(val consequent: List<Inst>, val alternative: List<Inst>): Inst()
object StartMarking: Inst() {
    override fun toString(): String {
        return StartMarking.javaClass.simpleName
    }
}
data class EndMarking(val rootId: Int): Inst()
object Pop: Inst() {
    override fun toString(): String {
        return Pop.javaClass.simpleName
    }
}
object ReturnOp: Inst() {
    override fun toString(): String {
        return ReturnOp.javaClass.simpleName
    }
}
data class CallOp(val funName: String, val argsCount: Int): Inst()
data class BuildList(val itemsCount: Int): Inst()
data class SpawnOp(val code: List<Inst>): Inst()

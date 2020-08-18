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

sealed class Type {
    abstract val isComparable: Boolean
    abstract val isAllocated: Boolean
}

object IntType: Type() {
    override val isComparable = true
    override val isAllocated = false
    override fun toString(): String {
        return IntType.javaClass.simpleName
    }
}

object BoolType: Type() {
    override val isComparable = true
    override val isAllocated = false
    override fun toString(): String {
        return BoolType.javaClass.simpleName
    }
}

data class ListType(val elem: Type): Type() {
    override val isComparable = false
    override val isAllocated = true
}

data class VmType(val elem: Type): Type() {
    override val isComparable = false
    override val isAllocated = true
}

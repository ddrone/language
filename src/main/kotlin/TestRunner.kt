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

import java.io.File
import java.lang.RuntimeException

fun main() {
    File("tests").walkTopDown().forEach {
        if (it.isFile && it.extension == "lang") {
            println(it)
            val source = it.readText()
            val parsed = Parser(source).parse()
            eval(parsed, StdOutput)
        }
    }
}

fun eval(parsed: List<Function>, output: VMOutput) {
    val typechecker = TypeChecker()
    typechecker.typecheckProgram(parsed)

    val compiled = Compiler.compile(parsed)
    val debugger = Debugger(parsed, typechecker.typeById)

    val main = parsed.find { it.name.getText() == "main" }
            ?: throw RuntimeException("program does not have main")

    if (main.args.isNotEmpty()) {
        throw RuntimeException("main should not have arguments")
    }

    val mainCode = compiled["main"] ?: throw RuntimeException("compiled code has main missing?!")

    val vm = VM(mainCode, compiled, debugger, output)
    vm.loop()
}

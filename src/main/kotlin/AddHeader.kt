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

import java.util.*
import java.io.File

val calendar = Calendar.getInstance()
val year = calendar.get(Calendar.YEAR)

val header = """
   Copyright $year Andrew Shulaev

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
""".trimIndent()

val sourceHeader = header.lines().map { "// $it" }.joinToString("\n")

fun main() {
    File(".").walkTopDown().forEach {
        val extension = it.extension
        if (extension == "kt" || extension == "kts") {
            val contents = it.readText()
            if (!contents.startsWith("// Copyright")) {
                it.writeText("$sourceHeader\n\n$contents")
            }
        }
    }
}

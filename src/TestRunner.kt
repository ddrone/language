import java.io.File

fun main() {
    File("tests").walkTopDown().forEach {
        if (it.isFile && it.extension == "lang") {
            println(it)
            val source = it.readText()
            val parsed = Parser(source).parse()
            println(parsed)
            val compiled = Compiler.compile(parsed)
            val vm = VM(compiled)
            vm.loop()
            println(vm.stack)
        }
    }
}
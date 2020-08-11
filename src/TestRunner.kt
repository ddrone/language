import java.io.File

fun main() {
    File("tests").walkTopDown().forEach {
        if (it.isFile && it.extension == "lang") {
            println(it)
            val source = it.readText()
            val parsed = Parser(source).parse()
            println(parsed)
            eval(parsed)
        }
    }
}

fun eval(parsed: List<Stmt>) {
    val compiled = Compiler.compile(parsed)

    compiled.forEach(::println)

    val vm = VM(compiled)
    vm.loop()
    println(vm.stack)
}

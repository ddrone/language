import java.io.File

fun main() {
    File("tests").walkTopDown().forEach {
        if (it.isFile && it.extension == "lang") {
            println(it)
            val source = it.readText()
            val parsed = Parser(source).parse()
            eval(parsed)
        }
    }
}

fun eval(parsed: List<Stmt>) {
    val compiled = Compiler.compile(parsed)
    val typechecker = TypeChecker()
    typechecker.typecheckProgram(parsed)
    val debugger = Debugger(parsed, typechecker.typeById)

    val vm = VM(compiled, debugger)
    vm.loop()
    println(vm.stack)
}

import java.io.File
import java.lang.RuntimeException

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

fun eval(parsed: List<Function>) {
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

    val vm = VM(mainCode, compiled, debugger)
    vm.loop()
    println(vm.stack)
}

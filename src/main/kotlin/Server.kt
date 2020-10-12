import io.javalin.Javalin
import java.io.File

fun main() {
    val app = Javalin.create {
        it.showJavalinBanner = false
    }.start(31337)
    app.get("/") {
        val index = File("static/index.html").readText()
        it.html(index)
    }
    app.post("/run") {
        val code = it.formParam("code")
        if (code == null) {
            it.result("no argument!")
            return@post
        }
        val parsed = Parser(code).parse()
        val output = CollectOutput()
        eval(parsed, output)
        it.result(output.builder.toString())
    }
}
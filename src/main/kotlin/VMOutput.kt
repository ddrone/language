import java.lang.StringBuilder

interface VMOutput {
    fun output(s: String)
}

object StdOutput : VMOutput {
    override fun output(s: String) {
        println(s)
    }
}

class CollectOutput() : VMOutput {
    val builder = StringBuilder()

    override fun output(s: String) {
        builder.append(s).append("\n")
    }
}

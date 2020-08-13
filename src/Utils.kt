import java.lang.StringBuilder

fun <T> StringBuilder.separating(items: List<T>, sep: String = ", ", callback: (T) -> Unit) {
    var first = true
    for (item in items) {
        if (!first) {
            append(sep)
        }
        first = false
        callback(item)
    }
}
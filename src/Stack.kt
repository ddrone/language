import java.lang.RuntimeException

class Stack<E> {
    private val elements = mutableListOf<E>()

    fun indexOf(e: E): Int {
        return elements.indexOf(e)
    }

    fun push(e: E) {
        elements.add(e)
    }

    fun pop(): E {
        if (elements.size == 0) {
            throw RuntimeException("trying to pop from empty stack")
        }
        return elements.removeAt(elements.size - 1)
    }

    fun peek(): E {
        return elements.last()
    }

    operator fun get(index: Int): E {
        return elements[index]
    }

    operator fun set(index: Int, value: E) {
        elements[index] = value
    }

    override fun toString(): String {
        return elements.toString()
    }

    fun isEmpty(): Boolean {
        return elements.isEmpty()
    }
}
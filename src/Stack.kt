import java.lang.IndexOutOfBoundsException
import java.lang.RuntimeException

class Stack<E> {
    private val elements = mutableListOf<E>()
    private var size = 0

    fun indexOf(e: E): Int {
        val index = elements.indexOf(e)

        return if (index >= size) {
            -1
        } else {
            index
        }
    }

    fun push(e: E) {
        if (size < elements.size) {
            elements[size] = e
        } else {
            elements.add(e)
        }
        size++
    }

    fun pop(): E {
        if (size == 0) {
            throw RuntimeException("trying to pop from empty stack")
        }
        return elements[--size]
    }

    fun peek(): E {
        if (size == 0) {
            throw RuntimeException("trying to peek into empty stack")
        }
        return elements[size - 1]
    }

    operator fun get(index: Int): E {
        if (index < 0 || index >= size) {
            throw IndexOutOfBoundsException(index)
        }
        return elements[index]
    }

    operator fun set(index: Int, value: E) {
        if (index < 0 || index >= size) {
            throw IndexOutOfBoundsException(index)
        }
        elements[index] = value
    }

    override fun toString(): String {
        return elements.subList(0, size).toString()
    }

    fun isEmpty(): Boolean {
        return size == 0
    }

    fun downsize(newSize: Int) {
        if (newSize > elements.size) {
            throw RuntimeException("can't downsize to $newSize from ${elements.size}")
        }

        size = newSize
    }

    fun clear() {
        downsize(0)
    }
}
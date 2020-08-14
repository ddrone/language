class Heap {
    val items = mutableListOf<HeapValue>()

    fun put(value: HeapValue): Int {
        val result = items.size
        items.add(value)
        return result
    }
}

sealed class HeapValue
data class ListValue(val items: List<Int>): HeapValue()
data class ChildVmValue(val vm: VM): HeapValue()
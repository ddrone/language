data class Argument(
        val id: Int,
        val name: Token,
        val type: Type
)

data class Function(
        val id: Int,
        val name: Token,
        val args: List<Argument>,
        val returnType: Type,
        val body: List<Stmt>
)

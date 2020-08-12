data class Argument(
        val name: Token,
        val type: Type
)

data class Function(
        val name: Token,
        val args: List<Argument>,
        val returnType: Type,
        val body: List<Stmt>
)

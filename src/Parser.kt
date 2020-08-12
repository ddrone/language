import java.lang.RuntimeException

enum class TokenType(val string: String? = null, val pattern: Regex? = null) {
    LITERAL(pattern = Regex("-?[0-9]+")),
    IDENTIFIER(pattern = Regex("[a-zA-Z][a-zA-Z0-9_]*")),
    COMMENT_START(string = "//"),
    MULTILINE_COMMENT_START(string = "/*"),
    PLUS(string = "+"),
    MINUS(string = "-"),
    MULT(string = "*"),

    // Order of enum values determines the order in which these would be checked.
    // Therefore, if a token is a prefix of another token it should go later.
    // In this case, DIV goes after COMMENT_START.
    DIV(string = "/"),
    MOD(string = "%"),
    AND(string = "&&"),
    OR(string = "||"),
    OPEN_PAREN(string = "("),
    CLOSE_PAREN(string = ")"),
    OPEN_BRACE(string = "{"),
    CLOSE_BRACE(string = "}"),
    SEMICOLON(string = ";"),
    COLON(string = ":"),
    EQUALS_EQUALS(string = "=="),
    EQUALS(string = "="),
    COMMA(string = ","),
    KEYWORD,
    EOF;
}

object Keywords {
    val valKw = "val"
    val funKw = "fun"
    val debugKw = "debug"
    val ifKw = "if"
    val elseKw = "else"
    val returnKw = "return"

    val allKeywords = setOf(
            valKw,
            funKw,
            debugKw,
            ifKw,
            elseKw,
            returnKw
    )
}

object BuiltinTypes {
    val boolType = "bool"
    val intType = "int"

    val allTypes: Map<String, Type> = mapOf(
            boolType to BoolType,
            intType to IntType
    )
}

class Token(
        val type: TokenType,
        val source: String,
        val start: Int,
        val end: Int
) {
    private var text: String? = null

    fun getText(): String {
        val copy = text
        if (copy != null) {
            return copy
        }

        val result = source.substring(start, end)
        text = result
        return result
    }

    override fun toString(): String {
        return "Token(type=$type, text=${getText()})"
    }
}

class ParserException(val parser: Parser, override val message: String) : RuntimeException(message) {
    override fun toString(): String {
        return "$message, pos=${parser.currentPos}"
    }
}

class Parser(val source: String) {
    var currentPos = 0
    var reachedEof = false
    var savedToken: Token? = null
    var lastId: Int = 0

    private fun freshId(): Int {
        return lastId++
    }

    private fun skipSpaces() {
        while (currentPos < source.length && source[currentPos].isWhitespace()) {
            currentPos++
        }
    }

    // Auxiliary version of next token: can include pseudo-tokens such as start of a comment.
    private fun nextTokenAux(): Token {
        if (reachedEof) {
            throw ParserException(this, "trying to parse after EOF")
        }

        skipSpaces()

        if (currentPos == source.length) {
            reachedEof = true
            return Token(TokenType.EOF, source, currentPos, currentPos)
        }

        for (value in TokenType.values()) {
            if (value.string != null) {
                if (source.startsWith(value.string, currentPos)) {
                    val result = Token(value, source, currentPos, currentPos + value.string.length)
                    currentPos += value.string.length
                    return result
                }
            }
            if (value.pattern != null) {
                val match = value.pattern.find(source, currentPos)
                if (match != null && match.range.first == currentPos) {
                    currentPos = match.range.last + 1
                    val tokenType =
                            if (value == TokenType.IDENTIFIER && Keywords.allKeywords.contains(match.value)) {
                                TokenType.KEYWORD
                            } else {
                                value
                            }
                    return Token(tokenType, source, match.range.first, match.range.last + 1)
                }
            }
        }

        throw ParserException(this, "unrecognized character '${source[currentPos]}'")
    }

    private fun nextToken(): Token {
        while (true) {
            val token = nextTokenAux()
            when (token.type) {
                TokenType.COMMENT_START -> {
                    var commentEnd = source.indexOf('\n', currentPos)
                    if (commentEnd == -1) {
                        commentEnd = source.length
                    }
                    // Advance currentPos past newline.
                    currentPos = commentEnd + 1
                }
                TokenType.MULTILINE_COMMENT_START -> {
                    val commentEnd = source.indexOf("*/", currentPos)
                    if (commentEnd == -1) {
                        throw ParserException(this, "unclosed comment")
                    } else {
                        // Advance currentPos past comment end.
                        currentPos = commentEnd + 2
                    }
                }
                else -> return token
            }
        }
    }

    private fun peek(): Token {
        val copy = savedToken
        if (copy != null) {
            return copy
        }

        val result = nextToken()
        savedToken = result
        return result
    }

    private fun consume(): Token {
        val result = peek()
        savedToken = null
        return result
    }

    private fun consume(type: TokenType): Token {
        val result = peek()
        if (result.type != type) {
            throw ParserException(this, "expected $type, got ${result.type}")
        }

        savedToken = null
        return result
    }

    private fun consumeKeyword(keyword: String): Token {
        val token = consume(TokenType.KEYWORD)
        val text = token.getText()
        if (text != keyword) {
            throw ParserException(this, "expected $keyword, got $text")
        }

        return token
    }

    private fun expression(): Expr {
        val token = peek()
        return if (token.type == TokenType.KEYWORD) {
            if (token.getText() == Keywords.debugKw) {
                consume()
                Debug(freshId(), disjunction())
            } else {
                throw ParserException(this, "unexpected keyword ${token.getText()}")
            }
        } else {
            disjunction()
        }
    }

    private fun leftFold(tokens: Map<TokenType, BinaryOp>, callback: () -> Expr): Expr {
        var result = callback()

        while (true) {
            when (val op = tokens[peek().type]) {
                null -> {
                    return result
                }
                else -> {
                    consume()
                    result = Binary(freshId(), result, op, callback())
                }
            }
        }
    }

    private fun disjunction(): Expr {
        return leftFold(mapOf(TokenType.OR to BinaryOp.OR), ::conjunction)
    }

    private fun conjunction(): Expr {
        return leftFold(mapOf(TokenType.AND to BinaryOp.AND), ::equality)
    }

    private fun equality(): Expr {
        return leftFold(mapOf(TokenType.EQUALS_EQUALS to BinaryOp.EQ), ::sum)
    }

    private fun sum(): Expr {
        return leftFold(mapOf(TokenType.PLUS to BinaryOp.PLUS, TokenType.MINUS to BinaryOp.MINUS), ::summand)
    }

    private fun summand(): Expr {
        return leftFold(mapOf(TokenType.MULT to BinaryOp.MULT), ::factor)
    }

    private fun factor(): Expr {
        val token = peek()
        return when (token.type) {
            TokenType.MINUS -> {
                consume()
                Unary(freshId(), UnaryOp.NEGATE, factor())
            }
            TokenType.OPEN_PAREN -> {
                consume()
                val result = expression()
                consume(TokenType.CLOSE_PAREN)
                result
            }
            TokenType.LITERAL -> {
                consume()
                Literal(freshId(), token.getText().toLong())
            }
            TokenType.IDENTIFIER -> {
                consume()
                if (peek().type == TokenType.OPEN_PAREN) {
                    consume()
                    val args = mutableListOf<Expr>()
                    while (peek().type != TokenType.CLOSE_PAREN) {
                        args.add(expression())
                        if (peek().type == TokenType.COMMA) {
                            consume()
                        } else if (peek().type != TokenType.CLOSE_PAREN) {
                            throw ParserException(this, "expected comma or closing parenthesis when parsing function call")
                        }
                    }
                    consume(TokenType.CLOSE_PAREN)
                    Call(freshId(), token, args)
                } else {
                    Reference(freshId(), token)
                }
            }
            TokenType.KEYWORD -> {
                expression()
            }
            else -> {
                throw ParserException(this, "unexpected token ${token.type}")
            }
        }
    }

    fun isKeyword(keyword: String): Boolean {
        val token = peek()
        return token.type == TokenType.KEYWORD && token.getText() == keyword
    }

    fun statement(): Stmt {
        return if (isKeyword(Keywords.valKw)) {
            consume()
            val target = consume(TokenType.IDENTIFIER)
            consume(TokenType.EQUALS)
            val expr = expression()
            consume(TokenType.SEMICOLON)
            Assign(freshId(), true, Reference(freshId(), target), expr)
        } else if (isKeyword(Keywords.ifKw)) {
            consume()
            consume(TokenType.OPEN_PAREN)
            val condition = expression()
            consume(TokenType.CLOSE_PAREN)
            val consequent = block()

            val alternative: List<Stmt> = if (isKeyword(Keywords.elseKw)) {
                consume()
                block()
            } else {
                listOf()
            }

            If(freshId(), condition, consequent, alternative)
        } else if (isKeyword(Keywords.returnKw)) {
            consume()
            val expr = expression()
            consume(TokenType.SEMICOLON)
            Return(freshId(), expr)
        } else {
            val expr = expression()
            when (val type = peek().type) {
                TokenType.EQUALS -> {
                    consume()
                    val rhs = expression()
                    val result = Assign(freshId(), false, expr, rhs)
                    consume(TokenType.SEMICOLON)
                    result
                }
                TokenType.SEMICOLON -> {
                    consume()
                    ExprWrap(freshId(), expr)
                }
                else -> {
                    throw ParserException(this, "unexpected token $type")
                }
            }
        }
    }

    fun block(): List<Stmt> {
        consume(TokenType.OPEN_BRACE)
        val result = statements()
        consume(TokenType.CLOSE_BRACE)
        return result
    }

    fun statements(): List<Stmt> {
        val result = mutableListOf<Stmt>()

        while (true) {
            val type = peek().type
            if (type == TokenType.EOF || type == TokenType.CLOSE_BRACE) {
                break
            }
            result.add(statement())
        }

        return result
    }

    fun type(): Type {
        val name = consume(TokenType.IDENTIFIER)
        return BuiltinTypes.allTypes[name.getText()]
                ?: throw ParserException(this, "unknown type ${name.getText()}")
    }

    fun argument(): Argument {
        val name = consume(TokenType.IDENTIFIER)
        consume(TokenType.COLON)
        val argType = type()
        return Argument(freshId(), name, argType)
    }

    fun function(): Function {
        consumeKeyword(Keywords.funKw)
        val name = consume(TokenType.IDENTIFIER)
        consume(TokenType.OPEN_PAREN)

        val args = mutableListOf<Argument>()
        while (peek().type != TokenType.CLOSE_PAREN) {
            args.add(argument())

            if (peek().type == TokenType.COMMA) {
                // Trailing comma after last argument is explicitly allowed
                consume()
            } else if (peek().type != TokenType.CLOSE_PAREN) {
                throw ParserException(this, "expected comma or closing bracket after function parameter")
            }
        }

        consume(TokenType.CLOSE_PAREN)
        consume(TokenType.COLON)
        val returnType = type()
        val body = block()

        return Function(freshId(), name, args, returnType, body)
    }

    fun parse(): List<Function> {
        val result = mutableListOf<Function>()
        while (peek().type != TokenType.EOF) {
            result.add(function())
        }

        return result
    }
}
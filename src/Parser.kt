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
    OPEN_PAREN(string = "("),
    CLOSE_PAREN(string = ")"),
    SEMICOLON(string = ";"),
    EOF;
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
}

class ParserException(val parser: Parser, override val message: String): RuntimeException(message) {
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
                    return Token(value, source, match.range.first, match.range.last + 1)
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

    private fun expression(): Expr {
        var result = summand()

        while (true) {
            when (peek().type) {
                TokenType.PLUS -> {
                    consume()
                    result = Binary(freshId(), result, BinaryOp.PLUS, summand())
                }
                TokenType.MINUS -> {
                    consume()
                    result = Binary(freshId(), result, BinaryOp.MINUS, summand())
                }
                else -> {
                    return result
                }
            }
        }
    }

    private fun summand(): Expr {
        var result = factor()

        while (true) {
            when (peek().type) {
                TokenType.MULT -> {
                    consume()
                    result = Binary(freshId(), result, BinaryOp.MULT, factor())
                }
                else -> {
                    return result
                }
            }
        }
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
            else -> {
                throw ParserException(this, "unexpected token ${token.type}")
            }
        }
    }

    fun parse(): Expr {
        return expression()
    }
}
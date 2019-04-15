import Scanner._
import org.scalatest.FunSuite

class DFATests extends FunSuite {

  def assertIsIdentifier(word: String): Unit = {
    val tokens = tokenize(joos1wDFAs, word)
    assert(tokens.size == 3)
    val token: Token = tokens(1)
    assert(token.kind == "IDENTIFIER")
    assert(token.lexeme == word)
  }

  test("identifier test") {
    assertIsIdentifier("i")
    assertIsIdentifier("in")
    assertIsIdentifier("inte")
    assertIsIdentifier("int3")
  }

  def assertIsStringLiteral(word: String): Unit = {
    val tokens = tokenize(joos1wDFAs, word)
    assert(tokens.size == 3)
    val token: Token = tokens(1)
    assert(token.kind == "STRING_LITERAL")
    assert(token.lexeme == word)
  }

  test("string literal test") {
    assertIsStringLiteral("\"\\b\"")
    assertIsStringLiteral("\"\\t\"")
    assertIsStringLiteral("\"\\n\"")
    assertIsStringLiteral("\"\\f\"")
    assertIsStringLiteral("\"\\r\"")
    assertIsStringLiteral("\"\\0\"")
    assertIsStringLiteral("\"\\123\"")
    assertIsStringLiteral("\"\\13\"")
    assertIsStringLiteral("\"\\034\"")
    assertIsStringLiteral("\"\\2\\5\"")
    assertIsStringLiteral("\"\\77\"")
    assertIsStringLiteral("\"\\7\\7\"")
  }

  def assertIsCharLiteral(word: String): Unit = {
    val tokens = tokenize(joos1wDFAs, word)
    assert(tokens.size == 3)
    val token: Token = tokens(1)
    assert(token.kind == "CHAR_LITERAL")
    assert(token.lexeme == word)
  }

  test("char literal test") {
    assertIsCharLiteral("\'\\t\'")
    assertIsCharLiteral("\'\\b\'")
    assertIsCharLiteral("\'\\7\'")
    assertIsCharLiteral("\'\\77\'")
    assertIsCharLiteral("\'\\077\'")
    assertIsCharLiteral("\'\\\"\'")
    assertIsCharLiteral("\'\\\\\'")
  }

  def assertIsBoolLiteral(word: String): Unit = {
    val tokens = tokenize(joos1wDFAs, word)
    assert(tokens.size == 3)
    val token: Token = tokens(1)
    assert(token.kind == "BOOLEAN_LITERAL")
    assert(token.lexeme == word)
  }

  test("boolean literal test") {
    assertIsBoolLiteral("false")
    assertIsBoolLiteral("true")
  }

  def assertIsNullLiteral(word: String): Unit = {
    val tokens = tokenize(joos1wDFAs, word)
    assert(tokens.size == 3)
    val token: Token = tokens(1)
    assert(token.kind == "NULL_LITERAL")
    assert(token.lexeme == word)
  }

  test("null literal test") {
    assertIsNullLiteral("null")
  }

  def assertIsIntegerLiteral(word: String): Unit = {
    val tokens = tokenize(joos1wDFAs, word)
    assert(tokens.size == 3)
    val token: Token = tokens(1)
    assert(token.kind == "INTEGER_LITERAL")
    assert(token.lexeme == word)
  }

  test("integer literal test") {
    assertIsIntegerLiteral("0")
    assertIsIntegerLiteral("9123")
    assertIsIntegerLiteral("100")
  }

  def assertIsKewordLiteral(word: String): Unit = {
    val tokens = tokenize(joos1wDFAs, word)
    assert(tokens.size == 3)
    val token: Token = tokens(1)
    assert(keywords.contains(token.kind.toLowerCase()))
    assert(token.lexeme == word)
  }

  test("keywords test") {
    assertIsKewordLiteral("if")
    assertIsKewordLiteral("while")
    assertIsKewordLiteral("for")
  }

  def assertIsOperators(word: String, kind: String): Unit = {
    val tokens = tokenize(joos1wDFAs, word)
    assert(tokens.size == 3)
    val token: Token = tokens(1)
    assert(token.kind == kind)
    assert(token.lexeme == word)
  }

  test("operators test") {
    assertIsOperators("/", "DIV")
    assertIsOperators("%", "PERC")
    assertIsOperators("==", "EQ")
  }

  def assertIsSeparators(word: String, kind: String): Unit = {
    val tokens = tokenize(joos1wDFAs, word)
    assert(tokens.size == 3)
    val token: Token = tokens(1)
    assert(token.kind == kind)
    assert(token.lexeme == word)
  }

  test("separators test") {
    assertIsSeparators("(", "LEFT_PAREN")
    assertIsSeparators(")", "RIGHT_PAREN")
    assertIsSeparators("{", "LEFT_BRACE")
    assertIsSeparators("}", "RIGHT_BRACE")
    assertIsSeparators("[", "LEFT_SQUARE")
    assertIsSeparators("]", "RIGHT_SQUARE")
    assertIsSeparators(";", "SEMI")
    assertIsSeparators(",", "COMMA")
    assertIsSeparators(".", "PERIOD")
  }

  def assertCombined(word: String): Unit = {
    tokenize(joos1wDFAs, word)
  }

  test("Combination Tests"){
    //assertCombined("int 123; { helloworld true false }")
    assertCombined("{}")
    assertCombined("}{")
    assertCombined("// djweifjweof {}")
    assertCombined("/* djweifjweof {}" +
      "jfiwejiofjewi" +
      "fewjifjewo" +
      "*/")
  }

  test("char literal in string") {
    assertIsStringLiteral("\"abc\"")
    assertIsStringLiteral("\"u\"")
  }

}

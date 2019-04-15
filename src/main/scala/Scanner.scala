object Scanner {
  type State = String

  case class LexerException(message: String = "") extends Throwable {
    override def getMessage: String = message
  }

  case class DFA(
                  name: String,
                  alphabet: Set[Char] = Set.empty,
                  states: Set[State] = Set.empty,
                  start: State = "",
                  accepting: State => Boolean,
                  transition: PartialFunction[(State, Char), State] = PartialFunction.empty,
                  priority: Int = 0
                )

  case class Token(kind: String, lexeme: String = "") {
    override def toString: String = "%s(%s)".format(kind, lexeme)
  }

  def recognize(dfa: DFA, input: List[Char]): Boolean = {
    var curState = dfa.start
    input.foreach(c => {
      if (!dfa.alphabet.contains(c)) return false
      if (!dfa.transition.isDefinedAt((curState, c))) return false
      curState = dfa.transition((curState, c))
    })
    dfa.accepting(curState)
  }

  val StartState = "start"
  val JavaLetter: Set[Char] = ('a' to 'z').toSet ++ ('A' to 'Z').toSet ++ Set('_', '$')
  val JavaDigit: Set[Char] = ('0' to '9').toSet
  val JavaLetterOrDigit: Set[Char] = JavaLetter ++ JavaDigit

  val separators: List[String] = List(
    "(", ")", "{", "}", "[", "]", ";", ",", ".",
  )
  val separator_alphabets = separators.flatten.toSet

  val integers: List[String] = List.range(0, 9).map(_.toString)
  val booleans: List[String] = List("true", "false")
  val nulls: List[String] = List("null")


  val operators: List[String] = List(
    "=", ">", "<", "!", "?", ":", "==", "<=", ">=", "!=", "&&", "||", "+", "-", "*", "/", "%"
  )

  val nullAlphabets: Set[Char] = Set(
    'n', 'u', 'l'
  )

  val booleanAlphabets: Set[Char] = Set(
    't', 'r', 'u', 'e', 'f', 'a', 'l', 's',
  )

  val operatorAlphabets: Set[Char] = Set(
    '=', '>', '<', '!','~','?',':','&','|','+','-','*','/','^','%',
  )

  val keywords: List[String] = List(
    "abstract", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue",
    "default", "do", "double", "else", "extends", "final", "finally", "float", "for", "goto",
    "if", "implements", "import", "instanceof", "int", "interface", "long", "native", "new", "package",
    "private", "protected", "public", "return", "short", "static", "strictfp", "super", "switch", "synchronized",
    "this", "throw", "throws", "transient", "try", "void", "volatile", "while"
  )

  // Integer Literals
  val integer_literal_alphabets: Set[Char] = ('0' to '9').toSet
  val integer_literal_states: Set[State] = Set(
    "ZERO", "OTHER_INTEGERS"
  )
  val integer_literal_accepting_states: Set[State] = integer_literal_states
  val integerLiteralDFA = DFA(
    name = "integerLiteralDFA",
    alphabet = integer_literal_alphabets,
    states = integer_literal_states,
    start = StartState,
    accepting = integer_literal_accepting_states,
    transition = {
      case ("start", '0') => "ZERO"
      case ("start", x) if ('1' to '9').toSet.contains(x) => "OTHER_INTEGERS"
      case ("OTHER_INTEGERS", x) if ('0' to '9').toSet.contains(x) => "OTHER_INTEGERS"
    }
  )

  // Character Literals
  val character_literal_states: Set[State] = Set(
    "start", "single_quote", "CHAR_LITERAL", "char_escape_sequence", "char_escape_sequence1", "char_escape_sequence2", "char_octal_escape_zero_to_three1",
    "char_octal_escape_zero_to_three2", "char_octal_escape_zero_to_three3", "char_raw_input_character",
  )
  val char_literal_accepting_state: Set[State] = Set("CHAR_LITERAL")

  // String Literals
  val EscapeSequence: Set[Char] = Set('b', 't', 'n', 'f', 'r', '\"', '\'', '\\')
  val ZeroToThree: Set[Char] = ('0' to '3').toSet
  val OctalDigit: Set[Char] = ('0' to '7').toSet
  val string_literal_alphabets: Set[Char] = (0 to 127).map(x => x.toChar).toSet

  val string_literal_states: Set[State] = Set(
    "start", "double_quote", "STRING_LITERAL", "string_escape_sequence", "string_escape_sequence1", "string_escape_sequence2", "string_octal_escape_zero_to_three1",
    "string_octal_escape_zero_to_three2", "string_octal_escape_zero_to_three3", "string_raw_input_character",
  )
  val string_literal_accepting_state: Set[State] = Set("STRING_LITERAL")

  val charLiteralDFA = DFA(
    name = "charLiteralDFA",
    alphabet = string_literal_alphabets,
    states = character_literal_states,
    start = StartState,
    accepting = char_literal_accepting_state,
    transition = {
      case ("start", '\'') => "single_quote"
      case ("single_quote", '\\') => "char_escape_sequence"
      case ("single_quote", x) if x != '\\' => "char_literal1"
      case ("char_literal1", '\'') => "CHAR_LITERAL"

      case ("char_escape_sequence", x) if EscapeSequence.contains(x) => "char_escape_sequence1"
      case ("char_escape_sequence1", '\'') => "CHAR_LITERAL"

      case ("char_escape_sequence", x) if ZeroToThree.contains(x) => "char_octal_escape_zero_to_three1"
      case ("char_escape_sequence", x) if OctalDigit.contains(x) => "char_octal_escape_zero_to_three2"
      // \ ZeroToThree OctalDigit OctalDigit
      case ("char_octal_escape_zero_to_three1", x) if OctalDigit.contains(x) => "char_octal_escape_zero_to_three2"
      case ("char_octal_escape_zero_to_three2", x) if OctalDigit.contains(x) => "char_octal_escape_zero_to_three3"
      // quit anytime in the escape sequence
      case ("char_octal_escape_zero_to_three1", '\'') => "CHAR_LITERAL"
      case ("char_octal_escape_zero_to_three2", '\'') => "CHAR_LITERAL"
      case ("char_octal_escape_zero_to_three3", '\'') => "CHAR_LITERAL"
    }
  )

  val stringLiteralDFA = DFA(
    name = "stringLiteralDFA",
    alphabet = string_literal_alphabets,
    states = string_literal_states,
    start = StartState,
    accepting = string_literal_accepting_state,
    transition = {
      case (StartState, '\"') => "double_quote"
      case ("double_quote", '\"') => "STRING_LITERAL"
      case ("double_quote", '\\') => "string_escape_sequence"
      case ("double_quote", _) => "double_quote"

      case ("string_escape_sequence", x) if EscapeSequence.contains(x) => "string_escape_sequence1"
      case ("string_escape_sequence1", '\"') => "STRING_LITERAL"
      case ("string_escape_sequence1", '\\') => "string_escape_sequence"
      case ("string_escape_sequence1", _) => "double_quote"

      case ("string_escape_sequence", x) if ZeroToThree.contains(x) => "string_octal_escape_zero_to_three1"
      case ("string_escape_sequence", x) if OctalDigit.contains(x) => "string_octal_escape_zero_to_three2"
      // \ ZeroToThree OctalDigit OctalDigit
      case ("string_octal_escape_zero_to_three1", x) if OctalDigit.contains(x) => "string_octal_escape_zero_to_three2"
      case ("string_octal_escape_zero_to_three2", x) if OctalDigit.contains(x) => "string_octal_escape_zero_to_three3"
      // quit anytime in the escape sequence
      case ("string_octal_escape_zero_to_three1", '\"') => "STRING_LITERAL"
      case ("string_octal_escape_zero_to_three1", '\\') => "string_escape_sequence"
      case ("string_octal_escape_zero_to_three1", _) => "double_quote"
      case ("string_octal_escape_zero_to_three2", '\"') => "STRING_LITERAL"
      case ("string_octal_escape_zero_to_three2", '\\') => "string_escape_sequence"
      case ("string_octal_escape_zero_to_three2", _) => "double_quote"
      case ("string_octal_escape_zero_to_three3", '\"') => "STRING_LITERAL"
      case ("string_octal_escape_zero_to_three3", '\\') => "string_escape_sequence"
      case ("string_octal_escape_zero_to_three3", _) => "double_quote"
    }
  )
  // String Literals

  def buildSubStates(input: List[String]): Set[State] = {
    var result: Set[State] = Set()
    input.foreach(key => {
      for (i <- 1 until key.length) {
        result += key.slice(0, i)
      }
    })
    result
  }

  def buildTransitionPartialFunction(input: List[String]): Map[(State, Char), State] = {
    var result: Map[(State, Char), State] = Map()
    for (key <- input) {
      for (i <- 1 to key.length) {
        var before: State = ""
        if (i == 1) before = StartState
        else before = key.slice(0, i - 1)
        var next = key.slice(0, i)
        if (keywords.contains(before)) before = before.toUpperCase()
        if (keywords.contains(next)) next = next.toUpperCase()
        result += ((before, key.charAt(i - 1)) -> next)
      }
    }
    result
  }

  val booleanLiteralDFA = DFA(
    name = "booleanLiteralDFA",
    alphabet = booleanAlphabets,
    states = buildSubStates(booleans),
    start = StartState,
    accepting = (x: State) => {
      booleans.contains(x)
    },
    transition = buildTransitionPartialFunction(booleans)
  )

  val nullLiteralDFA = DFA(
    name = "nullLiteralDFA",
    alphabet = nullAlphabets,
    states = buildSubStates(nulls),
    start = StartState,
    accepting = (x: State) => {
      nulls.contains(x)
    },
    transition = buildTransitionPartialFunction(nulls)
  )

  val commentDFA = DFA(
    name = "commentDFA",
    alphabet = string_literal_alphabets,
    states = Set("slash", "comment2", "comment3", "COMMENT", "eol_comment", "COMMENT_LF", "COMMENT_CR", "COMMENT_CR_LF"),
    start = StartState,
    accepting = (x: State) => {
      Set("EOL_COMMENT", "COMMENT", "COMMENT_LF", "COMMENT_CR", "COMMENT_CR_LF").contains(x)
    },
    transition = {
      case ("start", '/') => "slash"
      case ("slash", '*') => "comment2"
      case ("comment2", '*') => "comment3"
      case ("comment2", _) => "comment2"
      case ("comment3", '/') => "COMMENT"
      case ("comment3", '*') => "comment3"
      case ("comment3", _) => "comment2"
      case ("slash", '/') => "EOL_COMMENT"
      case ("EOL_COMMENT", '\n') => "COMMENT_LF"
      case ("EOL_COMMENT", '\r') => "COMMENT_CR"
      case ("COMMENT_CR", '\n') => "COMMENT_CR_LF"
      case ("EOL_COMMENT", _) => "EOL_COMMENT"
    }
  )

  val seperatorsDFA = DFA(
    name = "seperatorDFA",
    alphabet = separator_alphabets,
    states = Set("LEFT_PAREN", "RIGHT_PAREN", "LEFT_BRACE", "RIGHT_BRACE", "LEFT_SQUARE", "RIGHT_SQUARE", "SEMI", "COMMA", "PERIOD"),
    start = StartState,
    accepting = (x: State) => {
      Set("LEFT_PAREN", "RIGHT_PAREN", "LEFT_BRACE", "RIGHT_BRACE", "LEFT_SQUARE", "RIGHT_SQUARE", "SEMI", "COMMA", "PERIOD").contains(x)
    },
    transition = {
      case (StartState, '(') => "LEFT_PAREN"
      case (StartState, ')') => "RIGHT_PAREN"
      case (StartState, '{') => "LEFT_BRACE"
      case (StartState, '}') => "RIGHT_BRACE"
      case (StartState, '[') => "LEFT_SQUARE"
      case (StartState, ']') => "RIGHT_SQUARE"
      case (StartState, ';') => "SEMI"
      case (StartState, ',') => "COMMA"
      case (StartState, '.') => "PERIOD"
    }
  )

  val operatorsDFA = DFA(
    name = "operatorDFA",
    alphabet = operatorAlphabets,
    states = buildSubStates(operators),
    start = StartState,
    accepting = (x: State) => {
      Set("ASSIGN", "GREATER", "LESS", "NOT", "PLUS", "MINUS", "MULTI", "DIV", "PERC", "EAND", "EXOR", "EOR", "COLON", "EQ", "GT", "LT", "NEQ", "LAND", "LOR").contains(x)
    },
    transition = {
      /* use case in joos1w:
      =
      > Comparison
      < Comparison
      ! Eager boolean
      ~
      ?
      :
      == Comparison
      <= Comparison
      >= Comparison
      != Comparison
      && Lazy Boolean
      || Lazy Boolean
      ++
      --
      + Arithmetic, Implicit String Concatenation
      - Arithmetic
      * Arithmetic
      / Arithmetic
      & Eager boolean
      | Eager boolean
      ^
      % Arithmetic
      <<
      >>
      >>>
      +=
      -=
      *=
      /=
      &=
      |=
      ^=
      %=
      <<=
      >>=
      >>>=
       */
      case (StartState, '=') => "ASSIGN"
      case (StartState, '>') => "GREATER"
      case (StartState, '<') => "LESS"
      case (StartState, '!') => "NOT"
      case (StartState, '+') => "PLUS"
      case (StartState, '-') => "MINUS"
      case (StartState, '*') => "MULTI"
      case (StartState, '/') => "DIV"
      case (StartState, '%') => "PERC"
      case (StartState, '&') => "EAND"
      case (StartState, '^') => "EXOR"
      case (StartState, '|') => "EOR"
      case (StartState, ':') => "COLON"
      case ("ASSIGN", '=') => "EQ" // ==
      case ("GREATER", '=') => "GT" // >=
      case ("LESS", '=') => "LT" // <=
      case ("NOT", '=') => "NEQ" // !=
      case ("EAND", '&') => "LAND" // &&
      case ("EOR", '|') => "LOR" // ||
    }
  )

  val keywordsDFA = DFA(
    name = "keywordsDFA",
    alphabet = ('a' to 'z').toSet,
    states = buildSubStates(keywords),
    start = StartState,
    accepting = (x: State) => {
      keywords.contains(x.toLowerCase())
    },
    transition = buildTransitionPartialFunction(keywords),
    priority = 5
  )


  val identifierDFA = DFA(
    name = "identifierDFA",
    alphabet = JavaLetterOrDigit,
    states = Set(StartState, "IDENTIFIER"),
    start = StartState,
    accepting = (x: State) => {
      x == "IDENTIFIER"
    },
    transition = {
      case (StartState, x)  if JavaLetter.contains(x) => "IDENTIFIER"
      case ("IDENTIFIER", x) if JavaLetterOrDigit.contains(x) => "IDENTIFIER"
    }
  )

  val whitespaceDFA = DFA(
    name = "whitespaceDFA",
    alphabet = Set(' ', '\r', '\t', '\n'),
    start = StartState,
    accepting = (x: State) => {
      Set("NEWLINE_LF", "NEWLINE_CR", "NEWLINE_CR_LF", "WHITESPACE", "TAB").contains(x)
    },
    transition = {
      case (StartState, '\n') => "NEWLINE_LF"
      case (StartState, '\r') => "NEWLINE_CR"
      case ("NEWLINE_CR", '\n') => "NEWLINE_CR_LF"
      case (StartState, ' ') => "WHITESPACE"
      case (StartState, '\t') => "TAB"
    }
  )

  val joos1wDFAs: List[DFA] = commentDFA :: identifierDFA :: keywordsDFA :: operatorsDFA :: seperatorsDFA ::
          booleanLiteralDFA :: nullLiteralDFA :: stringLiteralDFA :: charLiteralDFA :: integerLiteralDFA ::
          whitespaceDFA :: Nil


  def tokenize(dfas: List[DFA], input: String): Seq[Token] = {

    def nextToken(dfa: DFA, input: List[Char], state: State, backtrack: (List[Char], State)): (List[Char], State) = {
      if (input.isEmpty) backtrack
      else if (!dfa.transition.isDefinedAt((state, input.head)))
        backtrack
      else {
        val nextState = dfa.transition((state, input.head))
        if (dfa.accepting(nextState)){
          nextToken(dfa, input.tail, nextState, (input.tail, nextState))
        }
        else nextToken(dfa, input.tail, nextState, backtrack)
      }
    }


    def maximalMunch(input: List[Char], accumulator: List[Token] = List.empty): Seq[Token] = {
      if (input.isEmpty) accumulator
      else {
        var curPriority = -1
        var curLength = 0
        var curState = ""

        dfas.foreach(dfa => {
          val (rest, state) = nextToken(dfa, input, dfa.start, (input, dfa.start))
          val len = input.size - rest.size
          if (len > curLength || len == curLength && dfa.priority > curPriority) {
            curLength = len
            curPriority = dfa.priority
            curState = state
          }
        })
        if (curLength == 0) {
          throw LexerException("Cannot tokenize from %s".format(input.slice(0, 40).mkString))
        } else {
          val (left, right) = input.splitAt(curLength)
          maximalMunch(right, accumulator ++ Seq(Token(curState, left.mkString)))
        }
      }
    }

    def clean(token: Token): Option[Token] = {
      if (booleans.contains(token.lexeme.toLowerCase)) return Some(Token("BOOLEAN_LITERAL", token.lexeme))
    else if (nulls.contains(token.lexeme.toLowerCase)) return Some(Token("NULL_LITERAL", token.lexeme))
    else if (Set("ZERO", "OTHER_INTEGERS").contains(token.kind)) return Some(Token("INTEGER_LITERAL", token.lexeme))
    else if (Set("EOL_COMMENT", "COMMENT", "COMMENT_LF", "COMMENT_CR", "COMMENT_CR_LF").contains(token.kind)) return None
    else if (Set("NEWLINE_LF", "NEWLINE_CR", "NEWLINE_CR_LF", "WHITESPACE", "TAB").contains(token.kind)) return None
    Some(token)
  }

    Seq(Token("BOF")) ++ maximalMunch(input.toList).flatMap(clean) ++ Seq(Token("EOF"))

  }

  def tokenizeJoos1w(input: String): Seq[Token] = {
    tokenize(joos1wDFAs, input)
  }
}

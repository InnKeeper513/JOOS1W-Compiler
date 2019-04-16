import A1.Scanner.tokenizeJoos1w
import A1.Parser.parseJoos1w
import A1.Weeding._
import org.scalatest.FunSuite

class WeederTests extends FunSuite {

  test("integer overflow2"){
    val str =
    """
      |public class Helloworld{
      |public void integer_range_test(){
      | int a = 3000;
      | int overflow1 = -21474836479;
      |}
      |}
    """.stripMargin

    val tokens = tokenizeJoos1w(str)
    val root = parseJoos1w(tokens)

    intercept[Throwable]{
      IntegerRangeWeeder.weed(root)
    }
  }

  test("integer overflow1"){
    val str =
    """
      |public class Helloworld{
      |public void integer_range_test(){
      | int a = 3000;
      | int overflow1 = 21474836479;
      |}
      |}
    """.stripMargin

    val tokens = tokenizeJoos1w(str)
    val root = parseJoos1w(tokens)

    intercept[Throwable]{
      IntegerRangeWeeder.weed(root)
    }
  }

  test("class abstract final") {

    val str =
      """
        |public abstract final class Helloworld {
        |
        |}
      """.stripMargin

    val tokens = tokenizeJoos1w(str)
    val root = parseJoos1w(tokens)

    intercept[Throwable] {
      AbstractFinalClassWeeder.weed(root)
    }
  }

  test("abstract class body") {

    var str =
      """
        |public class Helloworld {
        |abstract void greet() {}
        |}
      """.stripMargin

    var tokens = tokenizeJoos1w(str)
    var root = parseJoos1w(tokens)

    var exception = intercept[Throwable] {
      AbstractClassBodyWeeder.weed(root)
    }

    str =
      """
        |public class Helloworld {
        |public void greet() {}
        |}
      """.stripMargin
    tokens = tokenizeJoos1w(str)
    root = parseJoos1w(tokens)
    AbstractClassBodyWeeder.weed(root)

    str =
      """
        |public class Helloworld {
        |public void greet() ;
        |}
      """.stripMargin
    tokens = tokenizeJoos1w(str)
    root = parseJoos1w(tokens)
    exception = intercept[Throwable] {
      AbstractClassBodyWeeder.weed(root)
    }

    str =
      """
        |public class Helloworld {
        |abstract void greet() ;
        |}
      """.stripMargin
    tokens = tokenizeJoos1w(str)
    root = parseJoos1w(tokens)
    AbstractClassBodyWeeder.weed(root)
  }

  test("method modifier") {
    var str =
      """
        |public class Helloworld {
        |abstract static void greet() ;
        |}
      """.stripMargin

    var tokens = tokenizeJoos1w(str)
    var root = parseJoos1w(tokens)

    var exception = intercept[Throwable] {
      MethodModifierWeeder.weed(root)
    }

    str =
      """
        |public class Helloworld {
        |static final void greet() {}
        |}
      """.stripMargin
    tokens = tokenizeJoos1w(str)
    root = parseJoos1w(tokens)
    exception = intercept[Throwable] {
      MethodModifierWeeder.weed(root)
    }

    str =
      """
        |public class Helloworld {
        |native void greet() {}
        |}
      """.stripMargin
    tokens = tokenizeJoos1w(str)
    root = parseJoos1w(tokens)
    exception = intercept[Throwable] {
      MethodModifierWeeder.weed(root)
    }

  }

  test("void only used in return type") {
    val str =
      """
        |public class Helloworld {
        | public static void main(void[] args) {
        |
        | }
        |}
      """.stripMargin
    val tokens = tokenizeJoos1w(str)
    intercept[Throwable] {
      parseJoos1w(tokens)
    }

  }

  test("every class should have explicit constructor") {
    val str =
      """
        |public class Helloworld {
        |}
      """.stripMargin
    val tokens = tokenizeJoos1w(str)
    val root = parseJoos1w(tokens)
    intercept[Throwable] {
      ExplicitConstructorWeeder.weed(root)
    }
  }

  test("no field can be final") {
    val filename = "resources/testcases/a1/Je_1_FinalField_NoInitializer.java"
    val source = io.Source.fromFile(filename)
    val str = (for (line <- source.getLines()) yield line).mkString("\n")
    val tokens = tokenizeJoos1w(str)
    val root = parseJoos1w(tokens)
    intercept[Throwable] {
      NoFieldFinalWeeder.weed(root)
    }
  }

  test("cast weeder test") {
    val str =
      """
        |import A;
        |public class Helloworld {
        |  public Helloworld(int i, int j) { i = (A.B)j; }
        |}
      """.stripMargin
    val tokens = tokenizeJoos1w(str)
    val root = parseJoos1w(tokens)
    weedJoos1w(root)
  }

  test("cast weeder test 2") {
    val str =
      """
        |import A.B;
        |public class Helloworld {
        |  public Helloworld(int i, int j) { i = (B)j; }
        |}
      """.stripMargin
    val tokens = tokenizeJoos1w(str)
    val root = parseJoos1w(tokens)
    weedJoos1w(root)
  }

  test("cast weeder test 3") {
    val str =
      """
        |import A.B;
        |public class Helloworld {
        |  public Helloworld(int i, int j) { i = (1 + A.B + 5)j; }
        |}
      """.stripMargin
    val tokens = tokenizeJoos1w(str)
    val root = parseJoos1w(tokens)
    intercept[Throwable] {
      weedJoos1w(root)
    }
  }

  test("cast weeder test 4") {
    val str =
      """
        |import A.B;
        |public class Helloworld {
        |  public Helloworld(int i, int j) { i = (A.B)(1 + A.B + 5)j; }
        |}
      """.stripMargin
    val tokens = tokenizeJoos1w(str)
    val root = parseJoos1w(tokens)
    intercept[Throwable] {
      weedJoos1w(root)
    }
  }



}

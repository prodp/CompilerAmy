package amyc.test

import amyc.parsing._
import org.junit.Test

class LexerTests extends TestSuite {
  val pipeline = Lexer andThen DisplayTokens

  val baseDir = "test/resources/lexer"

  val outputExt = "txt"

  /*@Test def testKeywords = shouldOutput("Keywords")

  @Test def testSingleAmp = shouldFail("SingleAmp")

  // TODO (optionally): Add your own tests

  @Test def testToken = shouldOutput("String")

  @Test def testHello = shouldOutput("Hello")

  @Test def testTest1 = shouldOutput("test1")

  @Test def testHelloOriginal = shouldOutput("HelloOriginal")

  @Test def testOperator = shouldOutput("Operator")

  @Test def testMultiFail = shouldFail("MultiCommentFail")

  @Test def testMultiLine = shouldOutput("MultiLine")*/

  //@Test def testMultiCommentFail2 = shouldFail("MultiCommentFail2")

  // NEW TESTS
  //@Test def testVarReassign = shouldOutput("varReassign")
}

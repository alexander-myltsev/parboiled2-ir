import org.scalatest.{MustMatchers, WordSpec}

class SimpleParserSpec extends WordSpec with MustMatchers {

  class SimpleParserRunner(val input: String) extends Runner {
    val parser =
      ParserDef("SimpleParser", Seq(
        RuleDefinitionDef("abRule", Seq(), StringLiteral("ab"))
      ))
  }

  "SimpleParser" must {
    "parse correctly" in {
      new SimpleParserRunner("ab").parse() mustBe true
      new SimpleParserRunner("ac").parse() mustBe false
    }
  }
}

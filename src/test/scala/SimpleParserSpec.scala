import org.scalatest.{MustMatchers, WordSpec}

class SimpleParserSpec extends WordSpec with MustMatchers {
  "SimpleStringParser" must {
    class SimpleStringParser(val input: String) extends Runner {
      val parser =
        ParserDef("SimpleParser", Seq(
          RuleDefinitionDef("", Seq(), StringLiteral("ab"))
        ))
    }

    "parse correctly" in {
      new SimpleStringParser("ab").parse() mustBe true
      new SimpleStringParser("ac").parse() mustBe false
    }
  }

  "SimpleSeqFirstOfParser" must {
    class SimpleSeqFirstOfParser(val input: String) extends Runner {
      // rule { (ab ~ cd) | ef }

      val parser =
        ParserDef("SimpleParser", Seq(
          RuleDefinitionDef("", Seq(), FirstOf(Sequence(StringLiteral("ab"), StringLiteral("cd")), StringLiteral("ef")))
        ))
    }

    "parse correctly" in {
      new SimpleSeqFirstOfParser("abcd").parse() mustBe true
      new SimpleSeqFirstOfParser("ef").parse() mustBe true
      new SimpleSeqFirstOfParser("abf").parse() mustBe false
      new SimpleSeqFirstOfParser("e").parse() mustBe false
    }
  }
}

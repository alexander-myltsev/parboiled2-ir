import org.scalatest.{MustMatchers, WordSpecLike}

class SimpleSeqFirstOfParserSpec extends Runner with WordSpecLike with MustMatchers {
  // rule { (ab ~ cd) | ef }
  val parser =
    ParserDef("SimpleSeqFirstOfParser", Seq(
      RuleDefinitionDef("", Seq(), FirstOf(Sequence(StringLiteral("ab"), StringLiteral("cd")), StringLiteral("ef")))
    ))

  "must parse correctly" in {
    parse("abcd") mustBe true
    parse("ef") mustBe true
    parse("abf") mustBe false
    parse("e") mustBe false
  }
}

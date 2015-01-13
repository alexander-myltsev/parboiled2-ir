import org.scalatest.{MustMatchers, WordSpecLike}

class ParserCallSpec extends Runner with WordSpecLike with MustMatchers {
  val externalParsers = ParserDef("AParser", Seq(
    RuleDefinitionDef("a", Seq(), StringLiteral("a"))
  ))

  val parser =
    ParserDef("ParserCallParser", Seq(
      RuleDefinitionDef("b", Seq(), RuleCall("AParser.a"))
    ))

  "must parse correctly" in { pending
    parse("a") mustBe true
    parse("b") mustBe false
  }
}

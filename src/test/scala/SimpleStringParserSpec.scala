import org.scalatest.{MustMatchers, WordSpecLike}

class SimpleStringParserSpec extends Runner with WordSpecLike with MustMatchers {
  val parser =
    ParserDef("SimpleStringParser", Seq(
      RuleDefinitionDef("", Seq(), StringLiteral("ab"))
    ))

  "must parse correctly" in {
    parse("ab") mustBe true
    parse("ac") mustBe false
  }
}

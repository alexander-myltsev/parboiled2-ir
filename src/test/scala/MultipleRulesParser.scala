import org.scalatest.{MustMatchers, WordSpecLike}

class MultipleRulesParser extends Runner with WordSpecLike with MustMatchers {
  // S ::= A ~ B
  // A ::= "a"
  // B ::= "b"
  val parser =
    ParserDef("MultipleRulesParser", Seq(
      RuleDefinitionDef("aa", Seq(), Sequence(RuleCall("a"), RuleCall("b"))),
      RuleDefinitionDef("a", Seq(), StringLiteral("a")),
      RuleDefinitionDef("b", Seq(), StringLiteral("b"))
    ))

  "must parse correctly" in {
    parse("ab") mustBe true
    parse("aa") mustBe false
  }
}

import org.scalatest.{MustMatchers, WordSpecLike}

class RuleInlinerSpec extends RunnerOptInliner with WordSpecLike with MustMatchers {
  override val parser =
    ParserDef("RuleInliner", Seq(
      RuleDefinitionDef("abc", Seq(), Sequence(Sequence(RuleCall("a"), RuleCall("b")), RuleCall("c"))),
      RuleDefinitionDef("a", Seq(), StringLiteral("a")),
      RuleDefinitionDef("b", Seq(), StringLiteral("b")),
      RuleDefinitionDef("c", Seq(), StringLiteral("c"))
    ))

  "must parse correctly" in {
    parse("abc") mustBe true
    parse("aac") mustBe false
  }

  "must inline rules" in {
    optimizedParser mustBe ParserDef("RuleInliner", Seq(
      RuleDefinitionDef("abc", Seq(), Sequence(Sequence(StringLiteral("a"), StringLiteral("b")), StringLiteral("c"))),
      RuleDefinitionDef("a", Seq(), StringLiteral("a")),
      RuleDefinitionDef("b", Seq(), StringLiteral("b")),
      RuleDefinitionDef("c", Seq(), StringLiteral("c"))
    ))
  }
}

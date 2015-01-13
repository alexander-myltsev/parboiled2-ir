import org.scalatest.{MustMatchers, WordSpecLike}

class ParametrizedRulesSpec extends Runner with WordSpecLike with MustMatchers {
  val parser =
    ParserDef("ParametrizedRulesParser", Seq(
      RuleDefinitionDef("a", Seq(), RuleCall("b", Seq(StringLiteral("a"), StringLiteral("b")))),
      RuleDefinitionDef("b", Seq("x", "y"), FirstOf(Parameter("x"), Parameter("y")))
    ))

  "must call parametrized rule" in { pending
    optimizedParser mustBe ParserDef("ParametrizedRulesParser", Seq(
      RuleDefinitionDef("a", Seq(), RuleCall("b")),
      RuleDefinitionDef("b_1", Seq(), FirstOf(StringLiteral("a"), StringLiteral("b")))
    ))
  }
}

import org.scalatest.{MustMatchers, WordSpecLike}

class ConstantFoldingsSpec extends Runner with WordSpecLike with MustMatchers {
  val parser =
    ParserDef("ConstantFoldingsParser", Seq(
      RuleDefinitionDef("a", Seq(), Sequence(Sequence(StringLiteral("a"), StringLiteral("b")), StringLiteral("c"))),
      RuleDefinitionDef("b", Seq(), FirstOf(FirstOf(StringLiteral("foo"), StringLiteral("bar")), StringLiteral("baz"))), // https://github.com/sirthias/parboiled2/issues/115
      RuleDefinitionDef("c", Seq(), FirstOf(StringLiteral("aba"), StringLiteral("abc")))
    ))

  "must fold constants" in { pending
    optimizedParser mustBe ParserDef("ConstantFoldingsParser", Seq(
      RuleDefinitionDef("a", Seq(), StringLiteral("abc")),
      RuleDefinitionDef("b", Seq(), StringLiteral("foo")),
      RuleDefinitionDef("c", Seq(), Sequence(StringLiteral("ab"), FirstOf(StringLiteral("a"), StringLiteral("c"))))
    ))
  }
}

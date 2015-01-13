import org.scalatest.{MustMatchers, WordSpecLike}

class DeadRuleEliminationSpec extends RunnerOptDeadRuleElimination with WordSpecLike with MustMatchers {
  /*
  class RuleInlinerParser {
    def abc = rule { ((a() ~ b()) ~ c()) }
    def a = rule { "a" }
    def b = rule { "b" }
    def c = rule { "c" | "d" }
  }

  class RuleInlinerOptimizedParser {
    def abc = rule { (("a" ~ "b") ~ c()) }
    def c = rule { "c" | "d" }
  }
  */

  val parser =
    ParserDef("MultipleRulesParser", Seq(
      RuleDefinitionDef("aa", Seq(), Sequence(Sequence(RuleCall("a"), RuleCall("b")), RuleCall("c"))),
      RuleDefinitionDef("a", Seq(), StringLiteral("a")),
      RuleDefinitionDef("b", Seq(), StringLiteral("b")),
      RuleDefinitionDef("c", Seq(), FirstOf(StringLiteral("c"), StringLiteral("d")))
    ))

  "must parse correctly" in { pending
    parse("abc") mustBe true
    parse("abd") mustBe true
    parse("aac") mustBe false
  }

  "must drop dead rules" in { pending
    optimizedParser mustBe ParserDef("RuleInliner", Seq(
      RuleDefinitionDef("abc", Seq(), Sequence(Sequence(StringLiteral("a"), StringLiteral("b")), RuleCall("c"))),
      RuleDefinitionDef("c", Seq(), FirstOf(StringLiteral("c"), StringLiteral("d")))
    ))
  }
}

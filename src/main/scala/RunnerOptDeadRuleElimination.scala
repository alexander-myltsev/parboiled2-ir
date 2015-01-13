abstract class RunnerOptDeadRuleElimination extends RunnerOptInliner {
  override def optimizedParser: ParserDef = {
    val startRule = optimizedParser.rules(0).asInstanceOf[RuleDefinitionDef]

    // ToDo: filter rules that are not reachable from `startRule`

    ???
  }
}

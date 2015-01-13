abstract class RunnerOptInliner extends Runner {
  override def optimizedParser: ParserDef = {
    val inlineableRules = super.optimizedParser.rules.filter {
      case RuleDefinitionDef(_, _, sl: StringLiteral) => true
      case _ => false
    }.map { case rd @ RuleDefinitionDef(Const(name), _, body) => (name, body) }
     .toMap

    val inlinedRules = super.optimizedParser.rules.map { case rd @ RuleDefinitionDef(_, _, body) =>
      def inlineRules(r: Rep[Rule]): Rep[Rule] = r match {
        case Sequence(lhs, rhs) =>
          val lhsN = lhs match {
            case RuleCall(Const(lhsName)) => inlineableRules.getOrElse(lhsName, inlineRules(lhs))
            case l => inlineRules(l)
          }
          val rhsN = rhs match {
            case RuleCall(Const(rhsName)) => inlineableRules.getOrElse(rhsName, inlineRules(rhs))
            case l => inlineRules(l)
          }
          Sequence(lhsN, rhsN)

        case FirstOf(lhs, rhs) =>
          val lhsN = lhs match {
            case RuleCall(Const(lhsName)) => inlineableRules.getOrElse(lhsName, inlineRules(lhs))
            case l => inlineRules(l)
          }
          val rhsN = rhs match {
            case RuleCall(Const(rhsName)) => inlineableRules.getOrElse(rhsName, inlineRules(rhs))
            case l => inlineRules(l)
          }
          FirstOf(lhsN, rhsN)

        case l => l
      }
      rd.copy(body = inlineRules(body))
    }
    super.optimizedParser.copy(rules = inlinedRules)
  }
}

abstract class Runner extends ParboiledOpsExp {
  val parser: ParserDef
  def optimizedParser: ParserDef = parser

  def parse(input: String): Boolean = {
    var cursor = 0

    val startRule = optimizedParser.rules(0).asInstanceOf[RuleDefinitionDef]
    def matchRule(r: Rep[Rule]): Boolean = r match {
      case StringLiteral(Const(str)) => (str.length + cursor <= input.length) &&
        (input.substring(cursor, cursor + str.length) == str) && { cursor += str.length; true }

      case Sequence(lhs, rhs) => matchRule(lhs) && matchRule(rhs)

      case FirstOf(lhs, rhs) => val cur = cursor; matchRule(lhs) || { cursor = cur; matchRule(rhs) }

      case RuleCall(Const(callingRuleName)) =>
        optimizedParser.rules.find { case RuleDefinitionDef(Const(name), _, _) => callingRuleName == name } match {
          case Some(RuleDefinitionDef(_, _, body)) => matchRule(body)
          case None => throw new Exception("Undefined rule to call")
        }

      case _ => ???
    }
    matchRule(startRule.body)
  }

  def toPseudoScala[T](exp: Def[T]): String = exp match {
    case ParserDef(Const(name), rules) =>
      s"""class ${name}Parser {
         |  ${rules.map(toPseudoScala).mkString("\n  ")}
         |}
       """.stripMargin
    case RuleDefinitionDef(Const(name), argNames, body) =>
      s"def $name = rule { ${toPseudoScala(body)} }"
    case StringLiteral(Const(str)) => s""""$str""""
    case Sequence(lhs, rhs) => s"(${toPseudoScala(lhs)} ~ ${toPseudoScala(rhs)})"
    case FirstOf(lhs, rhs) => s"(${toPseudoScala(lhs)} ~ ${toPseudoScala(rhs)})"
    case RuleCall(Const(callingRuleName)) => s"$callingRuleName()"
  }
}

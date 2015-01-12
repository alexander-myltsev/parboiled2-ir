trait Expressions {
  abstract class Exp[T]
  case class Const[T](x: T) extends Exp[T]

  implicit def stringToConst(v: String): Const[String] = Const(v)

  type Def[T]
}

trait Base {
  type Rep[T]
}

trait ParboiledOps extends Base {
  abstract class Parser
  abstract class RuleDefinition
  abstract class Rule

  object Parser {
    def create(name: Rep[String], rules: Seq[Rep[RuleDefinition]]): Rep[Parser] = create_parser(name, rules)
  }

  def create_parser(name: Rep[String], rules: Seq[Rep[RuleDefinition]]): Rep[Parser]
  def infix_~(lhs: Rep[Rule], rhs: Rep[Rule]): Rep[Rule]
  def infix_|(lhs: Rep[Rule], rhs: Rep[Rule]): Rep[Rule]
}

trait BaseExp extends Base with Expressions {
  type Rep[T] = Exp[T]
}

trait ParboiledOpsExp extends BaseExp with ParboiledOps {
  type Def[T] = Exp[T] // while there is no toAtom...

  case class ParserDef(name: Rep[String], rules: Seq[Rep[RuleDefinition]]) extends Def[Parser]
  case class RuleDefinitionDef(name: Rep[String], argNames: Seq[Rep[String]], body: Rep[Rule]) extends Def[RuleDefinition]

  case class StringLiteral(str: Rep[String]) extends Def[Rule]
  case class Sequence(lhs: Rep[Rule], rhs: Rep[Rule]) extends Def[Rule]
  case class FirstOf(lhs: Rep[Rule], rhs: Rep[Rule]) extends Def[Rule]
  case class RuleCall(callingRuleName: Rep[String]) extends Def[Rule]

  def create_parser(name: Rep[String], rules: Seq[Rep[RuleDefinition]]): Rep[Parser] = ParserDef(name, rules)
  def infix_~(lhs: Rep[Rule], rhs: Rep[Rule]): Rep[Rule] = Sequence(lhs, rhs)
  def infix_|(lhs: Rep[Rule], rhs: Rep[Rule]): Rep[Rule] = FirstOf(lhs, rhs)
}

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
}

abstract class RunnerOpt extends Runner {
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

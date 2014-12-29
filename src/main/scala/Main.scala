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

  def create_parser(name: Rep[String], rules: Seq[Rep[RuleDefinition]]): Rep[Parser] = ??? // ParserDef(name, rules)
  def infix_~(lhs: Rep[Rule], rhs: Rep[Rule]): Rep[Rule] = ??? // Sequence(lhs, rhs)
  def infix_|(lhs: Rep[Rule], rhs: Rep[Rule]): Rep[Rule] = ??? // FirstOf(lhs, rhs)
}

abstract class Runner extends ParboiledOpsExp {
  val parser: ParserDef
  val input: String
  var cursor = 0

  def parse(): Boolean = {
    val startRule = parser.rules(0).asInstanceOf[RuleDefinitionDef]
    startRule.body match {
      case StringLiteral(Const(str)) => (str.length + cursor <= input.length) &&
        (input.substring(cursor, cursor + str.length) == str) && { cursor += str.length; true }
      case _ => ???
    }
  }
}

package net.tackley.regex

import util.matching.Regex
import java.util.regex.Pattern


abstract class Expression {
  def mkString: String
  def r = mkString.r
  protected def mkStringForAggregation
}

case class LiteralExpression(literal: String) extends Expression {
  def mkString = if (literal.forall(_.isLetterOrDigit)) literal else Pattern.quote(literal)
}

case class GroupExpression(e: Expression, isLazy: Boolean) extends Expression {
  def mkString = "("+prefix+e.mkString+lazyMarker+")"
  
  private def lazyMarker = if (isLazy) "?" else ""
  protected def prefix = ""
}

case class NonCaptureGroupExpression(e1: Expression, isLazy1: Boolean)
        extends GroupExpression(e1, isLazy1) {
  protected override def prefix = "?:"
}

case class RepeatsMinMaxExpression(e: Expression, min: Int, max: Int) extends Expression {
  def mkString = e.mkString+"{"+min+","+max+"}"
}

case class RepeatsNumExpression(e: Expression, num:Int) extends Expression {
  def mkString = e.mkString+"{"+num+"}"
}

case class OneOrMoreExpression(e: Expression) extends Expression {
  def mkString = e.mkString+"+"
}

case class ZeroOrMoreExpression(e: Expression) extends Expression {
  def mkString = e.mkString+"*"
}

case class OptionalExpression(e: Expression) extends Expression {
  def mkString = e.mkString+"?"
}

class RegexBuilder(val s: String) {
  def this() = this("")

  def literal(literal: String) = new RegexBuilder(s + literal)

  def r = s.r
}
package net.tackley.regex

import util.matching.Regex
import java.util.regex.Pattern


trait Atomizer {
  this: Expression =>

  def atomic = if (isAtomic) this else NonCaptureGroupExpression(this, false)
  def isAtomic: Boolean
}

abstract class Expression extends Atomizer {
  def mkString: String
  def mkAtomicString = atomic.mkString
  def r = mkString.r
}


case class LiteralExpression(literal: String) extends Expression {
  def mkString = if (literal.forall(_.isLetterOrDigit)) literal else Pattern.quote(literal)
  override def isAtomic = mkString.length == 1
}

case class GroupExpression(e: Expression, isLazy: Boolean) extends Expression {
  def mkString = "("+prefix+e.mkString+lazyMarker+")"
  override def isAtomic = true

  private def lazyMarker = if (isLazy) "?" else ""
  protected def prefix = ""
}

case class NonCaptureGroupExpression(e1: Expression, isLazy1: Boolean)
        extends GroupExpression(e1, isLazy1) {
  protected override def prefix = "?:"
}

case class RepeatsMinMaxExpression(e: Expression, min: Int, max: Int) extends Expression {
  def mkString = e.mkAtomicString+"{"+min+","+max+"}"
  override def isAtomic = true
}


case class RepeatsNumExpression(e: Expression, num:Int) extends Expression {
  def mkString = e.mkAtomicString+"{"+num+"}"
  override def isAtomic = true
}

case class OneOrMoreExpression(e: Expression) extends Expression {
  def mkString = e.mkAtomicString+"+"
  override def isAtomic = true
}

case class ZeroOrMoreExpression(e: Expression) extends Expression {
  def mkString = e.mkAtomicString+"*"
  override def isAtomic = true
}

case class OptionalExpression(e: Expression) extends Expression {
  def mkString = e.mkAtomicString+"?"
  override def isAtomic = true
}

case class CompositeExpression(a: Expression, b: Expression) extends Expression {
  def mkString = a.mkString + b.mkString
  override def isAtomic = false
}

class EmptyExpression extends Expression {
  def mkString = ""
  override def isAtomic = false
}



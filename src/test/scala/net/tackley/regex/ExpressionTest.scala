package net.tackley.regex

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import org.junit.runner.RunWith

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class ExpressionTest extends FlatSpec with ShouldMatchers {
  "Regular expression tree" should "support simple literals" in {
    LiteralExpression("a").mkString should be ("a")
    LiteralExpression("abc").mkString should be ("abc")
    LiteralExpression(".").mkString should be ("""\Q.\E""")
  }

  it should "support non lazy groups" in {
    GroupExpression(LiteralExpression("a"), false).mkString should be ("(a)")
  }
  
  it should "support lazy groups" in {
    GroupExpression(LiteralExpression("a"), true).mkString should be ("(a?)")
  }

  it should "support non capture non lazy groups" in {
    NonCaptureGroupExpression(LiteralExpression("a"), false).mkString should be ("(?:a)")
  }

  it should "support non capture lazy groups" in {
    NonCaptureGroupExpression(LiteralExpression("a"), true).mkString should be ("(?:a?)")
  }

  it should "support min max on an expression" in {
    RepeatsMinMaxExpression(LiteralExpression("a"), 2, 5).mkString should be ("a{2,5}")
  }

  it should "support num on an expression" in {
    RepeatsNumExpression(LiteralExpression("a"), 2).mkString should be ("a{2}")
  }

  it should "support one or more" in {
    OneOrMoreExpression(LiteralExpression("a")).mkString should be ("a+")
  }

  it should "support zero or more" in {
    ZeroOrMoreExpression(LiteralExpression("a")).mkString should be ("a*")
  }

  it should "support optional" in {
    OptionalExpression(LiteralExpression("a")).mkString should be ("a?")
  }
  
  it should "correcly group when necessary" in {
    OptionalExpression(LiteralExpression("ab")).mkString should be ("(?:ab)?")
    RepeatsMinMaxExpression(LiteralExpression("ab"), 2, 5).mkString should be ("(?:ab){2,5}")
  }

}


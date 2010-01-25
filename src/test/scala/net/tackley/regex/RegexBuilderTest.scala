package net.tackley.regex

import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec


@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class RegexBuilderTest extends FlatSpec with ShouldMatchers with RegexBuilder {

  "the regex dsl" should "let me do simple literal expressions" in {
    build.literal("ab").oneOrMore.mkString should be ("(?:ab)+")
    build.literal("a").literal("b").mkString should be ("ab")

    val p = (build literal "a" literal "b").oneOrMore;
    println(p.e)

    p
  }
}

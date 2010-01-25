package net.tackley.regex


trait RegexBuilder {
  def build = new RegexDSL(new EmptyExpression)

  class RegexDSL(val e: Expression) {
    def literal(s: String) = new RegexDSL(CompositeExpression(e, LiteralExpression(s)))
    def oneOrMore = new RegexDSL(OneOrMoreExpression(e))
    def zeroOrMore = new RegexDSL(ZeroOrMoreExpression(e))
    def repeat(min: Int, max: Int) = new RegexDSL(RepeatsMinMaxExpression(e, min, max))
    def repeat(count: Int) = new RegexDSL(RepeatsNumExpression(e, count))

    def mkString = e.mkString
    def r = e.r
  }
}
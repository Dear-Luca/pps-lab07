package ex1

import ex1.Parsers.charParser
import org.scalatest.matchers.should.Matchers.*

class ParserScalaTest extends org.scalatest.funsuite.AnyFunSuite:
    def parser = new BasicParser(Set('a', 'b', 'c'))

    // Note NonEmpty being "stacked" on to a concrete class
    // Bottom-up decorations: NonEmptyParser -> NonEmpty -> BasicParser -> Parser
    def parserNE = new NonEmptyParser(Set('0', '1'))

    def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))

    // note we do not need a class name here, we use the structural type
    def parserNTCNE = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char]

    def sparser: Parser[Char] = "abc".charParser()

    test("test basic parser"):
      parser.parseAll("aabc") shouldBe true
      parser.parseAll("aabcdc") shouldBe false
      parser.parseAll("") shouldBe true






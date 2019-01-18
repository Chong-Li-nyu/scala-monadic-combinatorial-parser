package parsing

import scala.language.postfixOps
import org.scalatest.FlatSpec

class ParserSpec extends FlatSpec {

  import parsing.Parser._
  import parsing.Location._
  "Parser of empty string" should "parse any string" in{
    assert(
      Parser.string("").parse("abc").isSuccess
    )
  }
  "Parsers original tests" should "all pass" in {
    assert('a'.parse('a'.toString).get === 'a')

    assert("ab".parse("ab").get === "ab")
    assert("ab".parse("a").isFailure)

    assert(('a' andThen 'c').parse("ac").get === ('a', 'c'))
    assert(("ac" andThen Parser.string("")).parse("ac").get === ("ac",""))
//    assert(('a' orElse 'b').parse('a').get === 'a')
    assert(('a' orElse 'b').parse('b'.toString).get === 'b')

    assert(repeat('a').parse("aaa").get === List('a', 'a', 'a'))

    assert(repeatN(3)('a').parse("aaa").get === List('a', 'a', 'a'))

    assert((repeat('a') map (_.size)).parse("aaa").get === 3)

//    assert((digit flatMap (repeatN(_)('a'))).parse("3aaa").get === List('a', 'a', 'a'))
//    assert(('3'.map(_.toInt) flatMap (repeatN(_)('a'))).parse("3aaa").get === List('a', 'a', 'a'))
    assert(repeatN(3)('a').parse("aaa").get === List('a', 'a', 'a'))

    assert((('a' andThen 'b') orElse ('a' andThen 'a')).parse("aa").isFailure === true)

    assert((attempt('a' andThen 'a') orElse ('a' andThen 'b')).parse("ab").get === ('a', 'b'))

    assert(((string("ab") andThen string("ba")) orElse (string("cd") andThen string("ab"))).parse("cdab").get === ("cd","ab"))
    //  assert(((string("ab") andThen string("ba")) orElse (string("cd") andThen string("ab"))).parse("cdab").get === "cdab")

  }


  "Two parsers" should "work parsing combined string" in {
    assert((string("ab") andThen char('c')).parse("abc").get === ("ab", 'c'))
  }
    "Parser" should "parse empty string successfully" in {
    assert( repeat("ab").parse("").isSuccess === true )
  }
    "Repeat parser" should "succeed combined with andThen" in {
    assert((repeat("a") andThen char('b')).parse(fromString("aaab")).isSuccess === true)
  }


  /* some ideas for unit tests
  
  assert(repeat('a').parse("aaa").get === List('a', 'a', 'a'))

  assert(repeatN(3)('a').parse("aaa").get === List('a', 'a', 'a'))
  
  assert((repeat('a') map (_.size)).parse("aaa").get === 3)

  assert((digit flatMap (repeatN(_)('a'))).parse("3aaa").get === List('a', 'a', 'a'))

  assert((attempt('a' andThen 'a') orElse ('a' andThen 'b')).parse("ab").get === ('a', 'b'))
  */
}

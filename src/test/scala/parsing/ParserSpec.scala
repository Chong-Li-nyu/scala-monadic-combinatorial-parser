package parsing

import scala.language.postfixOps
import org.scalatest.FlatSpec

class ParserSpec extends FlatSpec {

  import parsing.Parser._
  import parsing.Location._
  "Parser of empty string" should "fail parsing any non-empty string" in{
    assert(
      Parser.string("").parse("abc").isFailure
    )
  }
  "Parser[Char] should fail more than one character" should "fail to parse an input of more than one character" in{
    assert('a'.parse("ab").isFailure)
    assert('a'.parse(" a").isFailure)
  }
  "Parsers original tests" should "all pass" in {
    assert('a'.parse('a'.toString).get === 'a')
    assert("ab".parse("ab").get === "ab")

    assert('a'.parse("ab").isFailure)
    assert("ab".parse("a").isFailure)
    assert("ab".parse("abc").isFailure)

    assert(('a' orElse 'b').parse('b'.toString).get === 'b')
    assert(repeat('a').parse("aaa").get === List('a', 'a', 'a'))

    assert(repeatN(3)('a').parse("aaa").get === List('a', 'a', 'a'))
    assert((repeat('a') map (_.size)).parse("aaa").get === 3)

    assert((('a' andThen 'b') orElse ('a' andThen 'a')).parse("aa").isFailure === true)

    assert((attempt('a' andThen 'a') orElse ('a' andThen 'b')).parse("ab").get === ('a', 'b'))
  }


  "Two parsers" should "work parsing combined string" in {
    assert(('a' andThen 'c').parse("ac").get === ('a', 'c'))
    assert(("ac" andThen Parser.string("")).parse("ac").get === ("ac",""))
    assert(("a" andThen char('c')).parse("ac").get === ("a", 'c'))
  }
  "OrElse" should "succeed if left parser doesn't consume anything" in {
    assert(((string("ab") andThen string("ba")) orElse (string("cd") andThen string("ab"))).parse("cdab").get === ("cd","ab"))
  }
  "flatMap failure" should "fail when this parser fail first" in {
    assert('a'.flatMap(_ => "bb").parse("cbb").isFailure)
    val cp =
      for{
        a<- 'a'
        b<- Parser.string("bb")
      } yield a.toString.concat(b);
    assert(cp.parse("cbb").isFailure)
  }
  "Repeat parser" should "parse empty string successfully" in {
    assert( repeat("ab").parse("").isSuccess === true )
  }
  "Repeat parser" should "succeed combined with andThen" in {
    assert((repeat("a") andThen char('b')).parse(fromString("aaab")).isSuccess === true)
  }
  "Repeat parser failure" should "fail when come unmatched char in string" in {
    assert(repeat('b').parse("abbbb").isFailure)
  }

  "Regex parser digit repeat" should "parse 3aaa" in {
    assert((digit flatMap (repeatN(_)('a'))).parse("3aaa").get === List('a', 'a', 'a'))
  }

  "repeatN paring not enough N" should "fail when" in {
    assert(repeatN(2)('a').parse("aaa").isFailure)
  }
  "Digit parser" should "throw exception when parsing 0123a" in{
    assert("[0-9]*".r.parse("0123a").isFailure)
  }
  "nested repeat " should "parse a patter in abbbabb" in {
    val rpa = repeat('a');
    val rpb = repeat('b');
    assert((repeat('a') andThen repeat('b')).parse("aabbb").isSuccess)
    assert(repeat('a' andThen repeat('b')).parse("abbbabb").get === List( ('a', List('b','b','b')), ('a',List('b','b')) ) )
    assert(repeat(repeat('b') andThen 'a').parse("bbbaa").isSuccess) // [([b,b,b],a), ([],a)]
    println(repeat(repeat('b') andThen 'a').parse("bbbaa").get)
    assert( repeat(rpb andThen rpa).parse("bbbaabba").isSuccess )

    assert(repeat(repeat('b') andThen repeat('a')).parse("bbbaabbabbb").isSuccess)
    println(repeat(rpb andThen rpa).parse("bbbaabbabbb" ).get); //List((List(b, b, b),List(a, a)), (List(b, b),List(a)), (List(b, b, b),List()))
  }
  /* some ideas for unit tests
  
  assert(repeat('a').parse("aaa").get === List('a', 'a', 'a'))

  assert(repeatN(3)('a').parse("aaa").get === List('a', 'a', 'a'))
  
  assert((repeat('a') map (_.size)).parse("aaa").get === 3)

  assert((digit flatMap (repeatN(_)('a'))).parse("3aaa").get === List('a', 'a', 'a'))

  assert((attempt('a' andThen 'a') orElse ('a' andThen 'b')).parse("ab").get === ('a', 'b'))

  assert((digit flatMap (repeatN(_)('a'))).parse("3aaa").get === List('a', 'a', 'a'))

  assert(('3'.map(_.toInt) flatMap (repeatN(_)('a'))).parse("3aaa").get === List('a', 'a', 'a'))

  */
}

package parsing

import scala.util.{Failure, Success, Try}
import scala.language.implicitConversions
import scala.util.matching.Regex

trait Location {
  val pos: Int;
  val s: String;
  val len: Int;
  def nextChar(): Char
  def hasNext(): Boolean
}
class StringLocation(val s: String, val pos: Int) extends Location{
  val len = s.length();
  override def nextChar() : Char = s.charAt(pos) //if we need to make pos increment or not? Not => create a new location

  override def hasNext(): Boolean = (pos < len)
}

object Location {
  implicit def fromString(s: String): Location = new StringLocation(s, 0)
  implicit def fromChar(c: Char): Location = fromString(c.toString)
}

private[parsing] sealed trait ParseState[+A]

/*
ParseState difference is that it not only contains final parsing result, but also have how long result matched,
for updating the loc.pos
 */
case class SuccessState[+A](val result: A, val matched: Int) extends ParseState[A] // You can treat SuccessState as subclass of ParseState
case class FailureState[+A](val exception: Exception, val consumed: Int, val isCommitted: Boolean = true) extends ParseState[A] //uncommited will be a partial match amount
// See flatmap for more detail how to determine consumed
/*
trait Function1[-T, +U] {
  def apply(x: T): U
}
*/

trait Parser[A] {
  protected def apply(loc: Location): ParseState[A]

  def parse(loc: Location): Try[A] = {
    this.apply(loc) match {
      case SuccessState(result, _) => Success(result)
      case FailureState(exception, _, _)  => Failure(exception)
    }
  }
  
  def orElse(p: Parser[A]): Parser[A] = {
    loc: Location => {
      this(loc) match {
        case SuccessState(result, matched) => SuccessState(result, matched)
        case FailureState(exception, consumed, true) => p(new StringLocation(loc.s, loc.pos + consumed)) // p parses in middle of location
        case FailureState(exception, consumed, false) => p(loc) //not isCommitted means fail and re-parse from start
      }
    }
  }

  def andThen[B](p: Parser[B]): Parser[(A, B)] =
    for {
      a <- this
      b <- p
    } yield (a,b)
  
  def map[B](f: A => B): Parser[B] = {
    loc: Location => {
      this(loc) match{
        case SuccessState(result, matched: Int) => SuccessState[B](f(result), matched)
        case FailureState(ex:Exception, consumed: Int, isCommitted) => FailureState(ex, consumed, isCommitted)
      }
    }
  }
  
  def flatMap[B](f: A => Parser[B]): Parser[B] = {
    loc: Location => {
      this(loc) match {
        case SuccessState(result, matchedA: Int) => {
          f(result)(new StringLocation(loc.s, loc.pos + matchedA)) match {
            case SuccessState(resultB, matchedB) => SuccessState(resultB, matchedA + matchedB)
            case FailureState(exception, consumed, isCommitted) => FailureState(exception, matchedA + consumed, isCommitted)
          }
        }
        case FailureState(ex, consumed, isCommitted) => FailureState(ex, consumed, isCommitted)
      }
    }
  }
}

object Parser {
  def repeat[A](p: Parser[A]): Parser[List[A]] = ???

  def repeatN[A](n: Int)(p: Parser[A]): Parser[List[A]] = ???

  /*
  The attempt need to return a FailureState of containing consumed part, for restoring the failure location.pos to
  where starts parsing
   */
  def attempt[A](p: Parser[A]): Parser[A] = {
    loc: Location => {
      p(loc) match {
        case SuccessState(result, matched) => SuccessState(result, matched)
        case FailureState(exception, consumed, _) => FailureState(exception, consumed, false)
      }
    }
  }

  def label[A](msg: String)(p: Parser[A]): Parser[A] = ???

  def tag[A](msg: String)(p: Parser[A]): Parser[A] = ???

  implicit def char(c: Char): Parser[Char] = {
    loc: Location => {
      if (c == loc.nextChar){
        SuccessState[Char](c, 1)
      } else{
        FailureState[Char](new Exception(s"Character ${c} fail parsing at position:${loc.pos} of ${loc.s}"), 0) 
        //because parser unmatches any char should consume nothing, consumed = 0
        // ((string("ab") andThen string("ba")) orElse (string("cd") andThen string("ab"))).parse("cdab") succeed
        //first parser does not consume any part of the input
      }
    }
  }
  
  implicit def string(s: String): Parser[String] = {
    loc: Location => {
      if (s.isEmpty()) {
          SuccessState("", 0)
      }
      if (!loc.hasNext && !s.isEmpty) {
        FailureState(new Exception("Input sequence is not long enough."), 0)
      }
      val x = s.charAt(0);
      val xs = s.substring(1);
      val xsParser: Parser[String] = if (xs.isEmpty()) (_ => SuccessState("", 0)) else string(xs);
      val combinedParser: Parser[(Char, String)] = x andThen xsParser;
      combinedParser.map(pair => pair._1.toString().concat(pair._2))(loc)
    }
  }
  
  implicit def regex(r: Regex): Parser[String] = ???

  def digit: Parser[Int] = "[0-9]".r map (_.toInt)
  // def digit: Parser[Int] = ('0' to '9') reduce (_ orElse _) map (_.toString.toInt)
  
  def digits: Parser[Int] = 
    (digit andThen repeat(digit)) map { case (d, ds) => (d :: ds) reduce (_ * 10 + _) }
}



package parsing

import scala.util.{Failure, Success, Try}
import scala.language.implicitConversions
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

trait Location {
  val pos: Int
  val s: String
  val len: Int
  def nextChar(): Char
  def hasNext(): Boolean
  def advanceBy(n :Int): Location
}
class StringLocation(val s: String, val pos: Int) extends Location{
  val len = s.length();
  override def nextChar(): Char = s.charAt(pos) //if we need to make pos increment or not? Not => create a new location
  override def hasNext(): Boolean = (pos < len)
  override def advanceBy(n :Int): Location = {
    new StringLocation(this.s, this.pos + n)
  }
}

object Location {
  implicit def fromString(s: String): Location = new StringLocation(s, 0)
  implicit def fromChar(c: Char): Location = fromString(c.toString)
}

private[parsing] sealed trait ParseState[+A]

/*
ParseState difference is that it not only contains final parsing result, but also have how long result consumed,
for updating the loc.pos
 */
case class SuccessState[+A](result: A, consumed: Int) extends ParseState[A] // You can treat SuccessState as subclass of ParseState
case class FailureState[+A](tempResult: Try[A], consumed: Int, isCommitted: Boolean = true) extends ParseState[A] //uncommited will be a partial match amount
// See flatmap for more detail how to determine consumed
/*
Considering a fucntion which is subtype of another function, its return value should be more childish type,
then it can be used as subtype the Function returning Parent class, because its return value can be upcast to parent
function type

trait Function1[-T, +U] {
  def apply(x: T): U
}
*/

trait Parser[A] {
  protected def apply(loc: Location): ParseState[A]

  def parse(loc: Location): Try[A] = {
    this.apply(loc) match {
      case SuccessState(result, _) => Success(result) //Strictly success such as String("ab"). parse("ab"), not string("ab").parse("abc")
      case FailureState(tempResult, _, _)  => Failure(new Exception(s"Parsing ${loc.s}@${loc.pos} failed"))
    }
  }

  /**
    * If the committed subparser fails, the orElse parser should itself fail without trying its right subparser.
    * @param p
    * @return
    */
  def orElse(p: Parser[A]): Parser[A] = {
    loc: Location => {
      this(loc) match {
        case SuccessState(result, consumed) => SuccessState(result, consumed)
        case FailureState(tempA, consumedA, false) => p(loc)  //attemp make it uncommitted, parse loc again.
        case FailureState(tempA, consumed, true) =>{
          if(consumed == 0) p(loc) else FailureState(tempA, consumed/*, true*/)
        }
      }
    }
  }

  def andThen[B](p: =>Parser[B]): Parser[(A, B)] = //the p type is a call-by-name parameter, Otherwise, you will likely end up in an infinite recursion when the parser for `repeat` is constructed
    for {
      a <- this
      b <- p
    } yield (a,b)
  //this flatMap { a => pb map { b => (a, b) } }


  def map[B](f: A => B): Parser[B] = {
    loc: Location => {
      this(loc) match{
        case SuccessState(result, consumed: Int) => SuccessState[B](f(result), consumed)
        case FailureState(tempA, consumed, isCommitted) => {
          val tempB = for{
            x <- tempA
          } yield f(x) //good monadic semantic to process both Success and Failure
          FailureState(tempB, consumed, isCommitted)
        }
      }
    }
  }
  
  def flatMap[B](f: A => Parser[B]): Parser[B] = {
    loc: Location => {
      val (resultA, consumedA) =  this(loc) match {
        case SuccessState(result, consumed) => (Success(result), consumed)
        case FailureState(tempResult, consumed, _:Boolean) => (tempResult, consumed)
          //because FailureState may represents this(loc) get fully matched parsing middle result, but not fully consume loc.
      }
      resultA match {
        case Failure(ex) => FailureState(Failure(ex), consumedA)
        case Success(result)=> {
          f(result)(loc.advanceBy(consumedA)) match {
            case SuccessState(resultB, consumedB) => SuccessState(resultB, consumedA + consumedB)
            case FailureState(tempResult, consumedB, isCommitted) => FailureState(tempResult, consumedA + consumedB, isCommitted)
          }
        }
      }
    }
  }
}


object Parser {

  /* Idea:
   */
  def repeat[A](p: =>Parser[A]): Parser[List[A]] = { //call-by-name lazy evaluation
    loc:Location => {
      if(!loc.hasNext) SuccessState[List[A]](Nil, 0) //explicitly specify the end condition, otherwise infinite loop
      else {
        val successParser: Parser[List[A]] = (_: Location) => SuccessState[List[A]](Nil, 0);
        val repeatParser: Parser[List[A]] = for {
          a <- attempt(p) //when this attemp(p) fails, it will not consume any loc. repeatParser will jump out and go to orElse successParser
          b <- repeat(p)
        } yield (a :: b)
        //      lazy val repeatParser = (attempt(p) andThen repeat(p)).map( (p:(A, List[A]))=> p._1::p._2 )
        (repeatParser orElse successParser) (loc) match { //(repeatParser orElse successParser) (loc) always returns SuccessState
          case SuccessState(result, consumed) => {
            if (!loc.advanceBy(consumed).hasNext()) SuccessState(result, consumed)
            else FailureState(Success(result), consumed)
          }
          case fs: FailureState[List[A]] => fs
        }
      }
    }
  }

  def repeatN[A](n: Int)(p: Parser[A]): Parser[List[A]] = {
    loc => {
      if (n == 0){
        if (!loc.hasNext()) SuccessState(Nil, 0) else FailureState(Success(Nil), 0)
      }
      else {
        val cp = for {
          a <- p
          b <- repeatN(n - 1)(p)
        } yield (a :: b)
        cp(loc) //lack failure case
      }
    }
  }

  /*
  The attempt need to return a FailureState of containing consumed part, for restoring the failure location.pos to
  where starts parsing
   */
  def attempt[A](p: Parser[A]): Parser[A] = {
    loc: Location => {
      p(loc) match {
        case SuccessState(result, consumed) => SuccessState(result, consumed)
        case FailureState(tempResult, consumed, _) => FailureState(tempResult, consumed, false)
      }
    }
  }

  def label[A](msg: String)(p: Parser[A]): Parser[A] = {
    loc: Location => {
      p(loc) match {
        case FailureState(_: Failure[A], consumed, _) =>
          FailureState(Failure(new Exception(msg)), consumed, false)
        case resultState: ParseState[A] => resultState
      }
    }
  }

  def tag[A](msg: String)(p: Parser[A]): Parser[A] = ???

  implicit def char(c: Char): Parser[Char] = {
    loc: Location => {
      if (!loc.hasNext()) FailureState(Failure(new Exception(s"Empty input for Parser[char] of ${c}")), 0)
      else {
        if (c == loc.nextChar()) {
          if (!loc.advanceBy(1).hasNext()) {
            SuccessState(c, 1)
          } else {
            FailureState(Success(c), 1) //failure but consumed 1
          }
        } else
          FailureState[Char](Failure(new Exception(s"Character ${c} fail parsing at position:${loc.pos} of ${loc.s}")), 0)
        //because parser unmatches any char should consume nothing, consumed = 0
        // ((string("ab") andThen string("ba")) orElse (string("cd") andThen string("ab"))).parse("cdab") succeed
        //first parser does not consume any part of the input
      }
    }
  }
  
  implicit def string(s: String): Parser[String] = {
    loc: Location => {
      s match {
        case "" => if (!loc.hasNext) SuccessState("", 0) else FailureState(Success(""), 0)
        case _ =>
          val combinedParser = for {
            head <- char(s.charAt(0)) //Why here don't need to check location.hasNext? Because the parser[Char] already does that.
            cons <- string(s.substring(1))
          } yield head.toString.concat(cons)
          combinedParser(loc)
//          val combinedParser = s.charAt(0) andThen Parser.string(s.substring(1))
//          combinedParser.map(pair => pair._1.toString.concat(pair._2))(loc)
      }
    }
  }

  
  implicit def regex(r: Regex): Parser[String] = {
    loc:Location => {
      r.findPrefixMatchOf(loc.s.substring(loc.pos)) match {
        case Some(matchResult: Match) => { //def end(i: Int): Int The index following the last matched character in group i.
          if (!loc.advanceBy(matchResult.end).hasNext) SuccessState(matchResult.matched, matchResult.end)
          else FailureState(Success(matchResult.matched.toString), matchResult.end)
        }
        case None => {
          FailureState(Failure(new Exception("Regex matching failed")), 0)
        }
      }
    }
  }

  def digit: Parser[Int] = "[0-9]".r map (_.toInt)
  // def digit: Parser[Int] = ('0' to '9') reduce (_ orElse _) map (_.toString.toInt)
  
  def digits: Parser[Int] = 
    (digit andThen repeat(digit)) map { case (d, ds) => (d :: ds) reduce (_ * 10 + _) }
}



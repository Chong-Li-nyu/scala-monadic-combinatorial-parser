package json

import parsing.Parser

object JSON {

  sealed abstract class JSON

  case object JNull extends JSON
  case class JBool(b: Boolean) extends JSON
  case class JNumber(n: Double) extends JSON
  case class JString(s: String) extends JSON
  case class JArray(a: IndexedSeq[JSON]) extends JSON
  case class JObject(a: Map[String, JSON]) extends JSON

  def jsonParser: Parser[JSON] = ???
  
  def parse(s: String): JSON = jsonParser.parse(s).get

}
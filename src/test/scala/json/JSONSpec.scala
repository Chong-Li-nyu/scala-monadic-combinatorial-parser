package json


import scala.language.postfixOps
import org.scalatest.FlatSpec
import JSON._

class JSONSpec extends FlatSpec {

  import parsing.Parser._

  val testInput =
    """{
      |  "number": 3.0,
      |  "bool": true,
      |  "string": "Hello",
      |  "array: [{ "x": 3 }, 42, null],
      |  "object: {}
      |}
    """.stripMargin
  
  val expected = JObject (
    Map (
      "number" -> JNumber(3.0),
      "bool" -> JBool(true),
      "string" -> JString("Hello"),
      "array" -> JArray(IndexedSeq(JObject(Map("x" -> JNumber(3))), JNumber(42), JNull)),
      "object" -> JObject(Map())
    )
  )
  
  "jsonParser" should "parse JSON data" in {
     assert (JSON.parse(testInput) === expected)
  }
  
  /** add your own tests */
}
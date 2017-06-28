
import org.peterc.srdp.examples.arithmetic.Arithmetic._
import org.peterc.srdp.examples.json.{Json, JsonRules}
import org.peterc.srdp.examples.json.Json._

eval"50 - 1"
eval"((100 + 300) / (4 * 25)) - 1"

//val parsedJson: Option[Int] =
Json.parse("""{"test": {"test2": [5]}}""").map(Json.toPrimitives)


val x = Json[Map[String, BigDecimal]](""" {"test": 5} """)

x.get.get("test")



//parsedJson.get

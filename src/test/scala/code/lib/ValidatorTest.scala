package code.lib

import org.scalatest._

import scala.xml.Text

/**
 * Created by anton on 1/30/15.
 */
class ValidatorTest extends FlatSpec with Matchers {
  "Validator" should "match web-3HywBe@mail-tester.com as valid email" in {
    Validator.email[Unit]("valid", Text("valid"))(_ => "web-3HywBe@mail-tester.com").run(Unit) should be (Nil)
  }
}

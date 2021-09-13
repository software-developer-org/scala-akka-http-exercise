package com.fashion

import akka.actor.ActorSystem
import akka.actor.testkit.typed.scaladsl.ActorTestKit
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model._
import akka.http.scaladsl.testkit.RouteTestTimeout
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.duration._

class EvaluationRoutesSpec extends AnyWordSpec with Matchers with ScalaFutures with ScalatestRouteTest {
  // the Akka HTTP route testkit does not yet support a typed actor system (https://github.com/akka/akka-http/issues/2036)
  // so we have to adapt for now
  lazy val testKit = ActorTestKit()
  implicit def typedSystem = testKit.system
  override def createActorSystem(): akka.actor.ActorSystem =
    testKit.system.classicSystem

  // Here we need to implement all the abstract members of EvaluationRoutes.
  // We use the real EvaluationRegistryActor to test it while we hit the Routes,
  // but we could "mock" it by implementing it in-place or by using a TestProbe
  // created with testKit.createTestProbe()
  val evaluationRegistry = testKit.spawn(EvaluationRegistry())
  lazy val routes = new EvaluationRoutes(evaluationRegistry).evaluationRoutes

  // use the json formats to marshal and unmarshall objects in the test
  import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
  import JsonFormats._

  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(5.seconds)

  "EvaluationRoutes" should {
    "evaluate speeches" in {
      // note that there's no need for the host part in the uri:
      val request = HttpRequest(uri = "/evaluation?url=https://raw.githubusercontent.com/software-developer-org/speeches-example-data/main/speech1.csv&url=https://raw.githubusercontent.com/software-developer-org/speeches-example-data/main/speech1.csv")

      request ~> routes ~> check {
        status should ===(StatusCodes.OK)

        // we expect the response to be json:
        contentType should ===(ContentTypes.`application/json`)

        // and no entries should be in the list:
        entityAs[String] should ===("""{"leastWordy":"Caesare Collins","mostSecurity":"Alexander Abel","mostSpeeches":"Alexander Abel"}""")
      }
    }
  }
}

package com.fashion

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.AskPattern._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.util.Timeout
import com.fashion.EvaluationRegistry._

import scala.concurrent.Future

class EvaluationRoutes(evaluationRegistry: ActorRef[EvaluationRegistry.Command])(implicit val system: ActorSystem[_]) {

  import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
  import JsonFormats._

  // If ask takes more time than this to complete the request is failed
  private implicit val timeout = Timeout.create(system.settings.config.getDuration("my-app.routes.ask-timeout"))

  def evaluate(): Future[Speeches] =
    evaluationRegistry.ask(Evaluate)

  val evaluationRoutes: Route =
    pathPrefix("evaluation") {
      concat(
        pathEnd {
          concat(
            get {
              complete(evaluate())
            })
        })
    }
}

package com.fashion

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.util.Timeout

import scala.collection.immutable
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Failure
import scala.util.Success

final case class Speech(speaker: String, topic: String, wordsCount: Int)
final case class Speeches(speeches: immutable.Seq[Speech])

object EvaluationRegistry {
  // actor protocol
  sealed trait Command
  final case class Evaluate(replyTo: ActorRef[Speeches]) extends Command
  final case class ActionPerformed(description: String)

  def apply(): Behavior[Command] = registry()

  private def registry(): Behavior[Command] =
    Behaviors.receiveMessage {
      case Evaluate(replyTo) =>
        getData(replyTo)
        Behaviors.same
    }

    private def getData(replyTo: ActorRef[Speeches]) {
        // HTTP client request
        implicit val system = ActorSystem(Behaviors.empty, "SingleRequest")
        val request = HttpRequest(uri = "https://raw.githubusercontent.com/software-developer-org/speeches-example-data/main/speech1.csv")
        val responseFuture: Future[HttpResponse] = Http().singleRequest(request)
        // needed for future's flatMap and onComplete in the end
        implicit val executionContext = system.executionContext
        responseFuture
          .onComplete {
            case Success(res) => println(res)
            case Failure(res) => res.printStackTrace//sys.error("something wrong")
          }

        // subscribe and handle response
        import scala.language.postfixOps
        responseFuture.flatMap(_.entity.toStrict(2 seconds)).map(_.data.utf8String).foreach { item =>
          println("======================================================================")
          item.split('\n').zipWithIndex.foreach{ case (line, idx) => println(idx + ": " + line)}
          println("======================================================================")
          replyTo ! Speeches(Set.empty.toSeq)
        }
    }
}

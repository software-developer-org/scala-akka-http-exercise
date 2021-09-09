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
  final case class Evaluate(urls: Seq[String], replyTo: ActorRef[Speeches]) extends Command
  final case class ActionPerformed(description: String)

  def apply(): Behavior[Command] = registry()

  private def registry(): Behavior[Command] =
    Behaviors.receiveMessage {
      case Evaluate(urls, replyTo) =>
        getData(urls, replyTo)
        Behaviors.same
    }

    private def getData(urls: Seq[String], replyTo: ActorRef[Speeches]) {
      implicit val system = ActorSystem(Behaviors.empty, "SingleRequest")
      implicit val executionContext = system.executionContext
      val csvFutures = urls.map(httpClientRequest)
      val csvDataList = Future.sequence((csvFutures)).map(_.flatten);
      csvDataList.foreach( csvData => {
          println("======================================================================")
          csvData.foreach((row) => println("<" + row.reduce((a, b) => a + "|" + b) + ">"))
          println("======================================================================")
          replyTo ! Speeches(Set.empty.toSeq)
        })
    }

    private def httpClientRequest(url: String): Future[Array[Array[String]]] = {
      // HTTP client request
      implicit val system = ActorSystem(Behaviors.empty, "SingleRequest")
      val request = HttpRequest(uri = url)
      val responseFuture: Future[HttpResponse] = Http().singleRequest(request)
      // needed for future's flatMap and onComplete in the end
      implicit val executionContext = system.executionContext
      responseFuture
        .onComplete {
          case Success(res) => // noop
          case Failure(res) => res.printStackTrace
        }

      // subscribe and handle response
      import scala.language.postfixOps
      val data = responseFuture
        .flatMap(_.entity.toStrict(2 seconds)).map(_.data.utf8String)
        .map(asCsvArray)
      return data
    }

    private def asCsvArray(responseData: String): Array[Array[String]] = {
      responseData
        // split per row
        .split('\n')
        .map(row => {
          row
          // split each row into cols
          .split(',')
          // remove trailing whitespaces
          .map(column => column.trim())
        })
    }
}

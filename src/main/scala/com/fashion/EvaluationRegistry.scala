package com.fashion

import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

import scala.collection.immutable

final case class Speech(name: String, age: Int, countryOfResidence: String)
final case class Speeches(speeches: immutable.Seq[Speech])

object EvaluationRegistry {
  // actor protocol
  sealed trait Command
  final case class Evaluate(replyTo: ActorRef[Speeches]) extends Command
  final case class ActionPerformed(description: String)

  def apply(): Behavior[Command] = registry(Set.empty)

  private def registry(speeches: Set[Speech]): Behavior[Command] =
    Behaviors.receiveMessage {
      case Evaluate(replyTo) =>
        replyTo ! Speeches(speeches.toSeq)
        Behaviors.same
    }
}

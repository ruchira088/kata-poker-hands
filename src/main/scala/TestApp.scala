import cards.{Card, CardSuite, CardValue}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object TestApp
{
  def main(args: Array[String]): Unit =
  {
    for {
      pokerGame <- InputParser.parseLine("Black: 2H 4S 4C 4D 4H  White: 2S 8S AS QS 3S")
      _ = println(WinningCards(pokerGame.player_1.pokerHand))
    } yield true

  }
}

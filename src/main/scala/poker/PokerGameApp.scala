package poker

import java.nio.file.{Path, Paths}

import poker.game.PokerGame
import poker.utils.{FileUtils, InputParser, ScalaUtils}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object PokerGameApp
{
  val INPUT_FILE_PATH: Path = Paths.get("resources/input.txt")

  def main(args: Array[String]): Unit =
  {
    val pokerGameResultFutures: Future[List[String]] = for
      {
        gameStrings <- FileUtils.readFile(INPUT_FILE_PATH)

        pokerGames <- Future.fromTry(ScalaUtils.sequence(gameStrings.map(InputParser.parseLine)))

        results = pokerGames.map(PokerGame.getWinner)

      } yield results


    val results = Await.result(pokerGameResultFutures, 30 seconds)

    results.foreach(println)
  }
}

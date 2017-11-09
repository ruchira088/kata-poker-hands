package poker

import java.nio.file.{Path, Paths}

import poker.game.PokerGame.getWinner
import poker.utils.{FileUtils, InputParser}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

object PokerGameApp
{
  val INPUT_FILE_PATH: Path = Paths.get("resources/input.txt")

  def main(args: Array[String]): Unit =
  {
    val pokerGameResultFutures: Future[List[String]] = for
      {
        lines <- FileUtils.readFile(INPUT_FILE_PATH)

        results = lines.map(InputParser.parseLine)
          .map {
            case Success(pokerGame) => getWinner(pokerGame)
            case Failure(exception) => exception.getMessage
          }
      }
      yield results

    val output = Await.result(pokerGameResultFutures, 30 seconds)

    output.foreach(println)
  }
}
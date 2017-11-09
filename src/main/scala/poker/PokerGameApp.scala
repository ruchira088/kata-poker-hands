package poker

import java.nio.file.{Path, Paths}

import poker.game.PokerGame.getWinner
import poker.utils.{FileUtils, InputParser, ScalaUtils}
import poker.utils.ScalaUtils.toFuture

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.control.NonFatal

object PokerGameApp
{
  val INPUT_FILE_PATH: Path = Paths.get("resources/input.txt")

  def main(args: Array[String]): Unit =
  {
    val pokerGameResultFutures: Future[List[String]] = for
      {
        lines <- FileUtils.readFile(INPUT_FILE_PATH)

        results = lines.map {
          line => InputParser.parseLine(line)
            .map(getWinner)
            .recover { case NonFatal(exception) => exception.getMessage }
        }

        output <- ScalaUtils.sequence(results)
      }
      yield output

    val gameResults = Await.result(pokerGameResultFutures, 30 seconds)

    gameResults.foreach(println)
  }
}
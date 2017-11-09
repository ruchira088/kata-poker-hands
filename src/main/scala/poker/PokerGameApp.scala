package poker

import java.nio.file.{Path, Paths}

import poker.utils.ScalaUtils.toFuture
import poker.utils.{FileUtils, InputParser}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}
import scala.Console._

object PokerGameApp
{
  val DEFAULT_INPUT_FILE_PATH: Path = Paths.get("resources/input.txt")

  def main(args: Array[String]): Unit =
  {
    val inputFilePath = args.toList.headOption
      .map(Paths.get(_))
      .getOrElse(DEFAULT_INPUT_FILE_PATH)

    println(s"InputFile: ${inputFilePath.toAbsolutePath}")

    val pokerGameResultFutures: Future[List[String]] = for
      {
        lines <- FileUtils.readFile(inputFilePath)
        results <- InputParser.parseAndEvaluate(lines)
      }
      yield results

    pokerGameResultFutures.onComplete
    {
      case Success(results) =>
        results.foreach(println)

      case Failure(throwable) =>
        println(s"$RESET$RED$BOLD$throwable$RESET")
    }

    Await.ready(pokerGameResultFutures, 30 seconds)
  }
}
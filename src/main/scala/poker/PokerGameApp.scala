package poker

import java.nio.file.{Path, Paths}

import poker.utils.ScalaUtils.toFuture
import poker.utils.{FileUtils, InputParser}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object PokerGameApp
{
  val DEFAULT_INPUT_FILE_PATH: Path = Paths.get("resources/input.txt")

  def main(args: Array[String]): Unit =
  {
    val inputFilePath = args.toList.headOption
      .map(Paths.get(_))
      .filter(_.toFile.exists())
      .getOrElse(DEFAULT_INPUT_FILE_PATH)

    println(s"InputFile: ${inputFilePath.toAbsolutePath}")

    val pokerGameResultFutures: Future[List[String]] = for
      {
        lines <- FileUtils.readFile(inputFilePath)
        results <- InputParser.parseAndEvaluate(lines)
      }
      yield results

    val results = Await.result(pokerGameResultFutures, 30 seconds)

    results.foreach(println)
  }
}
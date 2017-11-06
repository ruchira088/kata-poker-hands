import java.nio.file.{Path, Paths}

import cards.Card
import exceptions.InputParseException
import utils.FileUtils

import scala.concurrent.Future
import scala.util.{Failure, Try}

object InputParser
{
  val INPUT_FILE_PATH: Path = Paths.get("resources/input.txt")
  val PLAYER_DELIMITER = "  "

  def parseLine(line: String) =
  {
    line.split(PLAYER_DELIMITER).toList
  }

  def parsePlayer(playerString: String): Try[Player] = playerString.split(":").toList match
    {
    case name :: cards :: Nil => ???
    case _ => Failure(InputParseException(playerString))
    }

  def parseCards(cardsString: String): Try[PokerHand] = ???

  def parseCard(cardString: String): Try[Card] = ???

//  def readInputFile(): Future[List[String]] = for
//    {
//      fileContents <- FileUtils.readFile(INPUT_FILE_PATH)
//    }
}

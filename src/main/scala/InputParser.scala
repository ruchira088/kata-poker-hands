import java.nio.file.{Path, Paths}

import cards.Card
import exceptions.InputParseException
import utils.ScalaUtils

import scala.util.{Failure, Success, Try}

object InputParser
{
  val INPUT_FILE_PATH: Path = Paths.get("resources/input.txt")
  val PLAYER_DELIMITER = "  "
  val CARD_DELIMITER = " "

  def parseLine(line: String): Try[PokerGame] =
    line.split(PLAYER_DELIMITER).toList.map(parsePlayer) match
      {
        case players if players.length == 2 =>
          ScalaUtils.sequence(players)
            .fold(
              Failure(_),
              { case player_1 :: player_2 :: _ => Success(PokerGame(player_1, player_2)) }
            )

        case _ => Failure(InputParseException(line))
      }

  def parsePlayer(playerString: String): Try[Player] = playerString.split(":").toList match
    {
      case name :: cards :: Nil => parseCards(cards.trim).map(Player(name, _))
      case _ => Failure(InputParseException(playerString))
    }

  def parseCards(cardsString: String): Try[PokerHand] = for
    {
      cards <- ScalaUtils.sequence(cardsString.split(CARD_DELIMITER).toList.map(Card.parse))

      pokerHand <- PokerHand.create(cards)
    }
    yield pokerHand
}

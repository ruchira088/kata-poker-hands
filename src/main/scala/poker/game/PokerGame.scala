package poker.game

import poker.hand.{HighestRankingCards, PokerHandSummary}

case class PokerGame(player_1: Player, player_2: Player)

object PokerGame
{
  def getWinner(pokerGame: PokerGame): String =
  {
    val playerOneSummary: PokerHandSummary = HighestRankingCards(pokerGame.player_1.pokerHand)
    val playerTwoSummary: PokerHandSummary = HighestRankingCards(pokerGame.player_2.pokerHand)

    def winnerDescription(playerName: String, pokerHandSummary: PokerHandSummary): String =
      s"$playerName WINS - ${pokerHandSummary.ranking.description}"

    playerOneSummary.compare(playerTwoSummary) match
    {
      case value if value < 0 => winnerDescription(pokerGame.player_2.name, playerTwoSummary)
      case value if value > 0 => winnerDescription(pokerGame.player_1.name, playerOneSummary)
      case 0 => "Tie."
    }
  }
}
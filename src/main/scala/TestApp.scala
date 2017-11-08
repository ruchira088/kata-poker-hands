

object TestApp
{
  def main(args: Array[String]): Unit =
  {
    for {
      pokerGame <- InputParser.parseLine("Black: 2H 3S 4C 5D 6H  White: 2S 8S AS QS 3S")
      _ = println(WinningCards(pokerGame.player_1.pokerHand))
    } yield true

  }
}

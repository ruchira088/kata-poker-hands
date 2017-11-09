package poker.exceptions

object EmptyOptionException extends Exception with ConsoleError
{
  override def getMessage = styleErrorMessage("Empty option")
}

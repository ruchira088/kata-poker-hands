package poker.exceptions

object EmptyStringException extends Exception with ConsoleError
{
  override def getMessage = styleErrorMessage("Empty string")
}
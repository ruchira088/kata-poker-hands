package poker.exceptions

case class CardSuiteMappingException(string: String) extends Exception with ConsoleError
{
  override def getMessage = styleErrorMessage(s"""Unable to parse suite: \"$string\"""")
}
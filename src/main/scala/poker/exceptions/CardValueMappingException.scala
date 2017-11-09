package poker.exceptions

case class CardValueMappingException(string: String) extends Exception with ConsoleError
{
  override def getMessage = styleErrorMessage(s"""Unable to parse card value: \"$string\"""")
}

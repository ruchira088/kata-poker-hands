package poker.exceptions

import Console.{RESET, RED, BOLD}

trait ConsoleError {
  error: Exception =>

  def styleErrorMessage(errorMessage: String) = s"$RESET$RED${BOLD}Exception = $errorMessage$RESET"
}

import scala.io.StdIn.{readInt, readLine}

object InteractionHandler {
  def handleStringInput(message: String): String = {
    println(message)
    readLine()
  }

  def handleIntInput(message: String): Int = {
    println(message)
    readInt()
  }

  def handleIntInputWithRetry(initialMessage: String, retryMessage: String = "invalid input try again", condition: Int => Boolean): Int = {
    handleInputWithRetry[Int](initialMessage, retryMessage, condition, handleIntInput)
  }

  def handleStringInputWithRetry(initialMessage: String, retryMessage: String, condition: String => Boolean): String = {
    handleInputWithRetry[String](initialMessage, retryMessage, condition, handleStringInput)
  }

  private def handleInputWithRetry[A](initialMessage: String, retryMessage: String, condition: A => Boolean, handleFunction: String => A): A = {
    var input: A = handleFunction(initialMessage)
    var b: Boolean = condition(input)
    while (!b) {
      input = handleFunction(retryMessage)
      b = condition(input)
    }
    input
  }

  def handleConfirmationOrDenial(message: String): Boolean = {
    val i: String = handleInputWithRetry[String](
      message + "(y/n)",
      "Invalid input, try again. (y/n)",
      i => i.equals("y") || i.equals("n"),
      handleStringInput
    )
    i.equals("y")
  }
}




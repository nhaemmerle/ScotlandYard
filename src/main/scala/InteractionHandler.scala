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

  def handleIntInputWithRetry(initialMessage: String, retryMessage: String, condition: Int => Boolean): Int = {
    var input: Int = handleIntInput(initialMessage)
    var b: Boolean = condition(input)
    while (!b) {
      input = handleIntInput(retryMessage)
      b = condition(input)
    }
    input
  }
}

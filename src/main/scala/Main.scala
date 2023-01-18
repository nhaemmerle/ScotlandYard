import scala.io.StdIn.readLine
import characters.*

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

object Main {
  def main(args: Array[String]): Unit = {
    //prepare the players
    //randomStartCards represents a random suffling of the 18 start cards
    //TODO: replace "1 to 18" by the values of the real start cards
    val randomStartCards: IndexedSeq[Int] = scala.util.Random.shuffle(1 to 18)

    //init mrX
    println("Name of Player Mr.X:")
    val randomMrXInitField: Int = randomStartCards(0)
    val mrX = MrX(readLine(), randomMrXInitField)

    //init detectives
    val detectives: ListBuffer[Detective] = new ListBuffer[Detective]()
    breakable {
      while (true) {
        if detectives.length >= 5 then break
        val nextPosition: Int = randomStartCards(detectives.length + 1)
        val newDetective: Detective = readDetective(detectives.length, nextPosition)
        if newDetective == null then
          if detectives.length < 2 then
            println("More detectives needed!")
          else break
        else detectives += newDetective
      }
    }
    mrX.setBlackTickets(detectives.length)
  }

  private def readDetective(numDetectives: Int, nextPosition: Int): Detective = {
    println("Name of Detective " + (numDetectives + 1) + ": (Enter \"q\" to quit adding players)")
    val input: String = readLine()
    if input.equals("q") then return null
    Detective(input, nextPosition)
  }
}

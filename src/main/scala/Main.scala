import scala.io.StdIn.readLine
import characters.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.*

object Main {
  def main(args: Array[String]): Unit = {
    //prepare the players
    // 18 initial start positions in the board
    val startCards = ListBuffer(13, 26, 29, 34, 51, 53, 91, 94, 103, 112, 117, 132, 138, 141, 155, 174, 197, 198)
    //randomStartCards represents a random suffling of the 18 start cards
    val randomStartCards: ListBuffer[Int] = scala.util.Random.shuffle(startCards)

    //init mrX
    println("Name of Player Mr.X:")
    val randomMrXInitField: Int = randomStartCards.head
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

    //add all players to the queue
    var playerQueue: mutable.Queue[PlayerCharacter] = mutable.Queue[PlayerCharacter](mrX)
    playerQueue.enqueueAll(detectives)

    // game loop
    var round = 1
    while (true) {
      gameLoop(round, mrX, detectives)
      round += 1
    }
  }

  private def readDetective(numDetectives: Int, nextPosition: Int): Detective = {
    println("Name of Detective " + (numDetectives + 1) + ": (Enter \"q\" to quit adding players)")
    val input: String = readLine()
    if input.equals("q") then return null
    Detective(input, nextPosition)
  }

  private def secondGameLoop(playerQueue: mutable.Queue[PlayerCharacter]): Unit = {
    val currentPlayer: PlayerCharacter = playerQueue.dequeue()
    var validMoves: List[Int] = List[Int]()


    playerQueue.enqueue(currentPlayer)
  }

  private def gameLoop(round: Int, mrX: MrX, detectives: ListBuffer[Detective]): Unit = {
    // Move MrX position
    var validMoves = List[Int]()
    validMoves = Board.taxiMap.get(mrX.currentLocation) match
      case Some(l) => l
      case None => List()
    validMoves = validMoves ++ (Board.busMap.get(mrX.currentLocation) match
      case Some(l) => l
      case None => List())
    validMoves = validMoves ++ (Board.undergroundMap.get(mrX.currentLocation) match
      case Some(l) => l
      case None => List())
    validMoves = validMoves ++ (Board.blackMap.get(mrX.currentLocation) match
      case Some(l) => l
      case None => List())

    // TODO : check if someone is currently at a valid moves position
    println(s"${mrX.name}, you are currently at location ${mrX.currentLocation}. You can move to: ${validMoves.mkString(", ")}")
    print("Enter the number of the location you want to move to: ")

    val move = scala.io.StdIn.readInt()
    if (validMoves.contains(move)) {
      mrX.currentLocation = move
      println(s"${mrX.name} has moved to location ${mrX.currentLocation}")
    } else {
      // TODO try again tut noch nicht
      println("Invalid move. Please try again.")
    }

    // Move Player positions
    for (detective <- detectives) {
      // check if game is over
      if detective.currentLocation == mrX.currentLocation then {
        println("The detectives win")
      } else if round >= 24 then {
        println("Mr. X wins!")
      }
      validMoves = List[Int]()
      validMoves = Board.taxiMap.get(detective.currentLocation) match
        case Some(l) => l
        case None => List()
      validMoves = validMoves ++ (Board.busMap.get(detective.currentLocation) match
        case Some(l) => l
        case None => List())
      validMoves = validMoves ++ (Board.undergroundMap.get(detective.currentLocation) match
        case Some(l) => l
        case None => List())
      // TODO : check if someone is currently at a valid moves position
      println(s"${detective.name}, you are currently at location ${detective.currentLocation}. You can move to: ${validMoves.mkString(", ")}")
      print("Enter the number of the location you want to move to: ")
      val move = scala.io.StdIn.readInt()
      if (validMoves.contains(move)) {
        detective.currentLocation = move
        println(s"${detective.name} has moved to location ${detective.currentLocation}")
      } else {
        // TODO try again tut noch nicht
        println("Invalid move. Please try again.")
      }
    }
    println(s"Round $round is finished")
  }
}


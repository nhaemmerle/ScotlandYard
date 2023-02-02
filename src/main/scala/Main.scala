import board.MapType.{BLACK, BUS, TAXI, UNDERGROUND}
import board.{Board, MapType}

import scala.io.StdIn.readLine
import characters.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.*

object Main {
  type Board = Map[MapType, Map[Int, List[Int]]]

  def main(args: Array[String]): Unit = {
    //prepare the players
    // 18 initial start positions in the board
    val startCards = ListBuffer(13, 26, 29, 34, 51, 53, 91, 94, 103, 112, 117, 132, 138, 141, 155, 174, 197, 198)
    //randomStartCards represents a random shuffling of the 18 start cards
    val randomStartCards: ListBuffer[Int] = scala.util.Random.shuffle(startCards)

    //init mrX
    val mrX = MrX(InteractionHandler.handleStringInput("Name of Player Mr.X:"), randomStartCards.head)

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
    val playerQueue: mutable.Queue[PlayerCharacter] = mutable.Queue[PlayerCharacter](mrX)
    playerQueue.enqueueAll(detectives)

    //init board
    val board: Board = Map(
      TAXI -> Board.taxiMap,
      BUS -> Board.busMap,
      UNDERGROUND -> Board.undergroundMap,
      BLACK -> Board.blackMap
    )

    // game loop
    //TODO: das mit numberOfMove hab ich nur auf die schnelle gemacht sodass es passt; kann man bestimmt schöner machen
    var round: Int = 1
    var numberOfMove: Int = 1
    breakable {
      while (true) {
        performOneMove(playerQueue, board)
        numberOfMove = (numberOfMove + 1) % (playerQueue.length + 1)
        round += numberOfMove / playerQueue.length
        if checkForWinCondition(round, mrX, detectives) then break
      }
    }
  }

  private def readDetective(numDetectives: Int, nextPosition: Int): Detective = {
    //TODO: maybe implement method in InteractionHandler for multiple inputs with break condition (to simplify detective creation)?
    val input: String = InteractionHandler.handleStringInput("Name of Detective " + (numDetectives + 1) + ": (Enter \"q\" to quit adding players)")
    if input.equals("q") then return null
    Detective(input, nextPosition)
  }

  private def performOneMove(playerQueue: mutable.Queue[PlayerCharacter], board: Board): Unit = {
    //dequeue to get the current player
    val currentPlayer: PlayerCharacter = playerQueue.dequeue()
    var possibleMoves: Map[MapType, List[Int]] = Map[MapType, List[Int]]()

    //get the reachable fields for each means of transport
    //TODO: filter possibleMoves with the following criteria
    // - destination has to be empty (no other player)
    // - player needs to have a corresponding ticket
    board.foreach(map => {
      possibleMoves = possibleMoves + (
        map._1 -> (map._2.get(currentPlayer.location) match
          case Some(l) => l
          case None => List())
        )
    })

    //let the player make a move
    val move: Int = InteractionHandler.handleIntInputWithRetry(
      s"${currentPlayer.name}, you are currently at location ${currentPlayer.location}." +
        s" You can move to: ${possibleMoves.mkString(", ")}",
      "Invalid move. Please try again.",
      i => possibleMoves.get(TAXI) match
        case Some(l) => l.contains(i)
        case None => false
    )
    currentPlayer.location = move

    //eventually enqueue the current player again
    playerQueue.enqueue(currentPlayer)
  }

  private def checkForWinCondition(round: Int, mrX: MrX, detectives: ListBuffer[Detective]): Boolean = {
    //FIXME: non local returns no longer supported (?)
    for (detective <- detectives) {
      if detective.location == mrX.location then {
        println("The detectives win")
        return true
      } else if round >= 24 then {
        println("Mr. X wins!")
        return true
      }
    }
    false
  }
}


import board.MapType.*
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
    val testStartCards = ListBuffer(1, 2, 3, 4, 5)
    //randomStartCards represents a random shuffling of the 18 start cards
    //TODO: implement as queue/stack/...
    val randomStartCards: ListBuffer[Int] = scala.util.Random.shuffle(testStartCards)

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
        //TODO: spawn 4 detectives if only 3 players (or at least raise warning)
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
      TAXI -> Board.testTaxiMap,
      BUS -> Board.busMap,
      UNDERGROUND -> Board.undergroundMap,
      BOAT -> Board.boatMap
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
    var possibleMoves: Map[MapType, List[Int]] = getPossibleMoves(board, currentPlayer, playerQueue)

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
    //TODO: decrease corresponding number of tickets

    //eventually enqueue the current player again
    playerQueue.enqueue(currentPlayer)
  }

  private def getPossibleMoves(board: Board, currentPlayer: PlayerCharacter, playerQueue: mutable.Queue[PlayerCharacter]): Map[MapType, List[Int]] = {
    //get the reachable fields for each means of transport
    //TODO: filter possibleMoves with the following criteria
    // - player needs to have a corresponding ticket

    //get positions blocked by other detectives
    var blockedPositions: ListBuffer[Int] = ListBuffer[Int]()
    playerQueue.foreach(
      player => {
        blockedPositions = blockedPositions ++ (player match
          case d: Detective => ListBuffer(d.location)
          case _: MrX => ListBuffer())
      }
    )

    var possibleMoves: Map[MapType, List[Int]] = Map[MapType, List[Int]]()
    board.foreach(map => {
      possibleMoves = possibleMoves + (
        map._1 -> (map._2.get(currentPlayer.location) match
          // remove blocked positions from possible moves
          case Some(l) => l.filterNot(blockedPositions.toSet)
          case None => List())
        )
    })
    possibleMoves
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


import characters.{Detective, MrX, PlayerCharacter, TicketType}
import board.{Board, MapType}
import characters.TicketType.{BLACK, UNDERGROUND}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object MoveHandler {
  /**
   * Handles interaction with current player, moves the player and increases corresponding ticket
   */
  def move(currentPlayer: PlayerCharacter, playerQueue: mutable.Queue[PlayerCharacter]): Unit = {
    //double move
    val doubleMove: Boolean = currentPlayer match
      case x: MrX => x.doubleTickets > 0 && InteractionHandler.handleConfirmationOrDenial("Do you want to make a double move?")
      case _ => false

    if doubleMove then performDoubleMove(currentPlayer, playerQueue) else performSingleMove(currentPlayer, playerQueue)

    currentPlayer match
      case x: MrX => if doubleMove then Main.mrXMoves += 2 else Main.mrXMoves += 1
      case _ =>
  }

  /**
   * Lets the player (mrX) make a double move which consumes two tickets.
   */
  private def performDoubleMove(currentPlayer: PlayerCharacter, playerQueue: mutable.Queue[PlayerCharacter]): Unit = {
    val possibleMoves: Map[(TicketType, TicketType), List[Int]] = getPossibleDoubleMoves(currentPlayer)
    var ticketCombination: (TicketType, TicketType) = null
    var destination: Int = -1
    if possibleMoves.isEmpty then {
      println("No double moves possible! Please perform single move.\n")
      performSingleMove(currentPlayer, playerQueue)
      return
    } else if possibleMoves.size == 1 then {
      //there is only one ticket combination, the player can choose the destination directly...
      println(s"The only possible ticket combination is: ${possibleMoves.head._1}.")
      ticketCombination = possibleMoves.head._1
    } else {
      //number the ticket combinations to make player interaction a bit easier
      var combinationNumbering: Map[Int, (TicketType, TicketType)] = Map()
      var message: String = "The possible move combinations are:\n\n"
      var c: Int = 1
      //list the possible ticket combinations
      for (move <- possibleMoves) {
        message = message + s"$c): ${move._1} -> ${move._2} \n"
        combinationNumbering = combinationNumbering + (c -> move._1)
        c += 1
      }

      //first, get the desired ticket combination
      println(message + "\n")
      ticketCombination = combinationNumbering(InteractionHandler.handleIntInputWithRetry(
        s"Please choose one combination (1 - ${c - 1})",
        "Invalid ticket combination, try again.",
        i => i > 0 && i < c
      ))
    }

    //get the destination
    destination = InteractionHandler.handleIntInputWithRetry(
      s"The possible destinations are: ${possibleMoves(ticketCombination)}. Which destination?",
      "Invalid destination, try again.",
      i => possibleMoves(ticketCombination).contains(i)
    )

    //alter player properties (i.e. move player and remove tickets)
    currentPlayer.location = destination
    currentPlayer.tickets = currentPlayer.tickets + (ticketCombination._1 -> (currentPlayer.tickets(ticketCombination._1) - 1))
    currentPlayer.tickets = currentPlayer.tickets + (ticketCombination._2 -> (currentPlayer.tickets(ticketCombination._2) - 1))
  }

  private def performSingleMove(currentPlayer: PlayerCharacter, playerQueue: mutable.Queue[PlayerCharacter]): Unit = {
    val possibleMoves: Map[TicketType, List[Int]] = getPossibleMoves(currentPlayer, playerQueue)
    //let the player make a move
    val move: Int = InteractionHandler.handleIntInputWithRetry(
      s"${currentPlayer.name}, you are currently at location ${currentPlayer.location}." +
        s" You can move to: ${possibleMoves.mkString(", ")}",
      "Invalid move. Please try again.",
      i => possibleMoves.foldLeft(false)((x, tuple) => x || tuple._2.contains(i))
    )

    //get ticket that shall be removed
    val correspondingTickets: List[TicketType] = possibleMoves.foldLeft(List[TicketType]())((x, tuple) => if tuple._2.contains(move) then tuple._1 :: x else x)
    var ticketChoice: TicketType = null
    if correspondingTickets.length > 1 then {
      //TODO: let player enter number instead of string
      ticketChoice = TicketType.valueOf(InteractionHandler.handleStringInputWithRetry(
        "Choose ticket:",
        "Invalid input, try again.",
        i => correspondingTickets.contains(TicketType.valueOf(i))
      ))
    } else {
      ticketChoice = correspondingTickets.head
    }

    //alter player properties (i.e. move player and remove ticket)
    currentPlayer.location = move
    currentPlayer.tickets = currentPlayer.tickets + (ticketChoice -> (currentPlayer.tickets(ticketChoice) - 1))

    //move ticket to mrX
    currentPlayer match
      case _: Detective =>
        playerQueue.foreach(
          player => player match
            case x: MrX => x.tickets = x.tickets + (ticketChoice -> (x.tickets(ticketChoice) + 1))
            case _ =>
        )
      case _ =>
  }

  private def getPossibleMoves(currentPlayer: PlayerCharacter, playerQueue: mutable.Queue[PlayerCharacter]): Map[TicketType, List[Int]] = {
    //get positions blocked by other detectives
    var blockedPositions: ListBuffer[Int] = ListBuffer[Int]()
    currentPlayer match
      case _: Detective =>
        playerQueue.foreach(
          player => {
            blockedPositions = blockedPositions ++ (player match
              case d: Detective => ListBuffer(d.location)
              case _ => ListBuffer())
          }
        )
      case _ =>

    getPossibleMovesStatic(currentPlayer.tickets, currentPlayer.location, blockedPositions)
  }

  private def getPossibleMovesStatic(tickets: Map[TicketType, Int], location: Int, blockedPositions: ListBuffer[Int]): Map[TicketType, List[Int]] = {
    val availableTickets: Map[TicketType, Int] = tickets.filter(ticket => ticket._2 > 0)
    availableTickets.map(ticket => (ticket._1, getPossibleMovesForTicketType(location, ticket._1, blockedPositions))).filter(tuple => tuple._2.nonEmpty)
  }

  private def getPossibleMovesForTicketType(position: Int, ticketType: TicketType, blockedFields: ListBuffer[Int]): List[Int] = {
    var possibleMoves: ListBuffer[Int] = ListBuffer()
    ticketType match
      case TicketType.TAXI => possibleMoves = getPossibleMovesForMapType(position, MapType.TAXI, blockedFields)
      case TicketType.BUS => possibleMoves = getPossibleMovesForMapType(position, MapType.BUS, blockedFields)
      case TicketType.UNDERGROUND => possibleMoves = getPossibleMovesForMapType(position, MapType.UNDERGROUND, blockedFields)
      case TicketType.BLACK => MapType.values.foreach(mapType => possibleMoves = possibleMoves ++ getPossibleMovesForMapType(position, mapType, blockedFields))

    possibleMoves.toList
  }

  private def getPossibleMovesForMapType(position: Int, mapType: MapType, blockedFields: ListBuffer[Int]): ListBuffer[Int] = {
    Board.board.get(mapType) match
      case Some(m) => m.get(position) match
        case Some(l) => (ListBuffer.empty ++= l).filterNot(blockedFields.toSet)
        case None => ListBuffer()
      case None => ListBuffer()
  }

  /**
   * Gets the destinations the player can reach in two steps
   */
  private def getPossibleDoubleMoves(currentPlayer: PlayerCharacter): Map[(TicketType, TicketType), List[Int]] = {
    var possibleMoves: Map[(TicketType, TicketType), List[Int]] = Map()

    //first step
    val possibleFirstSteps: Map[TicketType, List[Int]] = getPossibleMovesStatic(currentPlayer.tickets, currentPlayer.location, ListBuffer())

    //second step
    var possibleSecondSteps: Map[TicketType, List[(Int, Map[TicketType, List[Int]])]] =
      possibleFirstSteps.map((ticketType: TicketType, fields: List[Int]) => ticketType -> fields.map(field => (field, getPossibleMovesStatic(
        currentPlayer.tickets + (ticketType -> (currentPlayer.tickets(ticketType) - 1)),
        field,
        ListBuffer()
      ))))

    //zip together
    possibleSecondSteps.foreach((firstStepTicketType, firstStepFields) => {
      firstStepFields.foreach((_, secondSteps) => {
        secondSteps.foreach((secondStepTicketType, destinations) => {
          possibleMoves = possibleMoves + ((firstStepTicketType, secondStepTicketType) -> destinations)
        })
      })
    })

    possibleMoves
  }

}

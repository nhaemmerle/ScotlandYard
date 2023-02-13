import Main.Board
import characters.{Detective, PlayerCharacter, TicketType}
import board.MapType

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object MoveHandler {
  /**
   * Handles interaction with current player, moves the player and increases corresponding ticket
   */
  def move(currentPlayer: PlayerCharacter, playerQueue: mutable.Queue[PlayerCharacter], board: Board): Unit = {
    val possibleMoves: Map[TicketType, List[Int]] = getPossibleMoves(currentPlayer, playerQueue, board)

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
    val numTickets: Int = currentPlayer.tickets.get(ticketChoice) match
      case Some(value) => value
      case None => 0
    currentPlayer.tickets = currentPlayer.tickets + (ticketChoice -> (numTickets - 1))
  }

  private def getPossibleMoves(currentPlayer: PlayerCharacter, playerQueue: mutable.Queue[PlayerCharacter], board: Board): Map[TicketType, List[Int]] = {
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

    val availableTickets: Map[TicketType, Int] = currentPlayer.tickets.filter(ticket => ticket._2 > 0)
    availableTickets.map(ticket => (ticket._1, getPossibleMovesForTicketType(currentPlayer.location, board, ticket._1, blockedPositions))).filter(tuple => tuple._2.nonEmpty)
  }

  private def getPossibleMovesForTicketType(position: Int, board: Board, ticketType: TicketType, blockedFields: ListBuffer[Int]): List[Int] = {
    var possibleMoves: ListBuffer[Int] = ListBuffer()
    ticketType match
      case TicketType.TAXI => possibleMoves = getPossibleMovesForMapType(position, board, MapType.TAXI, blockedFields)
      case TicketType.BUS => possibleMoves = getPossibleMovesForMapType(position, board, MapType.BUS, blockedFields)
      case TicketType.UNDERGROUND => possibleMoves = getPossibleMovesForMapType(position, board, MapType.UNDERGROUND, blockedFields)
      case TicketType.BLACK => MapType.values.foreach(mapType => possibleMoves = possibleMoves ++ getPossibleMovesForMapType(position, board, mapType, blockedFields))

    possibleMoves.toList
  }

  private def getPossibleMovesForMapType(position: Int, board: Board, mapType: MapType, blockedFields: ListBuffer[Int]): ListBuffer[Int] = {
    board.get(mapType) match
      case Some(m) => m.get(position) match
        case Some(l) => (ListBuffer.empty ++= l).filterNot(blockedFields.toSet)
        case None => ListBuffer()
      case None => ListBuffer()
  }

}

import characters.{Detective, MrX, MrXKI, PlayerCharacter, TicketType}
import board.{Board, MapType}
import characters.TicketType.{BLACK, UNDERGROUND}
import MoveHandler.{getPossibleDoubleMoves, getPossibleMoves}
import characters.KILevel.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

object KIMoveHandler {
  def performKIMove(ki: MrXKI, playerQueue: mutable.Queue[PlayerCharacter]): Unit = {

    // possible next moves of ki
    val possibleMoves: Map[TicketType, List[Int]] = getPossibleMoves(ki.tickets, ki.location, playerQueue)
    var move: (TicketType, List[Int]) = possibleMoves.maxBy(_ => Random.nextInt)
    var ticketChoice = move._1

    // possible next moves of Detectives
    val possibleMovesDetective = getPossibleMovesDetectives(playerQueue)
    // Filter for moves that cant be reached by detectives in one move
    val filteredPossibleMoves = possibleMoves
      .map((ticketType, destinations: List[Int]) => (ticketType, destinations.filterNot(possibleMovesDetective.toSet)))
      .filter((_, xs: List[Int]) => xs.nonEmpty)
    val possibleDoubleMoves: Map[(TicketType, TicketType, Int), List[Int]] = getPossibleDoubleMoves(ki, playerQueue)
    val filteredPossibleDoubleMoves = possibleDoubleMoves
      .map((ticketType, destinations: List[Int]) => (ticketType, destinations.filterNot(possibleMovesDetective.toSet)))
      .filter((_, xs: List[Int]) => xs.nonEmpty)

    //TODO if else mit match und wieder einzelnen funktionen ersetzen?
    if ki.isLevel(Medium) || ki.isLevel(Hard) then
      if filteredPossibleMoves.nonEmpty then
        // move to position that cant be reached by detectives
        move = filteredPossibleMoves.maxBy(_ => Random.nextInt)
        ticketChoice = move._1
        return
      else if ki.isLevel(Hard) && filteredPossibleDoubleMoves.nonEmpty then
        val doubleMove = filteredPossibleDoubleMoves.maxBy(_ => Random.nextInt)
        val ticketTriple = doubleMove._1
        val firstMove: Int = ticketTriple._3
        val secondMove: Int = doubleMove._2.maxBy(_ => Random.nextInt)
        updateMrX(ki, firstMove, ticketTriple._1)
        updateMrX(ki, secondMove, ticketTriple._2)
        return
      else
        None
    // general case if single and double move are unfeasible or ki level Easy
    updateMrX(ki, move._2.maxBy(_ => Random.nextInt), ticketChoice)
  }

  private def getPossibleMovesDetectives(playerQueue: mutable.Queue[PlayerCharacter]): List[Int] = {
    var possibleMovesDetectives: List[Int] = List()
    playerQueue.foreach(
      player => {
        val possibleMoves: Map[TicketType, List[Int]] = getPossibleMoves(player.tickets, player.location, playerQueue)
        possibleMovesDetectives = possibleMovesDetectives ++ possibleMoves.values.flatten
      }

    )
    return possibleMovesDetectives.distinct
  }

  private def updateMrX(ki: MrXKI, location: Int, ticketChoice: TicketType): Unit = {
    //alter mrX properties (i.e. move player and remove ticket)
    ki.location = location
    ki.tickets = ki.tickets + (ticketChoice -> (ki.tickets(ticketChoice) - 1))
    Main.mrXMoves.append((ticketChoice, location))
    //TODO potentiell print wieder entfernen
    println(s"KI Location: ${ki.location}")
    println(s"KI last Ticket: ${ticketChoice}")
  }

}



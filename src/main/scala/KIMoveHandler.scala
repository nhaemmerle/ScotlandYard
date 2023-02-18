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
    (ki.level match
      case Easy => performEasyMove
      case Medium => performMediumMove
      case Hard => performHardMove)(ki, playerQueue)
  }

  private def performHardMove(ki: MrXKI, playerQueue: mutable.Queue[PlayerCharacter]): Unit = {
    // possible next moves of Detectives
    val possibleMovesDetective = getPossibleMovesDetectives(playerQueue)
    // possible next moves of ki
    val possibleMoves: Map[TicketType, List[Int]] = getPossibleMoves(ki.tickets, ki.location, playerQueue)
    val filteredPossibleMoves = possibleMoves
      .map((ticketType, destinations: List[Int]) => (ticketType, destinations.filterNot(possibleMovesDetective.toSet)))
      .filter((_, xs: List[Int]) => xs.nonEmpty)

    val possibleDoubleMoves: Map[(TicketType, TicketType, Int), List[Int]] = getPossibleDoubleMoves(ki, playerQueue)
    val filteredPossibleDoubleMoves = possibleDoubleMoves
      .map((ticketType, destinations: List[Int]) => (ticketType, destinations.filterNot(possibleMovesDetective.toSet)))
      .filter((_, xs: List[Int]) => xs.nonEmpty)

    var move: (TicketType, List[Int]) = null
    var ticketChoice: TicketType = null

    if filteredPossibleMoves.nonEmpty then
      // move to position that cant be reached by detectives
      move = filteredPossibleMoves.maxBy(_ => Random.nextInt)
      ticketChoice = move._1
      //alter mrX properties (i.e. move player and remove ticket)
      ki.location = move._2.maxBy(_ => Random.nextInt)
      ki.tickets = ki.tickets + (ticketChoice -> (ki.tickets(ticketChoice) - 1))
      Main.mrXMoves.append(ki.location)
      //TODO potentiell print wieder entfernen
      println(ki.location)
    //TODO ALARM!!! das ist ganz böse und muss aufgeräumt werden
    else if filteredPossibleDoubleMoves.nonEmpty then
      val doubleMove = filteredPossibleDoubleMoves.maxBy(_ => Random.nextInt)
      val ticketTriple = doubleMove._1
      val firstMove: Int = ticketTriple._3
      val secondMove: Int = doubleMove._2.maxBy(_ => Random.nextInt)
      //alter player properties (i.e. move player, remove tickets and add moves to Main.mrXMoves)
      ki.location = secondMove
      ki.tickets = ki.tickets + (ticketTriple._1 -> (ki.tickets(ticketTriple._1) - 1))
      ki.tickets = ki.tickets + (ticketTriple._2 -> (ki.tickets(ticketTriple._2) - 1))
      Main.mrXMoves.append(firstMove)
      Main.mrXMoves.append(secondMove)
      println(ki.location)
    else
      // move to random position and try your Luck
      move = possibleMoves.maxBy(_ => Random.nextInt)
      ticketChoice = move._1
      //alter mrX properties (i.e. move player and remove ticket)
      ki.location = move._2.maxBy(_ => Random.nextInt)
      ki.tickets = ki.tickets + (ticketChoice -> (ki.tickets(ticketChoice) - 1))
      Main.mrXMoves.append(ki.location)
      //TODO potentiell print wieder entfernen
      println(ki.location)
      //TODO ALARM!!! das ist ganz böse und muss aufgeräumt werden

  }


  private def performMediumMove(ki: MrXKI, playerQueue: mutable.Queue[PlayerCharacter]): Unit = {
    // possible next moves of Detectives
    val possibleMovesDetective = getPossibleMovesDetectives(playerQueue)
    // possible next moves of ki
    val possibleMoves: Map[TicketType, List[Int]] = getPossibleMoves(ki.tickets, ki.location, playerQueue)
    val filteredPossibleMoves = possibleMoves
      .map((ticketType, destinations: List[Int]) => (ticketType, destinations.filterNot(possibleMovesDetective.toSet)))
      .filter((_, xs: List[Int]) => xs.nonEmpty)

    var move: (TicketType, List[Int]) = null
    var ticketChoice: TicketType = null

    if filteredPossibleMoves.nonEmpty then
      // move to position that cant be reached by detectives
      move = filteredPossibleMoves.maxBy(_ => Random.nextInt)
      ticketChoice = move._1
    else
      // move to random position and try your Luck
      move = possibleMoves.maxBy(_ => Random.nextInt)
      ticketChoice = move._1

    //alter mrX properties (i.e. move player and remove ticket)
    ki.location = move._2.maxBy(_ => Random.nextInt)
    ki.tickets = ki.tickets + (ticketChoice -> (ki.tickets(ticketChoice) - 1))
    Main.mrXMoves.append(ki.location)
    //TODO potentiell print wieder entfernen
    println(ki.location)
  }

  private def performEasyMove(ki: MrXKI, playerQueue: mutable.Queue[PlayerCharacter]): Unit = {
    val possibleMoves: Map[TicketType, List[Int]] = getPossibleMoves(ki.tickets, ki.location, playerQueue)
    //let the player make a move
    val move: (TicketType, List[Int]) = possibleMoves.maxBy(_ => Random.nextInt)
    val ticketChoice = move._1

    //alter mrX properties (i.e. move player and remove ticket)
    ki.location = move._2.maxBy(_ => Random.nextInt)
    ki.tickets = ki.tickets + (ticketChoice -> (ki.tickets(ticketChoice) - 1))
    Main.mrXMoves.append(ki.location)
    //TODO potentiell print wieder entfernen
    println(ki.location)
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
}

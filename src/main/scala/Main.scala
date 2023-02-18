import board.MapType.*
import board.{Board, MapType}

import scala.io.StdIn.readLine
import characters.*

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.*

object Main {
  //stores how many moves mrX has done (to check win condition and if mrX has to reveal himself)
  var mrXMoves: ListBuffer[Int] = ListBuffer()
  private var mrXLatestSeen: Int = -1
  // number of moves at which mrX has to show himself
  private val revealMrXFields: List[Int] = List(3, 8, 13, 18, 24)

  def main(args: Array[String]): Unit = {
    //prepare the players
    // 18 initial start positions in the board
    val startCards = ListBuffer(13, 26, 29, 34, 51, 53, 91, 94, 103, 112, 117, 132, 138, 141, 155, 174, 197, 198)
    val testStartCards = ListBuffer(1, 2, 3, 4, 5)
    //randomStartCards represents a random shuffling of the 18 start cards
    //TODO: implement as queue/stack/...
    //val randomStartCards: ListBuffer[Int] = scala.util.Random.shuffle(startCards)
    val randomStartCards: ListBuffer[Int] = testStartCards

    printWelcomeMessage()

    var mrX: MrX = null
    // Choose if you want MrX to be a KI
    if InteractionHandler.handleConfirmationOrDenial("Do you want MrX to be a computer Opponent ") then
      InteractionHandler.handleIntInputWithRetry(
        """
          |Choose KI Level:
          | 1) Basic
          | 2) Still Basic but better
          | 3) Ultra Hard
          |""".stripMargin, "Invalid choice try again", i => List(1,2,3).contains(i) ) match
        case 1 => mrX = MrXKI(KILevel.Easy, randomStartCards.head)
        case 2 => mrX = MrXKI(KILevel.Medium, randomStartCards.head)
        case 3 => mrX = MrXKI(KILevel.Hard, randomStartCards.head)
    else
      //init mrX
      mrX = MrX(InteractionHandler.handleStringInput("Name of Player Mr.X:"), randomStartCards.head)

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

    // game loop
    while (!checkForWinCondition(mrX, detectives)) {
      performOneMove(playerQueue)
    }
  }
  private def printWelcomeMessage(): Unit = {
    //println("  _________             __  .__                     .___ _____.___.                .___")
    //println(" /   _____/ ____  _____/  |_|  | _____    ____    __| _/ \\__  |   |____ _______  __| _/")
    //println(" \\_____  \\_/ ___\\/  _ \\   __\\  | \\__  \\  /    \\  / __ |   /   |   \\__  \\\\_  __ \\/ __ | ")
    //println(" /        \\  \\__(  <_> )  | |  |__/ __ \\|   |  \\/ /_/ |   \\____   |/ __ \\|  | \\/ /_/ | ")
    //println("/_______  /\\___  >____/|__| |____(____  /___|  /\\____ |   / ______(____  /__|  \\____ | ")
    //println("        \\/     \\/                     \\/     \\/      \\/   \\/           \\/           \\/ \n")

    println("   _____           _   _                 _  __     __           _ ")
    println("  / ____|         | | | |               | | \\ \\   / /          | |")
    println(" | (___   ___ ___ | |_| | __ _ _ __   __| |  \\ \\_/ /_ _ _ __ __| |")
    println("  \\___ \\ / __/ _ \\| __| |/ _` | '_ \\ / _` |   \\   / _` | '__/ _` |")
    println("  ____) | (_| (_) | |_| | (_| | | | | (_| |    | | (_| | | | (_| |")
    println(" |_____/ \\___\\___/ \\__|_|\\__,_|_| |_|\\__,_|    |_|\\__,_|_|  \\__,_|")

    //println("  ___         _   _              _  __   __           _ ")
    //println(" / __| __ ___| |_| |__ _ _ _  __| | \\ \\ / /_ _ _ _ __| |")
    //println(" \\__ \\/ _/ _ \\  _| / _` | ' \\/ _` |  \\ V / _` | '_/ _` |")
    //println(" |___/\\__\\___/\\__|_\\__,_|_||_\\__,_|   |_|\\__,_|_| \\__,_|")

  }
  private def readDetective(numDetectives: Int, nextPosition: Int): Detective = {
    //TODO: maybe implement method in InteractionHandler for multiple inputs with break condition (to simplify detective creation)?
    val input: String = InteractionHandler.handleStringInput("Name of Detective " + (numDetectives + 1) + ": (Enter \"q\" to quit adding players)")
    if input.equals("q") then return null
    Detective(input, nextPosition)
  }

  private def performOneMove(playerQueue: mutable.Queue[PlayerCharacter]): Unit = {
    //dequeue to get the current player
    val currentPlayer: PlayerCharacter = playerQueue.dequeue()

    //print ticket state of player
    println(s"\n\n+++++++++++ Current player: ${currentPlayer.name} +++++++++++\n\n")
    println(s"MrX latest seen at: $mrXLatestSeen")
    println(s"${currentPlayer.name}, your tickets are: ")
    println(currentPlayer.tickets)

    MoveHandler.move(currentPlayer, playerQueue)

    //reveal mrX
    relocateMrXLatestSeen(currentPlayer)

    //eventually enqueue the current player again
    playerQueue.enqueue(currentPlayer)
  }

  private def relocateMrXLatestSeen(currentPlayer: PlayerCharacter): Unit = {
    var latestSeen: Int = -1
    revealMrXFields.foreach(field => {
      if field <= mrXMoves.length then latestSeen = mrXMoves(field - 1)
    })
    mrXLatestSeen = latestSeen
  }

  private def checkForWinCondition(mrX: MrX, detectives: ListBuffer[Detective]): Boolean = {
    //FIXME: non local returns no longer supported (?)
    for (detective <- detectives) {
      if detective.location == mrX.location then {
        println("The detectives win")
        return true
      } else if mrXMoves.length >= 24 then {
        println("Mr. X wins!")
        return true
      }
    }
    false
  }
}


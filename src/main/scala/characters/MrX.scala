package characters
import board.MapType
import characters.TicketType.*

class MrX(name: String, currentLocation: Int) extends PlayerCharacter(name, currentLocation) {
  var doubleTickets = 2
  tickets = Map(
    TAXI -> 4,
    BUS -> 3,
    UNDERGROUND -> 3
  )
  
  def setBlackTickets(numDetectives: Int): Unit =
    tickets = tickets + (BLACK -> numDetectives)
  
}

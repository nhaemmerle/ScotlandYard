package characters
import board.MapType

class MrX(name: String, currentLocation: Int) extends PlayerCharacter(name, currentLocation) {
  var blackTickets = 0
  var doubleTickets = 2
  taxiTickets = 4
  busTickets = 3
  undergroundTickets = 3
  
  def setBlackTickets(numDetectives: Int): Unit =
    blackTickets = numDetectives

  override def getTickets(mapType: MapType): Int = super.getTickets(mapType) + blackTickets
  
}

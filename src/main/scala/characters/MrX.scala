package characters

class MrX(name: String, currentLocation: Int) extends PlayerCharacter(name, currentLocation) {
  taxiTickets = 4
  busTickets = 3
  subwayTickets = 3
  doubleTickets = 2

  def setBlackTickets(numDetectives: Int): Unit =
    blackTickets = numDetectives
}

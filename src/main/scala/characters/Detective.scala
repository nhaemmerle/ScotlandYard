package characters

class Detective(name: String, currentLocation: Int) extends PlayerCharacter(name, currentLocation) {
  taxiTickets = 10
  busTickets = 8
  subwayTickets = 4
}

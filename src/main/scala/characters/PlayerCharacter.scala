package characters

abstract class PlayerCharacter(var name: String, var currentLocation: Int) {
  var taxiTickets = 0
  var busTickets = 0
  var subwayTickets = 0
}

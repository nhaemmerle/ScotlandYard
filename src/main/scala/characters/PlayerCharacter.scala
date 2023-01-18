package characters

abstract class PlayerCharacter(var name: String, var currentLocation: Int) {
  var blackTickets = 0
  var doubleTickets = 0
  var taxiTickets = 0
  var busTickets = 0
  var subwayTickets = 0
}

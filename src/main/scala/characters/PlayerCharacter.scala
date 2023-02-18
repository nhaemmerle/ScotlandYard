package characters

abstract class PlayerCharacter(var name: String, var location: Int) {
  var tickets: Map[TicketType, Int] = Map()
}


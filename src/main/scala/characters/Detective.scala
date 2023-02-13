package characters

import characters.TicketType.{BUS, TAXI, UNDERGROUND}

class Detective(name: String, currentLocation: Int) extends PlayerCharacter(name, currentLocation) {
  tickets = Map(
    TAXI -> 10,
    BUS -> 8,
    UNDERGROUND -> 4
  )
}

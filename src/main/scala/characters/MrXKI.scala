package characters

import characters.TicketType.{BUS, TAXI, UNDERGROUND}


class MrXKI(val level: KILevel, currentLocation: Int) extends MrX("KI", currentLocation) {
  tickets = Map(
    TAXI -> 4,
    BUS -> 3,
    UNDERGROUND -> 3
  )
}



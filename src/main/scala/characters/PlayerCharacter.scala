package characters

import board.MapType
import board.MapType.{BOAT, BUS, TAXI, UNDERGROUND}

abstract class PlayerCharacter(var name: String, var location: Int) {
  var taxiTickets = 0
  var busTickets = 0
  var undergroundTickets = 0

  def getTickets(mapType: MapType): Int = {
    mapType match
      case TAXI => taxiTickets
      case BUS => busTickets
      case UNDERGROUND => undergroundTickets
      case BOAT => 0
  }
}

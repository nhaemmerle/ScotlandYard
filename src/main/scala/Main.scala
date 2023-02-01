import scala.io.StdIn.readLine
import characters.*

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

object Main {
  def main(args: Array[String]): Unit = {
    //prepare the players
    // 18 initial start positions in the board
    val startCards = ListBuffer(13, 26, 29, 34, 51, 53, 91, 94, 103, 112, 117, 132, 138, 141, 155, 174, 197, 198)
    //randomStartCards represents a random suffling of the 18 start cards
    val randomStartCards: ListBuffer[Int] = scala.util.Random.shuffle(startCards)

    //init mrX
    println("Name of Player Mr.X:")
    val randomMrXInitField: Int = randomStartCards.head
    val mrX = MrX(readLine(), randomMrXInitField)

    //init detectives
    val detectives: ListBuffer[Detective] = new ListBuffer[Detective]()
    breakable {
      while (true) {
        if detectives.length >= 5 then break
        val nextPosition: Int = randomStartCards(detectives.length + 1)
        val newDetective: Detective = readDetective(detectives.length, nextPosition)
        if newDetective == null then
          if detectives.length < 2 then
            println("More detectives needed!")
          else break
        else detectives += newDetective
      }
    }
    mrX.setBlackTickets(detectives.length)

    // game loop
    var round = 1
    while (true) {
      // Move MrX position
      var validMoves = List[Int]()
      validMoves = taxiMap.get(mrX.currentLocation) match
        case Some(l) => l
        case None => List()
      validMoves = validMoves ++ (busMap.get(mrX.currentLocation) match
        case Some(l) => l
        case None => List())
      validMoves = validMoves ++ (undergroundMap.get(mrX.currentLocation) match
        case Some(l) => l
        case None => List())
      validMoves = validMoves ++ (blackMap.get(mrX.currentLocation) match
        case Some(l) => l
        case None => List())
      // TODO : check if someone is currently at a valid moves position
      println(s"${mrX.name}, you are currently at location ${mrX.currentLocation}. You can move to: ${validMoves.mkString(", ")}")
      print("Enter the number of the location you want to move to: ")
      val move = scala.io.StdIn.readInt()
      if (validMoves.contains(move)) {
        mrX.currentLocation = move
        println(s"${mrX.name} has moved to location ${mrX.currentLocation}")
      } else {
        // TODO try again tut noch nicht
        println("Invalid move. Please try again.")
      }
      // Move Player positions
      for (detective <- detectives) {
        // check if game is over
        if detective.currentLocation == mrX.currentLocation then {
          println("The detectives win")
        } else if round >= 24 then {
          println("Mr. X wins!")
        }
        validMoves = List[Int]()
        validMoves = taxiMap.get(detective.currentLocation) match
          case Some(l) => l
          case None => List()
        validMoves = validMoves ++ (busMap.get(detective.currentLocation) match
          case Some(l) => l
          case None => List())
        validMoves = validMoves ++ (undergroundMap.get(detective.currentLocation) match
          case Some(l) => l
          case None => List())
        // TODO : check if someone is currently at a valid moves position
        println(s"${detective.name}, you are currently at location ${detective.currentLocation}. You can move to: ${validMoves.mkString(", ")}")
        print("Enter the number of the location you want to move to: ")
        val move = scala.io.StdIn.readInt()
        if (validMoves.contains(move)) {
          detective.currentLocation = move
          println(s"${detective.name} has moved to location ${detective.currentLocation}")
        } else {
          // TODO try again tut noch nicht
          println("Invalid move. Please try again.")
        }
      }
      println(s"Round ${round} is finished")
      round += 1
    }
  }

  private def readDetective(numDetectives: Int, nextPosition: Int): Detective = {
    println("Name of Detective " + (numDetectives + 1) + ": (Enter \"q\" to quit adding players)")
    val input: String = readLine()
    if input.equals("q") then return null
    Detective(input, nextPosition)
  }

  val taxiMap: Map[Int, List[Int]] = Map(
    0 -> List(),
    1 -> List(8, 9),
    8 -> List(1, 18, 19),
    9 -> List(1, 19, 20),
    2 -> List(20, 10),
    20 -> List(2, 9, 33),
    10 -> List(2, 11, 21, 34),
    3 -> List(11, 12, 4),
    11 -> List(3, 10, 22),
    12 -> List(3, 23),
    4 -> List(3, 13),
    13 -> List(4, 23, 14, 24),
    5 -> List(15, 16),
    15 -> List(5, 14, 16, 26, 28),
    16 -> List(5, 15, 28, 29),
    6 -> List(29, 7),
    29 -> List(6, 16, 17, 41, 42),
    7 -> List(6, 17),
    17 -> List(7, 29, 30),
    18 -> List(8, 31, 43),
    19 -> List(8, 9, 32),
    21 -> List(10, 33),
    34 -> List(10, 22, 47, 48),
    22 -> List(11, 34, 23, 35),
    23 -> List(12, 13, 22, 37),
    14 -> List(13, 15, 25),
    24 -> List(13, 37, 38),
    25 -> List(14, 38, 39),
    26 -> List(15, 39, 27),
    28 -> List(15, 16, 27, 41),
    30 -> List(17, 42),
    31 -> List(18, 43, 44),
    43 -> List(18, 31, 57),
    32 -> List(19, 44, 33, 45),
    33 -> List(20, 21, 32, 46),
    35 -> List(22, 36, 48, 65),
    37 -> List(23, 24, 36, 50),
    38 -> List(24, 25, 50, 51),
    39 -> List(25, 26, 51, 52),
    27 -> List(26, 28, 40),
    40 -> List(27, 52, 41, 53),
    41 -> List(28, 29, 40, 54),
    42 -> List(29, 30, 56, 72),
    44 -> List(31, 32, 58),
    45 -> List(32, 58, 60, 59, 46),
    46 -> List(33, 45, 47, 61),
    47 -> List(34, 46, 62),
    48 -> List(34, 35, 62, 63),
    36 -> List(35, 37, 49),
    65 -> List(35, 64, 66, 82),
    49 -> List(36, 50, 66),
    50 -> List(37, 38, 49),
    51 -> List(38, 39, 67, 52, 68),
    52 -> List(39, 40, 51, 69),
    53 -> List(40, 54, 69),
    54 -> List(41, 53, 55, 70),
    56 -> List(42, 91),
    72 -> List(42, 71, 91, 90),
    57 -> List(43, 58, 73),
    58 -> List(44, 45, 57, 59, 74, 75),
    60 -> List(45, 61, 76),
    59 -> List(45, 58, 75, 76),
    61 -> List(46, 60, 62, 76, 78),
    62 -> List(47, 48, 61, 79),
    63 -> List(48, 64, 80, 79),
    66 -> List(49, 65, 67, 82),
    67 -> List(51, 66, 68, 84),
    68 -> List(51, 67, 69, 85),
    69 -> List(52, 53, 68, 86),
    55 -> List(54, 71),
    70 -> List(54, 71, 87),
    71 -> List(55, 70, 72, 89),
    91 -> List(56, 72, 90, 105, 107),
    73 -> List(57, 74, 92),
    74 -> List(58, 73, 92, 75),
    75 -> List(58, 59, 74, 94),
    76 -> List(59, 60, 61, 77),
    78 -> List(61, 77, 79, 97),
    79 -> List(62, 63, 78, 98),
    64 -> List(63, 65, 81),
    80 -> List(63, 99, 100),
    81 -> List(64, 82, 100),
    82 -> List(65, 66, 81, 101),
    84 -> List(67, 85),
    85 -> List(68, 84, 103),
    86 -> List(69, 103, 104),
    87 -> List(70, 88),
    89 -> List(71, 88, 105),
    90 -> List(72, 105, 91),
    92 -> List(73, 74, 93),
    94 -> List(75, 93, 95),
    77 -> List(76, 78, 95, 96),
    95 -> List(77, 94, 122),
    96 -> List(77, 97, 109),
    97 -> List(78, 96, 98, 109),
    98 -> List(79, 97, 99, 110),
    99 -> List(80, 98, 110, 112),
    100 -> List(80, 81, 101, 112, 113),
    101 -> List(82, 83, 100, 114),
    83 -> List(101, 102),
    102 -> List(83, 103, 115),
    103 -> List(85, 86, 102),
    104 -> List(86, 116),
    88 -> List(87, 117, 89),
    117 -> List(88, 108, 116, 129),
    105 -> List(89, 90, 91, 106, 108),
    107 -> List(91, 106, 119),
    93 -> List(92, 94),
    122 -> List(95, 121, 123, 146),
    109 -> List(96, 97, 110, 124),
    110 -> List(98, 99, 109, 111),
    112 -> List(99, 100, 111, 125),
    113 -> List(100, 114, 125),
    114 -> List(101, 113, 115, 126, 131, 132),
    115 -> List(102, 114, 127, 126),
    116 -> List(104, 117, 118, 127),
    106 -> List(105, 107),
    108 -> List(105, 117, 119),
    119 -> List(107, 108, 136),
    124 -> List(109, 111, 123, 138, 130, 138),
    111 -> List(110, 112, 124),
    125 -> List(112, 113, 131),
    126 -> List(114, 115, 127, 140),
    131 -> List(114, 125, 130),
    132 -> List(114, 140),
    127 -> List(115, 116, 126, 133, 134),
    118 -> List(116, 129, 134, 142),
    129 -> List(117, 118, 135, 142, 143),
    134 -> List(118, 127, 141, 142),
    142 -> List(118, 129, 134, 141, 143, 128, 158),
    136 -> List(119, 135, 162),
    120 -> List(121, 144),
    121 -> List(120, 122, 145),
    144 -> List(120, 145, 177),
    145 -> List(121, 144, 146),
    123 -> List(122, 124, 137, 148, 149),
    146 -> List(122, 145, 147, 163),
    137 -> List(123, 147),
    148 -> List(123, 149, 164),
    149 -> List(123, 148, 150, 165),
    138 -> List(124, 124, 150, 152),
    130 -> List(124, 131, 139),
    140 -> List(126, 132, 133, 139, 154, 156),
    133 -> List(127, 141, 140),
    128 -> List(188, 142, 143, 160, 172),
    188 -> List(128, 173, 187, 199),
    135 -> List(129, 136, 143, 161),
    143 -> List(129, 135, 142, 128, 160),
    139 -> List(130, 140, 154, 153),
    141 -> List(133, 134, 142, 158),
    161 -> List(135, 160, 174),
    162 -> List(136, 175),
    147 -> List(137, 146, 164),
    150 -> List(138, 149, 151),
    152 -> List(138, 151, 153),
    154 -> List(139, 140, 153, 155),
    153 -> List(139, 152, 154, 166, 167),
    156 -> List(140, 155, 157, 169),
    158 -> List(141, 142, 157, 159),
    160 -> List(143, 161, 173, 128),
    177 -> List(144, 163, 176),
    163 -> List(146, 177),
    164 -> List(147, 148, 178, 179),
    165 -> List(149, 151, 179, 180),
    151 -> List(150, 152, 165, 166),
    166 -> List(151, 153, 181, 183),
    167 -> List(153, 155, 168, 183),
    155 -> List(154, 167, 168, 156),
    168 -> List(155, 167, 184),
    157 -> List(156, 158, 170),
    169 -> List(156, 184),
    170 -> List(157, 159, 185),
    159 -> List(158, 170, 172, 186, 198),
    172 -> List(159, 187, 128),
    186 -> List(159, 185, 198),
    198 -> List(159, 186, 187, 199),
    173 -> List(160, 171, 174, 188),
    174 -> List(161, 173, 175),
    175 -> List(162, 171, 174),
    178 -> List(164, 189, 191),
    179 -> List(164, 165, 191),
    180 -> List(165, 181, 193),
    181 -> List(166, 180, 193, 182),
    183 -> List(166, 167, 182, 196),
    184 -> List(168, 169, 196, 185, 197),
    185 -> List(170, 184, 186),
    171 -> List(173, 175, 199),
    199 -> List(171, 188, 198),
    187 -> List(172, 188, 198),
    176 -> List(177, 189),
    189 -> List(176, 178, 190),
    191 -> List(178, 179, 190, 192),
    193 -> List(180, 181, 194),
    182 -> List(181, 183, 195),
    195 -> List(182, 194, 197),
    196 -> List(183, 184, 197),
    197 -> List(184, 195, 196),
    190 -> List(189, 191, 192),
    192 -> List(190, 191, 194),
    194 -> List(192, 193, 195)
  )
  val busMap: Map[Int, List[Int]] = Map(
    1 -> List(58, 46),
    58 -> List(1, 46, 74, 77),
    46 -> List(1, 34, 58, 78),
    3 -> List(22, 23),
    22 -> List(3, 34, 23, 65),
    23 -> List(3, 13, 22, 67),
    7 -> List(42),
    42 -> List(7, 29, 72),
    13 -> List(23, 14, 52),
    14 -> List(13, 15),
    52 -> List(13, 41, 67, 86),
    15 -> List(14, 41, 29),
    41 -> List(15, 29, 52, 87),
    29 -> List(15, 41, 42, 55),
    34 -> List(22, 46, 63),
    65 -> List(22, 63, 67, 82),
    67 -> List(23, 52, 65, 82, 102),
    55 -> List(29, 89),
    63 -> List(34, 79, 65, 100),
    87 -> List(41, 86, 105),
    72 -> List(42, 105, 107),
    78 -> List(46, 77, 79),
    86 -> List(52, 87, 102, 116),
    89 -> List(55, 105),
    74 -> List(58, 94),
    77 -> List(58, 94, 78, 124),
    79 -> List(63, 78),
    100 -> List(63, 82, 111),
    82 -> List(65, 67, 100, 140),
    102 -> List(67, 86, 127),
    105 -> List(72, 87, 89, 107, 108),
    107 -> List(72, 105, 161),
    94 -> List(74, 77, 93),
    124 -> List(77, 111, 123, 153, 153),
    140 -> List(82, 133, 154, 156),
    116 -> List(86, 108, 127, 142),
    93 -> List(94),
    111 -> List(100, 124),
    127 -> List(102, 116, 133),
    108 -> List(105, 116, 135),
    161 -> List(107, 135, 128, 199),
    135 -> List(108, 128, 161),
    142 -> List(116, 157, 128),
    122 -> List(144, 123),
    144 -> List(122, 123, 163),
    123 -> List(122, 144, 165, 124),
    165 -> List(123, 191, 180),
    153 -> List(124, 124, 154, 180, 184),
    133 -> List(127, 140, 157),
    128 -> List(199, 187, 135, 142, 161),
    199 -> List(128, 161, 185, 187),
    187 -> List(128, 185, 199),
    157 -> List(133, 142, 156, 185),
    154 -> List(140, 153, 156),
    156 -> List(140, 154, 157, 184),
    163 -> List(144, 176, 191),
    180 -> List(153, 165, 184, 190),
    184 -> List(153, 156, 180, 185),
    185 -> List(157, 184, 187, 199),
    176 -> List(163, 190),
    191 -> List(163, 165, 190),
    190 -> List(176, 180, 191)
  )
  val undergroundMap: Map[Int, List[Int]] = Map(
    1 -> List(46),
    46 -> List(1, 13, 74, 79),
    13 -> List(46, 67, 89),
    67 -> List(13, 79, 111, 89),
    89 -> List(13, 67, 140, 128),
    74 -> List(46),
    79 -> List(46, 67, 93, 111),
    111 -> List(67, 79, 153, 163),
    93 -> List(79),
    140 -> List(89, 153, 128),
    128 -> List(89, 185, 140),
    153 -> List(111, 140, 163, 185),
    163 -> List(111, 153),
    185 -> List(128, 153)
  )
  val blackMap: Map[Int, List[Int]] = Map(
    194 -> List(157),
    157 -> List(194, 115),
    115 -> List(157, 108),
    108 -> List(115)
  )
}


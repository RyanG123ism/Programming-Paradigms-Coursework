import scala.io.Source
import scala.io.StdIn.readInt
import scala.io.StdIn.readLine

object CW1_App extends App{

  //file path for data
  val filePath = "data.txt"

  //create and define mapData variable and storing the mapped file data inside
  val mapData: Map[Int, List[(String, Float, Int)]] = readFile(filePath)

  //defining menu options as a map
  //key is the option number
  //value is a function that will return true or false
  val actionMap = Map[Int, () => Boolean](
    1 -> handleOne,
    2 -> handleTwo,
    3 -> handleThree,
    4 -> handleFour,
    5 -> handleFive,
    6 -> handleSix,
    7 -> handleSeven)

  //initiating the menu loop
  menuLoop()

  //tail recursive function - will continue to make a call to itself until 'menu' function is returned as FALSE
  def menuLoop(): Unit = {
    // Display menu options
    val opt = readOption
    if (menu(opt)) {
      menuLoop() // Tail-recursive call
    }
  }

  //print raw data
  //println(mapData)

  //print parsed data     (key, value)
  //TESTING PURPOSES ONLY
  //  mapData.foreach { case (year, drivers) =>
   //   println(s"Year: $year")
   //   drivers.foreach { case (name, points, wins) =>
   //     println(s"  Driver: $name, Points: $points, Wins: $wins")
   //   }
   // }

  //READING FILE AND PARSING DATA**********************************************************************************

  //read the file and extract data into hashMap using the year as the Key
  def readFile(filePath: String): Map[Int, List[(String, Float, Int)]] = {

    //get all lines from the file data
    val lines = Source.fromFile(filePath).getLines()

    //iterate through each line and split the data into the kay (year) and values (drivers)
    lines.map { line =>
      //splits the line into two ('2') parts via the first comma, separating the year and driver data
      val parts = line.split(",", 2)
      val year = parts(0).toInt//convert year to int from string

      //splits driver data into individual driver entries
      //then calls lambda function to parse all driver data
      val drivers = parts(1).split(",").toList.flatMap { driverData =>
        parseDriverData(driverData.trim)
      }
      year -> drivers //creating touple - ex. 2023 -> List((string, float, int), (string, float, int))
    }.toMap //convert to Map
  }

  //parses the read in file data
  def parseDriverData(data: String): Option[(String, Float, Int)] = {
    //regular expression pattern object to match driver data to this format
    val pattern = """([^:]+): ([\d.]+) (\d+)""".r
    data match {
      //if data matches the pattern then converts into string, float, int and returns touple
      case pattern(name, points, wins) =>
        Some((name.trim, points.toFloat, wins.toInt))
        //if match is not found then return None
      case _ =>
        None
    }
  }

  //MENU OPTIONS***************************************************************************************************

  def menu(option: Int): Boolean = {
    actionMap.get(option) match {
      case Some(f) => f()
      case None =>
        println("Sorry, that command is not recognized")
        true
    }
  }

  // shows menu options and reads the users input - INT ONLY
  def readOption: Int = {
    println(
      """|Please select one of the following:
         |  1 - display winner for each season
         |  2 - display a chosen seasons results
         |  3 - display the total races for a chosen season
         |  4 - display seasonal average points
         |  5 - display total points for each season
         |  6 - search driver points by surname
         |  7 - quit""".stripMargin)
    readInt()
  }

  //MENU OPTION HANDLERS*********************************************************************************************

  //display season winners
  def handleOne(): Boolean = {
    displaySeasonWinners()
    true
  }

  //display season results
  def handleTwo(): Boolean = {
    println("Enter a season to display its results:")
    val season = readInt()
    displaySeasonResults(season)
    true
  }

  //display particular seasons total races
  def handleThree(): Boolean = {
    println("Enter a season to display the total races:")
    val season = readInt()
    displayTotalRaces(season)
    true
  }

  //display all seasons average points
  def handleFour(): Boolean = {
    displaySeasonalAveragePoints()
    true
  }

  //display the total points from each season
  def handleFive(): Boolean = {
    displayTotalPointsPerSeason()
    true
  }

  //search drivers stats
  def handleSix(): Boolean = {
    println("Enter driver's name to search for points:")
    val name = readLine().trim
    searchDriverPointsByName(name)
    true
  }

  //quit
  def handleSeven(): Boolean = {
    println("Selected quit") //returns false so loop terminates
    false
  }

  //ACTION FUNCTIONS ********************************************************************************************

  def displaySeasonWinners(): Unit = {
    mapData.foreach { case (year, drivers) =>
      val winner = drivers.maxBy(_._2) //max by the second element in the touple - '(_._2)'

      //print winner
      println(s"$year Winner: ${winner._1}, Points: ${winner._2}, Wins: ${winner._3}")
    }
  }

  def displaySeasonResults(season: Int): Unit = {
    //get relevant key using the season selected by the user
    mapData.get(season) match {
      case Some(drivers) =>
        println(s"Results for season $season:")
        drivers.foreach { case (name, points, wins) =>//loop through drivers in season as print their name, points, and wins
          println(s"Driver: $name, Points: $points, Wins: $wins")
        }
      case None => //if couldnt match season to key in hashmap
        println(s"No data found for season $season.")
    }
  }

  def displayTotalRaces(season: Int): Unit = {
    //get relevant key using the season selected by the user
    mapData.get(season) match {
      case Some(drivers) =>
        val totalRaces = drivers.map(_._3).sum //sums all wins - 3rd element in the touples
        println(s"Total races in season $season: $totalRaces")
      case None => //if couldnt match season to key in hashmap
        println(s"No data found for season $season.")
    }
  }

  def displaySeasonalAveragePoints(): Unit = {
    //loop through each year in hashmap and get the average points
    mapData.foreach { case (year, drivers) =>
      val averagePoints = drivers.map(_._2).sum / drivers.size
      println(f"$year Average Points: $averagePoints%.2f")//display result
    }
  }

  def displayTotalPointsPerSeason(): Unit = {
    //loop through each key in hashmap and total the points
    mapData.foreach { case (year, drivers) =>
      val totalPoints = drivers.map(_._2).sum // Sum of points for all drivers in a season
      println(f"$year Total Points: $totalPoints%.2f")
    }
  }

  def searchDriverPointsByName(name: String): Unit = {

    //flatten the mapData into a list of all drivers
    val allDrivers = mapData.values.flatten.toList

    //convert to lower and filter the first element in the touple by user input
    val matchingDrivers = allDrivers.filter(_._1.toLowerCase.contains(name.toLowerCase))

    //if name is not found
    if (matchingDrivers.isEmpty) {
      println(s"No results found for driver with name: $name")
    } else {
      //take first matching result for driver name
      val actualName = matchingDrivers.head._1

      //sum the total points from the drive
      val totalPoints = matchingDrivers.map(_._2).sum

      //display driver stats
      println(s"Driver: $actualName, Total Points: $totalPoints")
    }
  }
}






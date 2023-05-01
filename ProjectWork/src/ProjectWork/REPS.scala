package ProjectWork

import scala.io.StdIn
import scala.io.Source
import java.time.{LocalDateTime, LocalTime, YearMonth}
import java.time.temporal.WeekFields
import java.util.Locale
import java.time.format.DateTimeFormatter // format current time
import scala.io.AnsiColor._

/* Renewable Energy Plant System control system program */
case object REPS extends App {

  /* function to get current time as formatted string */
  def get_current_time(): String = {
    val current_time: String = LocalTime.now().format(DateTimeFormatter.ofPattern("HH:mm")) // get current time and format
    current_time
  }

  /* function to format the time*/
  def date_formatter(rows: List[List[String]], choice: String) = {
    val dateFormatter = DateTimeFormatter.ofPattern("M/d/yyyy H:mm")

    choice match {
      case "1" => // hourly
        rows.groupBy(row => row(0).trim)
      case "2" | "3" | "4" => // daily, weekly, monthly
        val groupedRows = choice match {
          case "2" => // daily
            rows.groupBy { row =>
              val timestamp = row(0).trim
              val date = LocalDateTime.parse(timestamp, dateFormatter).toLocalDate
              date.format(DateTimeFormatter.ISO_LOCAL_DATE)
            }
          case "3" => // weekly
            rows.groupBy { row =>
              val timestamp = row(0).trim
              val date = LocalDateTime.parse(timestamp, dateFormatter).toLocalDate
              val weekFields = WeekFields.of(Locale.getDefault)
              val weekOfYear = date.get(weekFields.weekOfWeekBasedYear())
              s"${date.getYear}-W$weekOfYear"
            }
          case "4" => // monthly
            rows.groupBy { row =>
              val timestamp = row(0).trim
              val date = LocalDateTime.parse(timestamp, dateFormatter).toLocalDate
              val yearMonth = YearMonth.from(date)
              yearMonth.format(DateTimeFormatter.ofPattern("yyyy-MM"))
            }
        }
        groupedRows
    }
  }


  /* function to read contents of .csv file and returns list of contents */
  def read_file(filename: String): List[List[String]] = {
    val source = Source.fromFile(filename)
    var contents: List[List[String]] = List()
    try {
      val lines = source.getLines().toList.tail // ignore header line
      for (line <- lines) {
        val fields = line.split(",").map(_.trim).toList
        contents = contents :+ fields
      }
      contents // return a list of fields
    } catch {
      case e: Exception => List(List("error", "operation failed."))
    } finally {
      source.close()
    }
  }

  // energy generation mechanism super class
  class EnergyMechanism(val id_num: String, var current_pos: String)

  // solar panel subclass
  class SolarPanel(id: String, pos: String) extends EnergyMechanism(id, pos) {
    override val id_num: String = id
    current_pos = pos
  }

  // wind turbine subclass
  class WindTurbine(id: String, pos: String) extends EnergyMechanism(id, pos) {
    override val id_num: String = id
    current_pos = pos
  }

  // create solar panels
  val SP1_panel: SolarPanel = new SolarPanel("SP1", "South") // solar panel SP1
  val SP2_panel: SolarPanel = new SolarPanel("SP2", "South") // solar panel SP2
  val SP3_panel: SolarPanel = new SolarPanel("SP3", "South") // solar panel SP3

  // create list of solar panels in power plant
  var solar_panel_list: List[SolarPanel] = List(SP1_panel, SP2_panel, SP3_panel)

  // create wind turbines
  val WT1_turbine = new WindTurbine("WT1", "East") // wind turbine WT1
  val WT2_turbine = new WindTurbine("WT2", "East") // wind turbine WT2
  val WT3_turbine = new WindTurbine("WT3", "East") // wind turbine WT3

  // create list of wind turbines in power plant
  var wind_turbine_list: List[WindTurbine] = List(WT1_turbine, WT2_turbine, WT3_turbine)

  // print basic information about REPS to user
  println("Hello and welcome to the Renewable Energy Plant System (REPS)!")
  println("This is multigeneration energy system plant that manages the production of renewable energy for cities and industries.\n" +
    "Through this system interface, you will be able to monitor and control the power plant operations.\n")

  /* main function to control REPS */
  def REPS_controller(): Unit = {
    try {
      println("Please choose an operation:\n" +
        "1) Adjust solar panel and wind turbine positions to optimize energy production\n" +
        "2) Monitor all solar panels and wind turbines\n" +
        "3) Overview of power plant activity\n" +
        "4) Analyze energy production of the power plant\n" +
        "5) View warning signs that may hinder quality of energy production\n" +
        "0) Shut down system")

      // trait to adjust solar panel and wind turbine positions
      trait Adjustable[-Type] {
        def adjust[A <: {def id_num: String; var current_pos: String}](obj_list: List[A], new_pos: String, current_time: String): Unit = {
          // reassign position of each object in category
          val new_obj_list = obj_list.map(obj => {
            obj.current_pos = new_pos
            obj
          })
          println(s"The solar panels are being rotated towards '$new_pos' at ${current_time}.\n\n")
        }
      }
      // objects to adjust solar panels and wind turbines
      object SolarAdjustable extends Adjustable[SolarPanel]
      object WindAdjustable extends Adjustable[WindTurbine]

      /* find current direction of sun according to time of day */
      def solar_direction(current_time: LocalTime): String = {
        if (current_time.isAfter(LocalTime.parse("06:00:00")) && current_time.isBefore(LocalTime.parse("09:00:00"))) {
          val morning_pos: String = "East"
          morning_pos
        } else if (current_time.isAfter(LocalTime.parse("09:00:00")) && current_time.isBefore(LocalTime.parse("12:00:00"))) {
          val noon_pos: String = "Southeast"
          noon_pos
        } else if (current_time.isAfter(LocalTime.parse("12:00:00")) && current_time.isBefore(LocalTime.parse("15:00:00"))) {
          val afternoon_pos: String = "South"
          afternoon_pos
        } else if (current_time.isAfter(LocalTime.parse("15:00:00")) && current_time.isBefore(LocalTime.parse("18:00:00"))) {
          val noon_pos: String = "Southwest"
          noon_pos
        } else {
          val night_pos = "West"
          night_pos
        }
      }

      /* get direction of user choice */
      def get_direction(user_input: String): String = {
        // pattern matching for user choice
        val direction = user_input match {
          case "1" => "North"
          case "2" => "Northeast"
          case "3" => "East"
          case "4" => "Southeast"
          case "5" => "South"
          case "6" => "Southwest"
          case "7" => "West"
          case "8" => "Northwest"
          case _ => "Incorrect choice, please try again..."
        }
        direction
      }

      /* function to control solar panel and wind turbine position */
      def control_mechanism_pos(): Unit = {
        println("Please state the mechanism that you would like to control:\n1) Solar panel\n2) Wind turbine")
        val user_choice: String = StdIn.readLine() // get user input

        // pattern matching to get operation of user's choice
        val operation = user_choice match {
          case "1" => { // adjust solar panels
            println("The position of the solar panels will be adjusted according to the direction of the sunlight at this time of day in Southern Finland.")
            val new_pos: String = solar_direction(LocalTime.now()) // find direction of sun
            SolarAdjustable.adjust(solar_panel_list, new_pos, get_current_time()) // adjust solar panel position
          }
          case "2" => { // adjust wind turbines
            println("Which direction would you like to rotate the wind turbine in:\n" +
              "1) North\n2) Northeast\n3) East\n4) Southeast\n5) South\n6) Southwest\n7) West\n8) Northwest")
            val user_input: String = StdIn.readLine()
            val new_pos = get_direction(user_input) // get direction of wind based on user input
            WindAdjustable.adjust(wind_turbine_list, new_pos, get_current_time()) // adjust wind turbine position
            REPS_controller()
          }
          case _ => println("This case does not exist.")
        }
      }

      // trait to print current wind turbine and sun panel positions
      trait Monitorable[-Type] {
        /* function to print current positions */
        def monitor[A <: {def id_num: String; def current_pos: String}](obj_list: List[A], obj_type: String): Unit = {
          println(s"${obj_type}s and their current positions:")
          obj_list.foreach(obj => println(s"$obj_type ${obj.id_num} is facing ${obj.current_pos}"))
        }
      }
      // objects for solar panel and wind turbine position monitoring
      object SolarMonitorable extends Monitorable[SolarPanel]
      object WindMonitorable extends Monitorable[WindTurbine]

      /* monitor all sun panel and wind turbine positions */
      def monitor_solar_wind(): Unit = {
        SolarMonitorable.monitor(solar_panel_list, "Solar panel") // print solar panel information
        WindMonitorable.monitor(wind_turbine_list, "Wind turbine") // print wind turbine information
        println("\n")
        REPS_controller()
      }

      // prints the data in the file in a cooler way.
      def printTable(rows: List[List[String]], header: Option[List[String]] = None) = {
        // calculate the maximum width of each column
        val allRows = header.map(h => h :: rows).getOrElse(rows)
        val colWidths = allRows.transpose.map(col => col.map(cell => cell.length).max)
        // create a format string for each row
        val formatStr = colWidths.map(width => s"%-${width}s").mkString(" | ")
        // print the header if provided
        header.foreach { h =>
          println(formatStr.format(h: _*))
          println(colWidths.map("-" * _).mkString("-+-"))
        }
        // print the table rows
        for (row <- rows) {
          println(formatStr.format(row: _*))
        }
      }


      /* overview of power plant activity */
      def overview_power_plant(): Unit = {
        println("Choose an option:\n1) View total production\n2) View employee activity\n3) View vehicle activity\n4) View number of goods by category")
        val user_choice: String = StdIn.readLine() // get user input

        // pattern matching to get operation of user's choice
        val view_operation = user_choice match {
          case "1" => { // view total production
            val rows = read_file("DMMProjectData/ProjectData/HourlyProduction.csv")
            println("Total Production:")
            val header = List("Time", "SP1", "SP2", "SP3", "WT1", "WT2", "WT3", "HP")
            printTable(rows, Some(header))
            REPS_controller()
          }
          case "2" => { // view employee activity
            val rows = read_file("DMMProjectData/ProjectData/NumberOfEmployees.csv")
            println("Employee Activity:")
            val header = List("Timestamp", "Number of Employees")
            printTable(rows, Some(header))
            REPS_controller()
          }
          case "3" => { // view vehicle activity
            val rows = read_file("DMMProjectData/ProjectData/NumberOfVehicles.csv")
            println("Vehicle Activity:")
            val header = List("Timestamp", "Number of Employee Cars", "Number of Delivery Trucks")
            printTable(rows, Some(header))
            REPS_controller()
          }
          case "4" => { // view number of goods arrived
            val rows = read_file("DMMProjectData/ProjectData/Number(Category)OfGoods.csv")
            println("Number of Goods:")
            val header = List("Date","Goods Category","Item","Quantity")
            printTable(rows, Some(header))
            REPS_controller()
          }
          case _ => println("This case does not exist.")
        }
      }

      /* data analyze statistics */
      def statistics(rows: List[List[String]]): List[List[String]] = {
        val numericData = rows.flatMap(row => row.drop(1).map(_.toDouble)) // data to numeric values
        val mean = numericData.sum / numericData.length // calculate the mean

        // calculate the median
        val sortedData = numericData.sorted
        val median = if (sortedData.length % 2 == 0) {
          (sortedData(sortedData.length / 2 - 1) + sortedData(sortedData.length / 2)) / 2
        } else {
          sortedData(sortedData.length / 2)
        }

        // calculate the mode
        val dataCounts = numericData.groupBy(identity).mapValues(_.size)
        val mode = dataCounts.maxBy(_._2)._1

        val range = sortedData.last - sortedData.head // calculate the range
        val midrange = (sortedData.head + sortedData.last) / 2 // calculate the midrange

        val data = List(
          List("Mean", mean.round.toString),
          List("Median", median.round.toString),
          List("Mode", mode.round.toString),
          List("Range", range.round.toString),
          List("Midrange", midrange.round.toString)
        )
        data
      }

      /* overview of power plant activity */
      def analyse_power_plant(): Unit = {
        println("Choose an option:\n1) Analyse total production\n2) Filter production\n3) Sort production\n4) Search for a specific mechanism and its consumption")
        val user_choice: String = StdIn.readLine() // get user input

        // pattern matching to get operation of user's choice
        val analyse_operation = user_choice match {
          case "1" => { // data analysis on production
            val rows = read_file("DMMProjectData/ProjectData/HourlyProduction.csv")
            println("Data Analysis on Total Production of the REPS:")
            val data = statistics(rows)
            printTable(data)
            REPS_controller()
          }
          case "2" => { // filter total consumption
            val rows = read_file("DMMProjectData/ProjectData/HourlyProduction.csv")
            println("Choose an option:\n1) Filter total consumption by hourly\n2) Filter total consumption by daily.\n3) Filter total consumption by weekly.\n4) Filter total consumption by monthly.")
            val user_choice_for_filter: String = StdIn.readLine() // get user input
            val groupedRows = date_formatter(rows, user_choice_for_filter)
            val dateFormatter = DateTimeFormatter.ofPattern("M/d/yyyy H:mm")

            // group by time period
            val sortedGroupedRows = groupedRows.map {
              case (timePeriod, rowsInPeriod) =>
                val sortedRowsInPeriod = rowsInPeriod.sortWith((a, b) => {
                  val dateA = LocalDateTime.parse(a(0).trim, dateFormatter)
                  val dateB = LocalDateTime.parse(b(0).trim, dateFormatter)
                  dateA.isBefore(dateB)
                })
                (timePeriod, sortedRowsInPeriod)
            }

            // sort the grouped time period
            val filteredRows = sortedGroupedRows.map {
              case (timePeriod, rowsInPeriod) =>
                val totalConsumption = rowsInPeriod.map(row => row.slice(1, 8).map(_.toDouble).sum).sum
                List(timePeriod, totalConsumption.toString)
            }.toList.sortBy(_(0))

            printTable(filteredRows)
            REPS_controller()
          }
          case "3" => { // sort consumption period
            val rows = read_file("DMMProjectData/ProjectData/HourlyProduction.csv")
            println("Sort the consumption period by:\n1) Ascending\n2) Descending\n")
            val user_choice_for_sort: String = StdIn.readLine() // get user input
            user_choice_for_sort match {
              case "1" => { // sort ascending
                val dateFormatter = DateTimeFormatter.ofPattern("M/d/yyyy H:mm")
                implicit val localDateTimeOrdering: Ordering[LocalDateTime] = Ordering.fromLessThan(_ isBefore _)
                val sortedRows = rows.sortBy(row => LocalDateTime.parse(row(0).trim, dateFormatter))
                val header = List("Time", "SP1", "SP2", "SP3", "WT1", "WT2", "WT3", "HP")
                printTable(sortedRows, Some(header))
              }
              case "2" => { // sort descending
                val dateFormatter = DateTimeFormatter.ofPattern("M/d/yyyy H:mm")
                implicit val localDateTimeOrdering: Ordering[LocalDateTime] = Ordering.fromLessThan(_ isAfter _)
                val sortedRows = rows.sortBy(row => LocalDateTime.parse(row(0).trim, dateFormatter))
                val header = List("Time", "SP1", "SP2", "SP3", "WT1", "WT2", "WT3", "HP")
                printTable(sortedRows, Some(header))
              }
              case _ => throw new IllegalArgumentException("Invalid option provided. Only values 1, 2, 3, and 4 are allowed.")
            }
            REPS_controller()
          }

          case "4" => { // search mechanism and its total consumption
            val rows = read_file("DMMProjectData/ProjectData/HourlyProduction.csv")
            val header = List("Time", "SP1", "SP2", "SP3", "WT1", "WT2", "WT3", "HP")
            println("Give a mechanism id:")
            val user_choice_for_mechanism: String = StdIn.readLine() // get user input
            val mechanismIndex = header.indexOf(user_choice_for_mechanism) // get the index of the chosen mechanism
            if (mechanismIndex >= 0) {
              val totalConsumption = rows.map(row => row(mechanismIndex).toDouble).sum // calculate the total consumption for the chosen mechanism
              println(s"Total consumption (HourlyProduction log file starting from 1 April 2023) for $user_choice_for_mechanism: $totalConsumption")
            } else {
              println("Invalid mechanism id")
            }
            REPS_controller()
          }
          case _ => println("This case does not exist.")
        }
      }

      /* detect any low energy production from a mechanism */
      def detect_warning(): Unit = {
        println("Processing last year's log file...")
        val dataRows = read_file("DMMProjectData/ProjectData/MonthlyProductionAmountForEachMechanism_2022.csv")
        val headerRow = Array("Month", "SP1", "SP2", "SP3", "WT1", "WT2", "WT3", "HP")
        println("Processing each mechanism's total consumption amount from the log file...")

        // set a threshold for low energy production
        val threshold = 400000.0
        println("Detecting any unusual activity...")

        // check for low energy production for each mechanism and store the results in 'warnings'
        val warnings = dataRows.flatMap { row =>
          val month = row(0)
          row.zipWithIndex.tail.collect {
            case (productionStr, idx) if productionStr.toDouble < threshold =>
              (idx, month)
          }
        }.groupBy(_._1).view.map { case (key, values) => key -> values.map(_._2) }.toMap

        println("Processing necessary warnings...")
        println(s"${RED}Here are some warnings according to last year's monthly log file:${RESET}\n")

        // print the warnings for mechanisms with low energy production in at least three different months
        if (warnings.nonEmpty) {
          warnings.foreach { case (mechanismIdx, months) =>
            if (months.length >= 3) {
              val mechanismName = headerRow(mechanismIdx)
              println(s"${BOLD}${RED}WARNING!$RESET $mechanismName might have equipment malfunction or needs to be maintained as it has low energy production (<400 000kWh) in the following months: ${months.mkString(", ")}.\n")
            }
          }
        } else {
          println("No warnings detected.")
        }
      }


      /* asks user to perform action */
      val user_input: String = StdIn.readLine() // get user input
      val entered_choice = user_input match {
        case "1" => control_mechanism_pos()
        case "2" => monitor_solar_wind()
        case "3" => overview_power_plant()
        case "4" => analyse_power_plant()
        case "5" => detect_warning()
        case "0" => {
          println("Shutting down...")
          sys.exit() // exit program
        }
      }
      REPS_controller() // recursive function call to main function to keep program running
    } catch { case e: Exception => println("Operation failed. The system shut down automatically.") }  // catch  exception
  }
  REPS_controller()
}

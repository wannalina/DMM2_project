package ProjectWork

import scala.io.StdIn
import scala.io.Source
import java.time.LocalTime  // library to get current time
import java.time.format.DateTimeFormatter // format current time

/* Renewable Energy Plant System control system program */
case object REPS extends App {

  /* function to read contents of .csv file and returns list of contents */
  def read_file(filename: String): Map[String, String] = {
    val source = Source.fromFile(filename)
    var contents: Map[String, String] = Map()
    try {
      source.getLines.drop(0) // ignore header line
      // loops acceptable to read file as per teacher's instructions
      for (line <- source.getLines()) {
        val fields = line.split(",") // split lines
        val key = fields.head
        val tail_vals = fields.tail
        val value: String = tail_vals.mkString(",") // convert tail from serializable to string
        contents = contents + (key -> value)
      }
      contents // return map of file contents
    } catch { case e: Exception => Map("error" -> "operation failed.") }  // catch  exception
    finally {
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
    println("Please choose an operation:\n" +
      "1) Adjust solar panel and wind turbine positions to optimize energy production\n" +
      "2) Monitor all solar panels and wind turbines\n" +
      "3) Overview of power plant activity\n" +
      "4) Analyze energy production of the power plant\n" +
      "5) View warning signs that may hinder quality of energy production\n" +
      "0) Shut down system")

    trait Adjustable[-Type] {
      def adjust[A <: {def id_num: String; var current_pos: String}](obj_list: List[A], new_pos: String, current_time: String): Unit = {
        // rotate each object in category
        val new_obj_list = obj_list.map(obj => {
          obj.current_pos = new_pos
          obj
        })
        println(s"The solar panels are being rotated towards '$new_pos' at ${current_time}.\n\n")
      }
    }
    object SolarAdjustable extends Adjustable[SolarPanel]
    object WindAdjustable extends Adjustable[WindTurbine]

    /* find current direction of sun */
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
        val operation = user_choice match {
          case "1" => {
            println("The position of the solar panels will be adjusted according to the direction of the sunlight at this time of day in Southern Finland.")
            val current_time: String = LocalTime.now().format(DateTimeFormatter.ofPattern("HH:mm")) // get current time and format
            val new_pos: String = solar_direction(LocalTime.now()) // find direction of sun
            SolarAdjustable.adjust(solar_panel_list, new_pos, current_time)
          }
          case "2" => {
            println("Which direction would you like to rotate the wind turbine in:\n" +
              "1) North\n2) Northeast\n3) East\n4) Southeast\n5) South\n6) Southwest\n7) West\n8) Northwest")
            val user_input: String = StdIn.readLine() // get user input
            val new_pos = get_direction(user_input) // get direction of wind based on user input
            val current_time: String = LocalTime.now().format(DateTimeFormatter.ofPattern("HH:mm")) // get current time and format
            WindAdjustable.adjust(wind_turbine_list, new_pos, current_time)
            REPS_controller() // recursive function call to main function to keep program running
          }
          case _ => println("This case does not exist.")
        }
      }

      trait Monitorable[-Type] {
        def monitor[A <: {def id_num: String; def current_pos: String}](obj_list: List[A], obj_type: String): Unit = {
          println(s"${obj_type}s and their current positions:")
          obj_list.foreach(obj => println(s"$obj_type ${obj.id_num} is facing ${obj.current_pos}"))
        }
      }
      object SolarMonitorable extends Monitorable[SolarPanel]
      object WindMonitorable extends Monitorable[WindTurbine]

      /* monitor all sun panel and wind turbine positions */
      def monitor_solar_wind(): Unit = {
        SolarMonitorable.monitor(solar_panel_list, "Solar panel")
        WindMonitorable.monitor(wind_turbine_list, "Wind turbine")
        println("\n")

        REPS_controller() // recursive function call to main function to keep program running
      }

    /* asks user to perform action */
    val user_input: String = StdIn.readLine() // get user input
    val entered_choice = user_input match {
      case "1" => control_mechanism_pos()
      case "2" => monitor_solar_wind()
      case "3" => "FUNCTION CALL FOR OVERVIEW INFO HERE"
      case "4" => "FUNCTION CALL FOR ENERGY PRODUCTION ANALYSIS HERE"
      case "5" => "FUNCTION CALL FOR WARNING SIGNS HERE"
      case "0" => {
        println("Shutting down...")
        sys.exit()
      }
    }
    REPS_controller()
  }
  REPS_controller()
}

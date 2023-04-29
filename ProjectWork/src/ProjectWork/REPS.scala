package ProjectWork

import scala.io.StdIn
import java.time.LocalTime
import java.time.format.DateTimeFormatter  // library to get current time

/** METHOD OF COMMENTING USED SO FAR:
 * 1) /**/ COMMENTS USED TO EXPLAIN FUNCTION PURPOSES ETC.
 * 2) // COMMENTS USED TO EXPLAIN OTHER SMALL REMARKS IN CODE
 */


/* Renewable Energy Plant System control system program */
object REPS extends App {
  // energy generation mechanism super class
  class EnergyMechanism(id: String, pos: String) {
    val id_num: String = ""
    var current_pos: String = ""

    def change_position(new_pos: String): Unit = {
      ""
    }
  }

  // solar panel subclass
  class SolarPanel(id: String, pos: String) extends EnergyMechanism(id, pos) {
    override val id_num: String = id
    current_pos = pos

    override def change_position(new_pos: String): Unit = {
      current_pos = new_pos
    }
  }

  // wind turbine subclass
  class WindTurbine(id: String, pos: String) extends EnergyMechanism(id, pos) {
    override val id_num: String = id
    current_pos = pos

    override def change_position(new_pos: String): Unit = {
      current_pos = new_pos
    }
  }

  // list of possible positions for solar panels and wind turbines
  val energy_mechanism_positions: List[String] = List("North", "Northeast", "East", "Southeast", "South", "Southwest", "West", "Northwest")

  // create solar panels
  val SP1_panel: SolarPanel = new SolarPanel("SP1", "South") // solar panel SP1
  val SP2_panel: SolarPanel = new SolarPanel("SP2", "South") // solar panel SP2
  val SP3_panel: SolarPanel = new SolarPanel("SP3", "South") // solar panel SP3

  // create wind turbines
  val WT1_turbine = new WindTurbine("WT1", "East") // wind turbine WT1
  val WT2_turbine = new WindTurbine("WT2", "East") // wind turbine WT2
  val WT3_turbine = new WindTurbine("WT3", "East") // wind turbine WT3

  // print basic information about REPS to user
  println("Hello and welcome to the Renewable Energy Plant System (REPS)!")
  println("This is multigeneration energy system plant that manages the production of renewable energy for cities and industries.\n" +
    "Through this system interface, you will be able to monitor and control the power plant operations.")

  /* main program begins here */
  def REPS_controller() {

    println("Please choose an operation:\n" +
      "1) Adjust solar panel and wind turbine positions to optimize energy production\n" +
      "2) Monitor all solar panels and wind turbines\n" +
      "3) Overview of power plant activity\n" +
      "4) Analyze energy production of the power plant\n" +
      "5) View warning signs that may hinder quality of energy production\n" +
      "0) Shut down system")

    // create list of solar panels in power plant
    val solar_panel_list: List[SolarPanel] = List(SP1_panel, SP2_panel, SP3_panel)
    // create list of wind turbines in power plant
    val wind_turbine_list: List[WindTurbine] = List(WT1_turbine, WT2_turbine, WT3_turbine)

    /* adjustable trait for solar panels and wind turbines */
    trait Findable[-E] {
      // find index from solar panel / wind turbine list
      def obj_index(id: String): Int = {
        val solarPanelIndex = solar_panel_list.indexWhere(_.id_num == id)
        if (solarPanelIndex != -1) solarPanelIndex
        else {
          val windTurbineIndex = wind_turbine_list.indexWhere(_.id_num == id)
          if (windTurbineIndex != -1) windTurbineIndex
          else -1
        }
      }
      /* function to adjust solar panel and wind turbine positions */
      def adjust(obj_id: String, list_name: List[EnergyMechanism], new_pos: String): String = {
        val index: Int = obj_index(obj_id) // get index of object from list
        if (index == -1) "This object does not exist."
        else {
          var obj: EnergyMechanism = list_name(index) // get object
          val id: String = obj.id_num // get id number of object
          val current_pos: String = obj.current_pos // get current position of object

          println(s"You have chosen to adjust the position of solar panel '$id' currently facing '$current_pos'.")

          obj.change_position(new_pos) // change position of object

          val current_time: String = LocalTime.now().format(DateTimeFormatter.ofPattern("HH:mm")) // get current time and format
          val message: String = s"The new position of object '$id' is now facing '$new_pos' at $current_time.\n\n"
          message
        }
      }
    }
    /* creates object for adjusting solar panels and wind turbines */
    object SolarFindable extends Findable[SolarPanel]
    object WindFindable extends Findable[WindTurbine]

    /* function to control solar panel and wind turbine position */
    def control_mechanism_pos(): Unit = {
      println("Please state the mechanism that you would like to control:\n1) Solar panel\n2) Wind turbine")
      val user_choice: String = StdIn.readLine() // get user input

      if (user_choice == "1") {
        println("Please state the solar panel to adjust:\n1) SP1\n2) SP2\n3) SP3")
        val user_choice_2: String = StdIn.readLine() // get user input

        // pattern matching for user choice
        val mechanism = user_choice_2 match {
          case "1" => adjust_solar("SP1")
          case "2" => adjust_solar("SP2")
          case "3" => adjust_solar("SP3")
          case _ => "Incorrect choice, please try again..."
        }
      }
      else if (user_choice == "2") {
        println("Please state the wind turbine to adjust:\n1) WT1\n2) WT2\n3) WT3")
        val user_choice_2: String = StdIn.readLine() // get user input

        // pattern matching for user choice
        val mechanism = user_choice_2 match {
          case "1" => adjust_wind("WT1")
          case "2" => adjust_wind("WT2")
          case "3" => adjust_wind("WT3")
          case _ => "Incorrect choice, please try again..."
        }
      }
      else "This choice does not exist, please try again..."
    }

    /* function to get new position for solar panel */
    def adjust_solar(panel_id: String): Unit = {
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
      println("The position of the solar panel will be adjusted according to the direction of the sunlight at this time of day in Southern Finland.\n")
      val new_pos: String = solar_direction(LocalTime.now()) // function call to find direction of sun using current time
      //val panel: SolarPanel = solar_panel_list(index)
      //println(change_position(new_pos)) // function call to adjust solar panel position
      REPS_controller() // recursive function call to main function to keep program running
    }

    /* function to get new position for wind turbine */
    def adjust_wind(turbine_id: String): Unit = {
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
      println("Which direction would you like to rotate the wind turbine in:\n" +
        "1) North\n2) Northeast\n3) East\n4) Southeast\n5) South\n6) Southwest\n7) West\n8) Northwest")
      val user_input: String = StdIn.readLine() // get user input
      val new_pos = get_direction(user_input) // get direction of wind based on user input
     // println(WindAdjustable.adjust(turbine_id, wind_turbine_list, new_pos)) // function call to adjust wind turbine position
      REPS_controller() // recursive function call to main function to keep program running
    }

    /* monitor all sun panels and wind turbines */
    def see_panels_and_turbines(): Unit = {
      def get_list_items(list_name: List[EnergyMechanism]): Unit = {
        val index: Int = 0
        def recursive_func(list_name: List[EnergyMechanism], index: Int): Unit = {
          if (index == list_name.length) ""
          else {
            val obj: EnergyMechanism = list_name(index)
            val obj_id: String = obj.id_num
            val obj_pos: String = obj.current_pos
            println(s"$obj_id: currently facing $obj_pos")
            recursive_func(list_name, index + 1)
          }
        }
        recursive_func(list_name, index)
      }
      println("Solar panels and their current positions:")
      get_list_items(solar_panel_list)
      println("Wind turbines and their current positions:")
      get_list_items(wind_turbine_list)
      println("\n\n")

      REPS_controller() // recursive function call to main function to keep program running
    }

    /* asks user to perform action */
    val user_input: String = StdIn.readLine() // get user input
    val entered_choice = user_input match {
      case "1" => control_mechanism_pos()
      case "2" => see_panels_and_turbines()
      case "3" => "FUNCTION CALL FOR OVERVIEW INFO HERE"
      case "4" => "FUNCTION CALL FOR ENERGY PRODUCTION ANALYSIS HERE"
      case "5" => "FUNCTION CALL FOR WARNING SIGNS HERE"
      case "0" => sys.exit()
    }
  }
  REPS_controller() // function call to main function to start program execution
}
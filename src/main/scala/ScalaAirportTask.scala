/**
 * Created by Illya on 20.07.2015.
 */

import java.io.{FileWriter, BufferedWriter, File}
import scala.collection.mutable.ListBuffer
import scala.io.Source

object ScalaAirportTask {
  val sourceFileName: String = "palnes_log.csv"
  var planesToAirportFilePath: String = "Planes arrived to airports.csv"

  case class FlightRow(year: Int, quater: Int, month: Int, dayOfMonth: Int, dayOfWeek: Int, flDate: String, origin: String, dest: String)

  def main(args: Array[String]) {
    val flightList = readFileAsFlightRowList(sourceFileName)
    val planesArrivedByAirportMap = collection.mutable.LinkedHashMap(getPlanesAmoutArrivedByAirportMap(flightList).toSeq.sortBy(_._1): _*)
    writePlanesByAirportToFile(planesArrivedByAirportMap)
  }


  def readFileAsFlightRowList(fileName: String): List[FlightRow] = {
    var flightRowBuffer = new ListBuffer[FlightRow]
    for (line <- Source.fromFile(fileName).getLines().drop(1)) {
      val Array(year, quarter, month, dayOfMonth, dayOfWeek, flDate, origin, dest) = line.split(",").map(_.trim)
      val flightRow = FlightRow(year.toInt, quarter.toInt, month.toInt, dayOfMonth.toInt, dayOfWeek.toInt, flDate, origin, dest)
      flightRowBuffer += flightRow
    }
    return flightRowBuffer.toList
  }


  def getPlanesAmoutArrivedByAirportMap(flightRowsList: List[FlightRow]): collection.mutable.Map[String, Int] = {
    val plainsByAirport = collection.mutable.Map[String, Int]()

    for (flight <- flightRowsList) {
      if (plainsByAirport.keySet.contains(flight.dest)) {
        val plainsAmount: Int = plainsByAirport.getOrElse(flight.dest, 0) + 1
        plainsByAirport.update(flight.dest,plainsAmount)
      }
      else {
        plainsByAirport.update(flight.dest, 1)
      }
    }
    return plainsByAirport
  }

  def writePlanesByAirportToFile(planesToAirportMap: collection.mutable.LinkedHashMap[String, Int]) = {
    val file = new File(planesToAirportFilePath)
    if (file.exists())
      file.delete()
    val bw = new BufferedWriter(new FileWriter(file, true))
    bw.write("Airport;Planes Arrived;\n")
    for ((k, v) <- planesToAirportMap) {
      bw.write(k + ";" + v + ";\n")
    }
    bw.close()
  }

}

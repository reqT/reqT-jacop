package reqt

import java.util.{GregorianCalendar, Calendar}

case class Date(year: Int, month: Int, dayOfMonth: Int = 1) 
    extends Ordered[Date]: 

  def compare(that: Date): Int = 
    import math.Ordered.orderingToOrdered
    (year, month, dayOfMonth) compare (that.year, that.month, that.dayOfMonth)
  
  val calendar = new GregorianCalendar(year, month - 1, dayOfMonth) 
  
  lazy val shortDate = s"$dayOfMonth/$month"
  lazy val longDate = f"$year%04d-$month%02d-$dayOfMonth%02d"
  def show = longDate
  
  lazy val calendarTime = calendar.getTime 
  
  def addDays(days: Int): Date = {
    val cal =  new GregorianCalendar(year, month - 1, dayOfMonth) 
    cal.add(Calendar.DAY_OF_YEAR, days)
    Date(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH) + 1, cal.get(Calendar.DAY_OF_MONTH))
  }
  
  lazy val workWeek = shortDate + "-" + addDays(4).shortDate

  lazy val weekDay: String = Date.dayOfWeekNumberToName(
    calendar.get(java.util.Calendar.DAY_OF_WEEK)
  ) 

  lazy val monthName: String = Date.monthName(month - 1)

object Date:

  def apply(s: String): Date = 
    val xs = s.split('-')
    require(xs.length == 3 && xs(0).length == 4 && 
      xs.map(_.toIntOption).forall(_.isDefined), 
      s"Illegal date YYYY-MM-DD: $s")
    new Date(xs(0).toInt, xs(1).toInt, xs(2).toInt)

  val dayOfWeekNumberToName: Map[Int, String] = Map(
    1 -> "sön",
    2 -> "mån",
    3 -> "tis",
    4 -> "ons",
    5 -> "tor",
    6 -> "fre",
    7 -> "lör",
  )
  val weekDays: Map[String, Int] = Map(
    "mån" -> 0,
    "tis" -> 1,
    "ons" -> 2,
    "tor" -> 3,
    "fre" -> 4,
  )
  val monthName = Vector("jan", "feb", "mar", "apr", "maj", "jun", "jul", "aug", "sep", "okt", "nov", "dec")
  def javaDate = new java.util.Date
  def javaCalendar = Calendar.getInstance
  def yearNow: Int = javaCalendar.get(Calendar.YEAR)
  def monthNowZeroBased: Int = javaCalendar.get(Calendar.MONTH)
  def monthNameNow: String = monthName(monthNowZeroBased)
  def dayNow: Int = javaCalendar.get(Calendar.DAY_OF_MONTH)
  def now = new Date(yearNow, monthNowZeroBased + 1, dayNow)
  def hourNow = javaCalendar.get(Calendar.HOUR_OF_DAY)
  def minNow = javaCalendar.get(Calendar.MINUTE)
  def secNow = javaCalendar.get(Calendar.SECOND)
  def millisNow = javaCalendar.get(Calendar.MILLISECOND)
  def timeNow = (hourNow, minNow, secNow, millisNow)
  def showDateTimeNow = 
    s"${now.longDate}@${timeNow.toArray.zip(Seq("h","m","s","ms")).map(_.toArray.mkString).mkString}"

  def at(start: Date, week: Int = 1, weekDay: String = "mån") = 
    start.addDays((week - 1)*7 + weekDays(weekDay))

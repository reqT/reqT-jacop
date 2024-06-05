package reqt

import csp.*
import tabby.*
import scala.collection.immutable.ArraySeq


class TestBigProblem extends munit.FunSuite:
  import TestBigProblem.*
  test("hello allocate"):
    println("slotGrid.headings" + slotGrid.headings)
    println("canGrid.headings" + canGrid.headings)

    val aj = new AllocatorJacop(slotGrid)
    
    println(s"\n  allGroups = ${aj.allGroups}")
    println(s"\n  Tutor.all.zipWithIndex = ${Tutor.all.zipWithIndex}")
    println(s"\n  aj.allocatedVars.size = " + aj.allocatedVars.size)
    println(s"\n  aj.allocatedVarsOfTutor(0).size = " + aj.allocatedVarsOfTutor(0).size)
    println(s"\n  aj.slots.size = " + aj.slots.size)

    println(s"\n  aj.allocatedConstr.size = " + aj.allocatedConstr.size)
    println(s"  aj.canConstr.size = " + aj.canConstr.size)
    println(s"  aj.canConstr.take(10) = " + aj.canConstr.take(10))
    // println(s"  aj.sumSlotsPerTutor.size = " + aj.sumSlotsPerTutor.size)
    // println(s"  aj.boundsPerTutor.size = " + aj.boundsPerTutor.size)
    // println(s"  aj.sumTutorsPerSlot.size = " + aj.sumTutorsPerSlot.size)
    // println(s"  aj.lowerBoundPerSlot.size = " + aj.lowerBoundPerSlot.size)
    // println(s"  aj.upperBoundPerSlot.size = " + aj.upperBoundPerSlot.size)
    // println(s"  aj.totalAlloc.size = " + aj.totalAlloc.size)

    //    println(s"\n  aj.allConstraints.size = " + aj.allConstraints.size)
    // println(s"\n  aj.result.conclusion = ${aj.result.conclusion}")
    // println(s"\n  aj.allocatedVarsOfTutor(0) = " + aj.allocatedVarsOfTutor(0))
    // println(s"aj.last = ${aj.last}")
    // println(s"aj.last.toSeq.sortBy(_._2) = ${aj.last.toSeq.sortBy(_._2)}")
    // println(s"aj.last.size = ${aj.last.size}")

    // println(aj.result)

object TestBigProblem:
  class AllocatorJacop(schema: Grid):
    import AllocatorJacop.*

    val slots: ArraySeq[Slot] = 
      schema.mapRows: rm => 
        val rooms = rm("lokal").split(",").toSeq
        val groups = rm("grupp").split(",").toSeq
        val date = Date(rm("datum"))
        val hrs = rm("start").takeWhile(_.isDigit).toInt
        val nbrAmb = PreAddAmbulances((date.weekDay, hrs.toString, rm("typ"))) 
        val ambRooms = Seq.tabulate(nbrAmb)(i => s"Ambulans${i + 1}") 
        val ambGroups = Seq.tabulate(nbrAmb)(i => s"XA${i + 1}") 

        val r: String = 
          val rooms = rm("lokal").split(",").map(_.stripPrefix("E:")).mkString(",") 
          val ambs = if ambRooms.nonEmpty then "," + ambRooms.mkString(",") else ""
          rooms + ambs
        
        val g: String = rm("grupp") + (if ambGroups.nonEmpty then "," + ambGroups.mkString(",") else "")
        
        Slot(date=date, hrs=hrs, course=rm("kurs"), tpe=Type.valueOf(rm("typ")), roomStr=r, groupStr=g)
      .to(ArraySeq)

    val T: Int = Tutor.all.length // number of tutors
    val S: Int = slots.length     // number of slots
    val allGroups: ArraySeq[String] = slots.flatMap(_.groups).distinct.sorted
    val G = allGroups.length // number of groups
    
    val allocatedVars: Seq[Var] = for s <- slots.indices; t <- Tutor.all.indices yield Var(s"s$s,t$t")
    val allocatedVarsOfTutor: Seq[Seq[Var]] = Tutor.all.indices.map(t => allocatedVars.filter(v => v.id.toString.endsWith(s"t$t"))).toSeq
    val allocatedVarsOfSlot: Seq[Seq[Var]] = slots.indices.map(s => allocatedVars.filter(v => v.id.toString.startsWith(s"s$s,"))).toSeq

    val allocatedConstr: Seq[Constr] =  for s <- slots.indices; t <- Tutor.all.indices yield Var(s"s$s,t$t").in(0 to 1)
    
    val canConstr: Seq[Constr] = 
      val xss = for s <- slots.indices; t <- Tutor.all.indices yield
        val sl = slots(s)
        val tut = Tutor.all(t)
        val c = Tutor.can(init=tut, date=sl.date, hrs=sl.hrs, tpe=sl.tpe)
        println(s"sl=$sl tut=$tut Tutor.can(init=$tut, date=${sl.date}, hrs=${sl.hrs}, tpe=${sl.tpe})=$c")
        if !c then Seq[XeqC](Var(s"s$s,t$t") === 0) else Seq[XeqC]() 
      xss.flatten
      
    val sumSlotsPerTutor: Seq[Constr] = for t <- Tutor.all.indices yield SumEq(allocatedVarsOfTutor(t),Var(s"t$t"))
    val boundsPerTutor: Seq[Constr] = for t <- Tutor.all.indices yield Var(s"t$t").in(10 to 30)

    val totalAlloc: Seq[Constr] = Seq(SumEq(Tutor.all.indices.map(t => Var(s"t$t")), Var("totSlots")), Var("totSlots").in(0 to (S*T)))

    val sumTutorsPerSlot: Seq[Constr] = for s <- slots.indices yield SumEq(allocatedVarsOfSlot(s),Var(s"s$s"))
    val lowerBoundPerSlot: Seq[Constr] = for s <- slots.indices yield Var(s"s$s") >= (slots(s).NP - 1)
    val upperBoundPerSlot: Seq[Constr] = for s <- slots.indices yield Var(s"s$s") <= (slots(s).N + 1)

    lazy val allConstraints: Seq[Constr] = 
      allocatedConstr ++ canConstr ++ sumSlotsPerTutor ++ boundsPerTutor ++ sumTutorsPerSlot ++ lowerBoundPerSlot ++ upperBoundPerSlot ++ totalAlloc

    val sc = solver.SearchConfig.defaultSearchConfig.copy(
      valueSelection = solver.ValueSelection.IndomainSimpleRandom,
      variableSelection = solver.VariableSelection.MostConstrainedDynamic,
      solutionLimitOption = Some(100),
      timeOutOption = Some(1L),
    )

    lazy val result: solver.Result = allConstraints.satisfy //.minimize(Var("totSlots"))

    lazy val last = result.lastSolution
    
  object AllocatorJacop:
    enum Course { case Pgk, Dod }
    enum Type { case PgkResurstid, PgkLabb, DodLabb } 
    extension (t: Type) def course = if t == Type.DodLabb then Course.Dod else Course.Pgk

    case class Slot(date: Date, hrs: Int, course: String, tpe: Type, roomStr: String, groupStr: String):
      val rooms: Array[String] = roomStr.split(",")
      val groups: Array[String] = groupStr.split(",")
      val N = groups.length // number of tutors need for this slot
      assert(rooms.length == N, s"nbr of rooms ${rooms.length} and groups $N must be the same")
      val NX = groups.filter(_.startsWith("X")).length
      val NP = groups.filterNot(_.startsWith("X")).length
      assert(N == NP + NX)
    
      
      
  //  ----- TEST DATA
  object Tutor:
    import AllocatorJacop.Type

    val all: ArraySeq[String] = canGrid("init").distinct.to(ArraySeq)

    def can(init: String, date: Date, hrs: Int, tpe: Type): Boolean = 
      canGrid
        .filter("init")(_ == init)
        .filter("datum")(_ == date.show)
        .filter("start")(_.takeWhile(_.isDigit).toIntOption == Some(hrs))
        .filter("typ")(t => Type.valueOf(t) == tpe)
        .apply("kan").headOption.map(_.toLowerCase) == Some("ja")
  
  val PreAddAmbulances: Map[(String, String, String), Int] = Map(
    ("ons", "10", "PgkResurstid") -> 3,
    ("ons", "13", "PgkResurstid") -> 2,
    ("ons", "15", "PgkResurstid") -> 2,
    ("tor", "08", "PgkResurstid") -> 2,
    ("tor", "13", "PgkResurstid") -> 3,
    ("tor", "15", "PgkResurstid") -> 2,
    ("fre", "10", "PgkLabb") -> 4,
    ("fre", "13", "PgkLabb") -> 4,
    ("fre", "15", "PgkLabb") -> 4,
  ).withDefaultValue(0)

  val slotGrid: Grid = Grid.fromLines(slotLines, delim = ';')
  val canGrid: Grid = Grid.fromFile("src/test/data/can-data.csv", delim = ';')

  def slotLines: Vector[String]  = """
datum;start;kurs;typ;lokal;grupp
2024-09-04;10:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma,E:Varg;D1.01,D1.02,D1.03,D1.04
2024-09-04;13:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma;D1.09,D1.10,D1.11
2024-09-04;15:00;Dod;DodLabb;E:Falk,E:Val,E:Varg;D1.05,D1.06,D1.07
2024-09-04;15:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma;D1.12,D1.13,D1.14
2024-09-05;08:00;Dod;DodLabb;E:Alfa,E:Beta,E:Hacke,E:Panter;D1.08,D1.09,D1.10,D1.11
2024-09-05;08:00;Pgk;PgkResurstid;E:Falk,E:Val,E:Varg;C1.01,C1.02,C1.03
2024-09-05;13:00;Dod;DodLabb;E:Falk,E:Hacke,E:Panter,E:Val;D1.01,D1.02,D1.03,D1.04
2024-09-05;13:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma,E:Varg;D1.05,D1.06,D1.07,D1.08
2024-09-05;15:00;Dod;DodLabb;E:Jupiter,E:Mars,E:Saturnus;C1.01,C1.02,C1.03
2024-09-05;15:00;Pgk;PgkResurstid;E:Falk,E:Val,E:Varg;C1.04,C1.05,C1.06
2024-09-06;10:00;Dod;DodLabb;E:Jupiter,E:Mars,E:Saturnus;D1.12,D1.13,D1.14
2024-09-06;10:00;Pgk;PgkLabb;E:Alfa,E:Beta,E:Falk,E:Gamma,E:Hacke,E:Val,E:Varg;D1.01,D1.02,D1.03,D1.04,D1.05,D1.06,D1.07
2024-09-06;13:00;Dod;DodLabb;E:Jupiter,E:Mars,E:Saturnus;C1.04,C1.05,C1.06
2024-09-06;13:00;Pgk;PgkLabb;E:Alfa,E:Beta,E:Falk,E:Gamma,E:Hacke,E:Val,E:Varg;D1.08,D1.09,D1.10,D1.11,D1.12,D1.13,D1.14
2024-09-06;15:00;Pgk;PgkLabb;E:Falk,E:Jupiter,E:Mars,E:Saturnus,E:Val,E:Varg;C1.01,C1.02,C1.03,C1.04,C1.05,C1.06
2024-09-11;10:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma,E:Varg;D1.01,D1.02,D1.03,D1.04
2024-09-11;13:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma;D1.09,D1.10,D1.11
2024-09-11;15:00;Dod;DodLabb;E:Falk,E:Val,E:Varg;D1.05,D1.06,D1.07
2024-09-11;15:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma;D1.12,D1.13,D1.14
2024-09-12;08:00;Dod;DodLabb;E:Alfa,E:Beta,E:Hacke,E:Panter;D1.08,D1.09,D1.10,D1.11
2024-09-12;08:00;Pgk;PgkResurstid;E:Falk,E:Val,E:Varg;C1.01,C1.02,C1.03
2024-09-12;13:00;Dod;DodLabb;E:Falk,E:Hacke,E:Panter,E:Val;D1.01,D1.02,D1.03,D1.04
2024-09-12;13:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma,E:Varg;D1.05,D1.06,D1.07,D1.08
2024-09-12;15:00;Dod;DodLabb;E:Jupiter,E:Mars,E:Saturnus;C1.01,C1.02,C1.03
2024-09-12;15:00;Pgk;PgkResurstid;E:Falk,E:Val,E:Varg;C1.04,C1.05,C1.06
2024-09-13;10:00;Dod;DodLabb;E:Jupiter,E:Mars,E:Saturnus;D1.12,D1.13,D1.14
2024-09-13;13:00;Dod;DodLabb;E:Jupiter,E:Mars,E:Saturnus;C1.04,C1.05,C1.06
2024-09-18;10:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma,E:Varg;D1.01,D1.02,D1.03,D1.04
2024-09-18;13:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma;D1.09,D1.10,D1.11
2024-09-18;15:00;Dod;DodLabb;E:Falk,E:Val,E:Varg;D1.05,D1.06,D1.07
2024-09-18;15:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma;D1.12,D1.13,D1.14
2024-09-19;08:00;Dod;DodLabb;E:Alfa,E:Beta,E:Hacke,E:Panter;D1.08,D1.09,D1.10,D1.11
2024-09-19;08:00;Pgk;PgkResurstid;E:Falk,E:Val,E:Varg;C1.01,C1.02,C1.03
2024-09-19;13:00;Dod;DodLabb;E:Falk,E:Hacke,E:Panter,E:Val;D1.01,D1.02,D1.03,D1.04
2024-09-19;13:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma,E:Varg;D1.05,D1.06,D1.07,D1.08
2024-09-19;15:00;Dod;DodLabb;E:Jupiter,E:Mars,E:Saturnus;C1.01,C1.02,C1.03
2024-09-19;15:00;Pgk;PgkResurstid;E:Falk,E:Val,E:Varg;C1.04,C1.05,C1.06
2024-09-20;10:00;Dod;DodLabb;E:Jupiter,E:Mars,E:Saturnus;D1.12,D1.13,D1.14
2024-09-20;10:00;Pgk;PgkLabb;E:Alfa,E:Beta,E:Falk,E:Gamma,E:Hacke,E:Val,E:Varg;D1.01,D1.02,D1.03,D1.04,D1.05,D1.06,D1.07
2024-09-20;13:00;Dod;DodLabb;E:Jupiter,E:Mars,E:Saturnus;C1.04,C1.05,C1.06
2024-09-20;13:00;Pgk;PgkLabb;E:Alfa,E:Beta,E:Falk,E:Gamma,E:Hacke,E:Val,E:Varg;D1.08,D1.09,D1.10,D1.11,D1.12,D1.13,D1.14
2024-09-20;15:00;Pgk;PgkLabb;E:Falk,E:Jupiter,E:Mars,E:Saturnus,E:Val,E:Varg;C1.01,C1.02,C1.03,C1.04,C1.05,C1.06
2024-09-25;10:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma,E:Varg;D1.01,D1.02,D1.03,D1.04
2024-09-25;13:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma;D1.09,D1.10,D1.11
2024-09-25;15:00;Dod;DodLabb;E:Falk,E:Val,E:Varg;D1.05,D1.06,D1.07
2024-09-25;15:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma;D1.12,D1.13,D1.14
2024-09-26;08:00;Dod;DodLabb;E:Alfa,E:Beta,E:Hacke,E:Panter;D1.08,D1.09,D1.10,D1.11
2024-09-26;08:00;Pgk;PgkResurstid;E:Falk,E:Val,E:Varg;C1.01,C1.02,C1.03
2024-09-26;13:00;Dod;DodLabb;E:Falk,E:Hacke,E:Panter,E:Val;D1.01,D1.02,D1.03,D1.04
2024-09-26;13:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma,E:Varg;D1.05,D1.06,D1.07,D1.08
2024-09-26;15:00;Dod;DodLabb;E:Jupiter,E:Mars,E:Saturnus;C1.01,C1.02,C1.03
2024-09-26;15:00;Pgk;PgkResurstid;E:Falk,E:Val,E:Varg;C1.04,C1.05,C1.06
2024-09-27;10:00;Dod;DodLabb;E:Jupiter,E:Mars,E:Saturnus;D1.12,D1.13,D1.14
2024-09-27;10:00;Pgk;PgkLabb;E:Alfa,E:Beta,E:Falk,E:Gamma,E:Hacke,E:Val,E:Varg;D1.01,D1.02,D1.03,D1.04,D1.05,D1.06,D1.07
2024-09-27;13:00;Dod;DodLabb;E:Jupiter,E:Mars,E:Saturnus;C1.04,C1.05,C1.06
2024-09-27;13:00;Pgk;PgkLabb;E:Alfa,E:Beta,E:Falk,E:Gamma,E:Hacke,E:Val,E:Varg;D1.08,D1.09,D1.10,D1.11,D1.12,D1.13,D1.14
2024-09-27;15:00;Pgk;PgkLabb;E:Falk,E:Jupiter,E:Mars,E:Saturnus,E:Val,E:Varg;C1.01,C1.02,C1.03,C1.04,C1.05,C1.06
2024-10-02;10:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma,E:Varg;D1.01,D1.02,D1.03,D1.04
2024-10-02;13:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma;D1.09,D1.10,D1.11
2024-10-02;15:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma;D1.12,D1.13,D1.14
2024-10-03;08:00;Pgk;PgkResurstid;E:Falk,E:Val,E:Varg;C1.01,C1.02,C1.03
2024-10-03;13:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma,E:Varg;D1.05,D1.06,D1.07,D1.08
2024-10-03;15:00;Pgk;PgkResurstid;E:Falk,E:Val,E:Varg;C1.04,C1.05,C1.06
2024-10-04;10:00;Pgk;PgkLabb;E:Alfa,E:Beta,E:Falk,E:Gamma,E:Hacke,E:Val,E:Varg;D1.01,D1.02,D1.03,D1.04,D1.05,D1.06,D1.07
2024-10-04;13:00;Pgk;PgkLabb;E:Alfa,E:Beta,E:Falk,E:Gamma,E:Hacke,E:Val,E:Varg;D1.08,D1.09,D1.10,D1.11,D1.12,D1.13,D1.14
2024-10-04;15:00;Pgk;PgkLabb;E:Falk,E:Jupiter,E:Mars,E:Saturnus,E:Val,E:Varg;C1.01,C1.02,C1.03,C1.04,C1.05,C1.06
2024-10-09;10:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma,E:Varg;D1.01,D1.02,D1.03,D1.04
2024-10-09;13:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma;D1.09,D1.10,D1.11
2024-10-09;15:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma;D1.12,D1.13,D1.14
2024-10-10;08:00;Pgk;PgkResurstid;E:Falk,E:Val,E:Varg;C1.01,C1.02,C1.03
2024-10-10;13:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma,E:Varg;D1.05,D1.06,D1.07,D1.08
2024-10-10;15:00;Pgk;PgkResurstid;E:Falk,E:Val,E:Varg;C1.04,C1.05,C1.06
2024-10-11;10:00;Pgk;PgkLabb;E:Alfa,E:Beta,E:Falk,E:Gamma,E:Hacke,E:Val,E:Varg;D1.01,D1.02,D1.03,D1.04,D1.05,D1.06,D1.07
2024-10-11;13:00;Pgk;PgkLabb;E:Alfa,E:Beta,E:Falk,E:Gamma,E:Hacke,E:Val,E:Varg;D1.08,D1.09,D1.10,D1.11,D1.12,D1.13,D1.14
2024-10-11;15:00;Pgk;PgkLabb;E:Falk,E:Jupiter,E:Mars,E:Saturnus,E:Val,E:Varg;C1.01,C1.02,C1.03,C1.04,C1.05,C1.06
2024-10-16;10:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma,E:Varg;D1.01,D1.02,D1.03,D1.04
2024-10-16;13:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma;D1.09,D1.10,D1.11
2024-10-16;15:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma;D1.12,D1.13,D1.14
2024-10-17;08:00;Pgk;PgkResurstid;E:Falk,E:Val,E:Varg;C1.01,C1.02,C1.03
2024-10-17;13:00;Pgk;PgkResurstid;E:Alfa,E:Beta,E:Gamma,E:Varg;D1.05,D1.06,D1.07,D1.08
2024-10-17;15:00;Pgk;PgkResurstid;E:Falk,E:Val,E:Varg;C1.04,C1.05,C1.06
2024-10-18;10:00;Pgk;PgkLabb;E:Alfa,E:Beta,E:Falk,E:Gamma,E:Hacke,E:Val,E:Varg;D1.01,D1.02,D1.03,D1.04,D1.05,D1.06,D1.07
2024-10-18;13:00;Pgk;PgkLabb;E:Alfa,E:Beta,E:Falk,E:Gamma,E:Hacke,E:Val,E:Varg;D1.08,D1.09,D1.10,D1.11,D1.12,D1.13,D1.14
2024-10-18;15:00;Pgk;PgkLabb;E:Falk,E:Jupiter,E:Mars,E:Saturnus,E:Val,E:Varg;C1.01,C1.02,C1.03,C1.04,C1.05,C1.06
""".trim.split("\n").toVector


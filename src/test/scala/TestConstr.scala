package reqt

import csp.*

class TestConstr extends munit.FunSuite:

  test("IntVar relation constraints"):
    val x = IntVar(id = "x")
    val cs = Seq(
      x === 42,
      x >= 42,
      x <= 42,
      x > 41,
      x < 43,
    )
    assert(cs ==  Seq(
      XeqC(x,42), 
      XgteqC(x,42), 
      XlteqC(x,42), 
      XgtC(x,41), 
      XltC(x,43),
    ))


  test("EnumVar String value relation constraints"):
    val x = EnumVar("Color", Seq("Red", "Black","White"))
    val cs = Seq(x === x.toInt("Black"))
    assert(cs == Seq(XeqC(x, x.toInt("Black"))))

  test("Ordinal scale prioritization"):
    val rankVars = Seq("exportSvg", "exportCsv", "exportGraphViz","exportMarkdown", "exportLatex").map(Var.apply)
    
    val pairs = rankVars.combinations(2).map(xs => xs(0) -> xs(1)).toSeq  // all pairwise combinations
    
    val deviations = pairs.indices.map(i => Var(s"d$i"))
    
    val comparisons = for i <- pairs.indices yield
      if scala.util.Random.nextBoolean() // simulate human choice that ranks of (a,b) are a < b or a > b
      then XplusYlteqZ(pairs(i)._1, deviations(i), pairs(i)._2)  //  a + di <= b   <=>  a <= b - di
      else XplusYlteqZ(pairs(i)._2, deviations(i), pairs(i)._1)  //  b + di <= a   <=>  b <= a - di

    val rankBounds = rankVars.map(v => v.in(1 to rankVars.length))

    def deviationBounds(d: Int) = 
      require(d >= 0, s"deviation d must be >= 0")
      deviations.map(v => v.in((1 - d) to 1))  // if d == 0 then only deviate one rank
    
    val sumError = sum(deviations) === Var("-totalDeviation")
    
    val allDiff = AllDifferent(rankVars)
    
    def problem(d: Int): Seq[Constr] = comparisons ++ rankBounds ++ deviationBounds(d) :+ sumError :+ allDiff 
    
    def solution(deviation: Int) = 
      problem(deviation)
        .maximize(Var("-totalDeviation"))(using solver.SearchConfig(warnUnsolved = solver.noWarn))

    val debug = false  // change to true to show more stuff from above during test
    if debug then
      println(" === \n\n")
      println(" comparisons");        comparisons.foreach(println)
      println(" rankBounds");         rankBounds.foreach(println)
      println(" deviationBounds(0)"); deviationBounds(0).foreach(println)
      println(" deviationBounds(1)"); deviationBounds(1).foreach(println)
      println(" problem(0)");         problem(0).foreach(println)
      println(" problem(1)");         problem(1).foreach(println)
    
      for d <- 0 to 4  do
        println(s"solution($d):")
        println(solution(d))
    
      println(" === \n\n")
    end if 

    assert:
      val c = solution(deviation = 0).conclusion
      c == solver.InconsistencyFound || c == solver.SolutionFound // sometimes it finds a solution

  test("release planning"):
    val simple = Model(
      Stakeholder("X") has (
        Prio(1),
        Feature("1") has Benefit(4),
        Feature("2") has Benefit(2),
        Feature("3") has Benefit(1),
      ),
      Stakeholder("Y") has (
        Prio(2),
        Feature("1") has Benefit(2),
        Feature("2") has Benefit(1),
        Feature("3") has Benefit(1),
      ),
      Release("A") precedes Release("B"),
      Resource("dev") has (
        Feature("1") has Cost(10),
        Feature("2") has Cost(70),
        Feature("3") has Cost(40),
        Release("A") has Capacity(100),
        Release("B") has Capacity(100),
      ),
      Resource("test") has (
        Feature("1") has Cost(40),
        Feature("2") has Cost(10),
        Feature("3") has Cost(70),
        Release("A") has Capacity(100),
        Release("B") has Capacity(100),
      ),
      Feature("3") precedes Feature("1")
    ) 

    def constraints(m: Model): Seq[Constr] =  
        
      val requiredEntityTypes = List(Release, Feature, Stakeholder, Resource)
      val isRequiredEntityTypes = requiredEntityTypes.toSet
  
      def missing(m: Model): Seq[EntType] = m.ents.map(_.t).filterNot(isRequiredEntityTypes)  // Should be e.t in next version of reqT-lang
      
      def isValid(m: Model): Boolean = missing(m).isEmpty
      
      if !isValid(m) then Seq() else
  
        val stakeholders = m.ents.filter(_.t == Stakeholder).distinct
        val features =     m.ents.filter(_.t == Feature).distinct
        val releases =     m.ents.filter(_.t == Release).distinct
        val resources =    m.ents.filter(_.t == Resource).distinct

        val featureOrder: Seq[Constr] = forAll(features) { f => Var(f.has/Order).in(1 to releases.size) }
        val releaseOrder: Seq[Constr] = forAll(releases) { r => Var(r.has/Order).in(1 to releases.size) }

        println(releases)

        val weightedBenefit: Seq[Constr] = forAll(stakeholders, features): (s, f) => 
          Var(f.has/s.has/Benefit) ===  (Var(s.has/f.has/Benefit) * Var(s.has/Prio))
        
        val featureBenefitSum: Seq[Constr] = forAll(features): f => 
          Var(f.has/Benefit) === sumForAll(stakeholders)(s => Var(f.has/s.has/Benefit)) 

        val featureBenefitPerRelease: Seq[Constr] = forAll(releases, features) { (r, f) =>
          IfThenElse(Var(f.has/Order) === Var(r.has/Order),
            Var(r.has/f.has/Benefit) === Var(f.has/Benefit),
            Var(r.has/f.has/Benefit) === 0) }
        
        val benefitPerRelease: Seq[Constr] = forAll(releases): r =>
          Var(r.has/Benefit) === sumForAll(features)(f => Var(r.has/f.has/Benefit))
        
        val featureCostPerReleasePerResource: Seq[Constr] = 
          forAll(releases,features, resources): (r, f, res) =>
            IfThenElse(Var(f.has/Order) === Var(r.has/Order),
              Var(r.has/res.has/f.has/Cost) === Var(res.has/f.has/Cost),
              Var(r.has/res.has/f.has/Cost) === 0)
        
        val resourceCostPerRelease: Seq[Constr] = forAll(releases,resources): (r, res) =>
          Var(r.has/res.has/Cost) === sumForAll(features)(f => Var(r.has/res.has/f.has/Cost))
        
        val featureCostPerRelease: Seq[Constr] = forAll(releases,features): (r, f) =>
          Var(r.has/f.has/Cost) === sumForAll(resources)(res => Var(r.has/res.has/f.has/Cost)) 
        
        val costPerRelease: Seq[Constr] = forAll(releases): r =>
          Var(r.has/Cost) === sumForAll(features)(f => Var(r.has/f.has/Cost))
      
        val costLimitPerResource: Seq[Constr] = forAll(releases, resources): (r, res) =>
          Var(r.has/res.has/Cost) <= Var(res.has/r.has/Capacity)
        
        val totalCostPerRelease: Seq[Constr] = forAll(releases): r =>
          Var(r.has/Cost) === sumForAll(resources)(res => Var(r.has/res.has/Cost))
        
        def rels(m: Model): Vector[Rel] = m.elems.flatMap:
          case _: Node => Vector() 
          case r@Rel(_, _, m) => Vector(r) ++ rels(m)
        
        val rs = simple.rels
        
        val precedences = rs.collect:
          case Rel(e1, Precedes, Model(Vector(e2: Ent))) => Var(e1.has/Order) < Var(e2.has/Order) 
        
        val exclusions = rs.collect:
          case Rel(e1, Excludes, Model(Vector(e2: Ent))) => Var(e1.has/Order) =/= Var(e2.has/Order) 
          
        val couplings = rs.collect:
          case Rel(e1, Requires, Model(Vector(e2: Ent))) => Var(e1.has/Order) === Var(e2.has/Order)

        val inputConstraints: Seq[Constr] = simple.paths.collect:
          case AttrPath(links, dest: IntAttr) => Var(AttrTypePath(links, dest.t)) === dest.value  

        Seq(inputConstraints, featureOrder, releaseOrder, weightedBenefit, featureBenefitSum, featureBenefitPerRelease, benefitPerRelease, featureCostPerReleasePerResource, resourceCostPerRelease, featureCostPerRelease, costPerRelease, costLimitPerResource, totalCostPerRelease, precedences, exclusions, couplings).flatten

    end constraints

    val problem = constraints(simple)

    //problem.foreach(c => println(s"  Constr:\n    $c"))
    println(s"    problem has ${problem.length} constraints")

    //val solution = problem.maximize(Var(Release("A")/Benefit))
    val solution = problem.maximize(Var(Release("A").has/Benefit))

    val solMap = solution.lastSolution

    // extension [T](at: AttrTypePath[T]) 
    //   def /(value: T): AttrPath[T] = AttrPath(at.links, at.dest.t.apply(value)) 

    val paths = 
      solMap.collect: 
        case (v, x) => 
          val at = v.id.asInstanceOf[AttrTypePath[Int]] 
          val d: IntAttrType = at.dest.asInstanceOf[IntAttrType]
          AttrPath(at.links, d.apply(x))
      .toVector


    //println(s"    solution.conclusion: ${solution.conclusion}")

    println(s"    paths:\n")
    paths.foreach(println)
    val m: Model = paths.foldLeft(Model())(_ ++ _.toModel)
    println("------------ SOLUTION")
    println(m.sorted.showCompact)

    val solReqT3 =  Model(Release("A").has(Feature("1").has(Benefit(0),Cost(0)),Feature("2").has(Benefit(0),Cost(0)),Feature("3").has(Benefit(3),Cost(110)),Resource("dev").has(Feature("1").has(Cost(0)),Feature("2").has(Cost(0)),Feature("3").has(Cost(40)),Cost(40)),Resource("test").has(Feature("1").has(Cost(0)),Feature("2").has(Cost(0)),Feature("3").has(Cost(70)),Cost(70)),Benefit(3),Cost(110),Order(1)),Release("B").has(Feature("1").has(Benefit(8),Cost(50)),Feature("2").has(Benefit(4),Cost(80)),Feature("3").has(Benefit(0),Cost(0)),Resource("dev").has(Feature("1").has(Cost(10)),Feature("2").has(Cost(70)),Feature("3").has(Cost(0)),Cost(80)),Resource("test").has(Feature("1").has(Cost(40)),Feature("2").has(Cost(10)),Feature("3").has(Cost(0)),Cost(50)),Benefit(12),Cost(130),Order(2)),Feature("1").has(Stakeholder("X").has(Benefit(4)),Stakeholder("Y").has(Benefit(4)),Benefit(8),Order(2)),Feature("2").has(Stakeholder("X").has(Benefit(2)),Stakeholder("Y").has(Benefit(2)),Benefit(4),Order(2)),Feature("3").has(Stakeholder("X").has(Benefit(1)),Stakeholder("Y").has(Benefit(2)),Benefit(3),Order(1)),Stakeholder("X").has(Feature("1").has(Benefit(4)),Feature("2").has(Benefit(2)),Feature("3").has(Benefit(1)),Prio(1)),Stakeholder("Y").has(Feature("1").has(Benefit(2)),Feature("2").has(Benefit(1)),Feature("3").has(Benefit(1)),Prio(2)),Resource("dev").has(Release("A").has(Capacity(100)),Release("B").has(Capacity(100)),Feature("1").has(Cost(10)),Feature("2").has(Cost(70)),Feature("3").has(Cost(40))),Resource("test").has(Release("A").has(Capacity(100)),Release("B").has(Capacity(100)),Feature("1").has(Cost(40)),Feature("2").has(Cost(10)),Feature("3").has(Cost(70))))

    println("------------ OLD SOLUTION")
    println(solReqT3.sorted.showCompact)

    assert:
      solReqT3.sorted == m.sorted
  

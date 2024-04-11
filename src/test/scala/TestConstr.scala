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
        Feature("3") has Benefit(1)),
      Stakeholder("Y") has (
        Prio(2),
        Feature("1") has Benefit(2),
        Feature("2") has Benefit(1),
        Feature("3") has Benefit(1)),
      Release("A") precedes Release("B"),
      Resource("dev") has (
        Feature("1") has Cost(10),
        Feature("2") has Cost(70),
        Feature("3") has Cost(40),
        Release("A") has Capacity(100),
        Release("B") has Capacity(100)),
      Resource("test") has (
        Feature("1") has Cost(40),
        Feature("2") has Cost(10),
        Feature("3") has Cost(70),
        Release("A") has Capacity(100),
        Release("B") has Capacity(100)),
      Feature("3") precedes Feature("1")) 

      val requiredEntityTypes = List(Release, Feature, Stakeholder, Resource)
      val isRequiredEntityTypes = requiredEntityTypes.toSet

      def missing(m: Model): Seq[EntType] = m.ents.map(_.et).filterNot(isRequiredEntityTypes)  // Should be e.t in next version of reqT-lang
      
      def isValid(m: Model): Boolean = missing(m).isEmpty

      def constraints(m: Model): Model = if !isValid(m) then Model() else 
        
        val stakeholders = m.ents.filter(_.et == Stakeholder)
        val features =     m.ents.filter(_.et == Feature)
        val releases =     m.ents.filter(_.et == Release)
        val resources =    m.ents.filter(_.et == Resource)

        val featureOrder: Seq[Constr] = forAll(features) { f => Var(f.has/Order).in(1 to releases.size) }
        val releaseOrder: Seq[Constr] = forAll(releases) { r => Var(r.has/Order).in(1 to releases.size) }

        val weightedBenefit = forAll(stakeholders, features): (s, f) => 
          Var(f.has/s.has/Benefit) ===  (Var(s.has/f.has/Benefit) * Var(s.has/Prio))
        
        val featureBenefitSum = forAll(features): f => 
          Var(f.has/Benefit) === sumForAll(stakeholders)(s => Var(f.has/s.has/Benefit)) 

        val featureBenefitPerRelease = forAll(releases, features) { (r, f) =>
          IfThenElse(Var(f/Order) === Var(r/Order),
            Var(r/f/Benefit) === Var(f/Benefit),
            Var(r/f/Benefit) === 0) }
        
        val benefitPerRelease = forAll(releases) { r =>
          Var(r/Benefit) === sumForAll(features)(f => Var(r/f/Benefit))  }    
        
        val featureCostPerReleasePerResource = forAll(releases,features, resources) { (r, f, res) =>
          IfThenElse(Var(f/Order) === Var(r/Order),
            Var(r/res/f/Cost) === Var(res/f/Cost),
            Var(r/res/f/Cost) === 0) }
        
        val resourceCostPerRelease = forAll(releases,resources) { (r, res) =>
          Var(r/res/Cost) === sumForAll(features)(f => Var(r/res/f/Cost))  }
        
        val featureCostPerRelease = forAll(releases,features) { (r, f) =>
          Var(r/f/Cost) === sumForAll(resources)(res => Var(r/res/f/Cost))  }
        
        val costPerRelease = forAll(releases) { r =>
          Var(r/Cost) === sumForAll(features)(f => Var(r/f/Cost))  }
      
        val costLimitPerResource = forAll(releases, resources) { (r, res) =>
          Var(r.has/res.has/Cost) <= Var(res.has/r.has/Capacity)
        }
        
        val totalCostPerRelease = forAll(releases) { r =>
          Var(r/Cost) === sumForAll(resources)(res => Var(r/res/Cost)) }    

        ???
  

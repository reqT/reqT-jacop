# reqT-jacop

* reqT-jacop is a [Scala](https://www.scala-lang.org/) interface to the [JaCoP](https://github.com/radsz/jacop) library for solving constraint satisfaction problems (csp). 

* reqT-jacop depends on [reqT-lang](https://github.com/reqT/reqT-lang) and [org.jacop:jacop](https://search.maven.org/artifact/org.jacop/jacop/4.10.0/jar?eh=)

# Use

* Install [scala-cli](https://scala-cli.virtuslab.org/install)

* Include this in a file called `project.scala`:

```scala
//> using scala 3.4
//> using dep "org.jacop:jacop:4.10.0"
//> using dep "reqt-lang:reqt-lang:4.2.0,url=https://github.com/reqT/reqT-lang/releases/download/4.2.0/reqt-lang_3-4.2.0.jar"
//> using dep "reqt-jacop:reqt-jacop:1.1.0,url=https://github.com/reqT/reqT-jacop/releases/download/v1.1.0/reqt-jacop_3-1.1.0.jar"
```
* With reqT-lang, org.jacop and reqT-jacop on your class path you will get access to constraint satisfaction problem (csp) solving using extension methods such as `satisfy`, `findAll`, `maximise` etc., as exported [here](https://github.com/reqT/reqT-jacop/blob/main/src/main/scala/reqt-jacop.scala#L3).

* Start REPL with `scala-cli repl .` and solve this simple constraint satisfaction problem example:

```scala
scala> import reqt.*

scala> val cs = Seq(Var("x") > Var("y"), Var("x") < 42, Var("y") > 0)
val cs: Seq[reqt.csp.PrimitiveConstr] = List(XgtY(IntVar(id="x"),IntVar(id="y")), XltC(IntVar(id="x"),42), XgtC(IntVar(id="y"),0))

scala> val r1 = cs.satisfy
val r1: reqt.solver.Result = Result(SolutionFound,1,Map(IntVar(id="x") -> 30, IntVar(id="y") -> 11),None,Some(Solutions([nSolutions=1][nVariables=2])))

scala> val r2 = cs.findAll
val r2: reqt.solver.Result = Result(SolutionFound,820,Map(IntVar(id="x") -> 14, IntVar(id="y") -> 3),None,Some(Solutions([nSolutions=820][nVariables=2])))

scala> val n = r2.solutionCount
val n: Int = 820
```
# Build

`source package.sh`

# Contribute

* Contributions are welcome! Contact the current maintainer: bjorn.regnell@cs.lth.se  

* NOTE: If you make a contribution to this repo you agree that the owner of this repo can use your code according to the license of this repo.

# Publish

* Bump the version in `package.sh` and run the script.

* Create a release and upload the jar to the release section of this GitHub repo.

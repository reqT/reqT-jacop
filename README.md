# reqT-jacop

* reqT-jacop is a [Scala](https://www.scala-lang.org/) interface to the [JaCoP](https://github.com/radsz/jacop) library for solving integer constraint satisfaction problems. 

* reqT-jacop depends on [reqT-lang](https://github.com/reqT/reqT-lang). 

# Use

* Install [scala-cli]()

* Include this in a file called `project.scala`:

```scala
//> using scala 3.4
//> using dep "org.jacop:jacop:4.10.0"
//> using dep "reqt-lang:reqt-lang:4.0.0,url=https://github.com/reqT/reqT-lang/releases/download/4.0.0-RC2/reqt-lang_3-4.0.0.jar"
//> using dep "reqt-jacop:reqt-jacop:1.1.0,url=https://github.com/reqT/reqT-jacop/releases/download/v1.1.0/reqt-jacop_3-1.1.0.jar"
```

* Start REPL with `scala-cli repl .` 

* Test solving a constraint model:

```scala
Welcome to Scala 3.3.3 (17.0.10, Java OpenJDK 64-Bit Server VM).
Type in expressions for evaluation. Or try :help.

scala> import reqt.*, constraints.*, jacop.*

scala> Seq(Var("x") > Var("y"), Var("x") < 42, Var("y") > 0).satisfy
val res0: reqt.solver.Result = Result(SolutionFound,1,Map(IntVar(id="x") -> 4, IntVar(id="y") -> 3),None,Some(Solutions([nSolutions=1][nVariables=2])))
```
# Build

`source package.sh`

# Contribute

Contributions are welcome! If you commit to this repo you agree that the owner of this repo can use your code according to the license.

# Publish

* Bump the version in `package.sh` and run the script.

* Create a release and upload the jar to the release section of this GitHub repo.

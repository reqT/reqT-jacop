VER="1.1.0"
scala-cli clean .
scala-cli test .
scala-cli package . --library -o ".scala-build/reqt-jacop_3-$VER.jar"

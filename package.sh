VER="1.1.1"
scala-cli clean .
scala-cli test .
scala-cli package . --library -o ".scala-build/reqt-jacop_3-$VER.jar"

echo "run in terminal to upload release using github cli:"

echo "  gh release create v$VER --generate-notes"
echo "  gh release upload v$VER .scala-build/reqt-jacop_3-$VER.jar"


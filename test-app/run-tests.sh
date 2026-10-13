#!/bin/sh
# Runs the tests in ../tests against ../src.
#
# elm-test can't run them from the package itself: lamdera/codecs isn't on
# package.elm-lang.org so the dependency solver fails, and kernel code (FNV.js,
# JsArray2.js) only compiles when it comes from a package in the cache, not from
# a source directory. So the tests run from this application, which depends on
# lamdera/containers 1.0.0, with a temporary ELM_HOME whose copy of that package
# has its src pointing at ../src. Everything else in the cache is linked to the
# real one.
#
# Usage: ./run-tests.sh [elm-test-rs options, e.g. --fuzz 1000 or --seed 123]

set -eu

APP=$(cd "$(dirname "$0")" && pwd)
ROOT=$(dirname "$APP")
# Lamdera 1.4.0 compiles some constructors to uncurried functions (e.g.
# ElmTestVariant__Labeled$), which elm-test-rs 3.0.0-5 doesn't recognise as tests,
# so it finds none. 1.3.2 works.
LAMDERA=${LAMDERA:-$(npx --yes --package=lamdera@0.19.1-1.3.2 -- sh -c 'command -v lamdera')}
REAL_ELM_HOME=${ELM_HOME:-$HOME/.elm}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

# Download any dependency the cache is missing. lamdera make knows where the
# lamdera/* packages live, but it only fetches "dependencies", so the test
# dependencies are moved in there for this step.
mkdir -p "$TMP/fetch/src"
node -e '
const app = require(process.argv[1]);
for (const kind of ["direct", "indirect"]) {
    Object.assign(app.dependencies[kind], app["test-dependencies"][kind]);
    app["test-dependencies"][kind] = {};
}
app["source-directories"] = ["src"];
console.log(JSON.stringify(app, null, 4));
' "$APP/elm.json" > "$TMP/fetch/elm.json"
printf 'module Fetch exposing (x)\n\n\nx =\n    1\n' > "$TMP/fetch/src/Fetch.elm"
(cd "$TMP/fetch" && "$LAMDERA" make src/Fetch.elm --output=/dev/null > /dev/null)

# Build the temporary ELM_HOME.
mkdir -p "$TMP/elm-home/0.19.1/packages/lamdera/containers/1.0.0"
for entry in "$REAL_ELM_HOME"/* "$REAL_ELM_HOME"/0.19.1/* "$REAL_ELM_HOME"/0.19.1/packages/* "$REAL_ELM_HOME"/0.19.1/packages/lamdera/*; do
    relative=${entry#"$REAL_ELM_HOME"/}
    if [ ! -e "$TMP/elm-home/$relative" ]; then
        ln -s "$entry" "$TMP/elm-home/$relative"
    fi
done
cp "$ROOT/elm.json" "$TMP/elm-home/0.19.1/packages/lamdera/containers/1.0.0/elm.json"
ln -s "$ROOT/src" "$TMP/elm-home/0.19.1/packages/lamdera/containers/1.0.0/src"

rm -rf "$APP/elm-stuff"
cd "$APP"
ELM_HOME="$TMP/elm-home" npx --yes elm-test-rs@3.0.0-5 --offline --compiler "$LAMDERA" "$@"

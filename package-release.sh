#!/bin/bash
function fail() {
    echo $0
    exit -1
}

BINARY=$(which sprinkles) || fail "sprinkles binary not found"
if [ ! -e "$BINARY" ]
then
    echo "Does not exist: $BINARY"
    exit -1
fi

RELEASEID=$(git describe --tags --always || git log --pretty='format:%h' -1)
PREPDIR="release/sprinkles"
mkdir -p "$PREPDIR"

mkdir "$PREPDIR/bin"
cp "$BINARY" "$PREPDIR/bin/"
cp -r README.md LICENSE examples "$PREPDIR/"
cd "$PREPDIR"
rm -rf examples/*/.cache
cd ".."

RELEASE_FILENAME="sprinkles-$RELEASEID"
echo "$RELEASE_FILENAME"

rm -f "$RELEASE_FILENAME.zip"
zip -r "$RELEASE_FILENAME.zip" sprinkles
rm -f "$RELEASE_FILENAME.tar.gz"
tar cvzf "$RELEASE_FILENAME.tar.gz" sprinkles

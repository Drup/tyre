bash -ex .travis-opam.sh

## Documentation stuff

set -e
# Make sure we're not echoing any sensitive data
set +x
set -o errexit -o nounset

eval `opam config env`
./configure --enable-docs
make doc

if [ -z "$TRAVIS" \
     -o "$TRAVIS_PULL_REQUEST" != "false" \
     -o "$TRAVIS_BRANCH" != "master" \
     -o -z "${DOC+x}" \
   ]; then
  echo "This is not a push Travis-ci build, doing nothing..."
  exit 0
else
  echo "Updating docs on Github pages..."
fi

DOCDIR=.gh-pages

# Error out if $GH_TOKEN is empty or unset
: ${GH_TOKEN:?"GH_TOKEN need to be uploaded via travis-encrypt"}

git clone https://${GH_TOKEN}@github.com/${TRAVIS_REPO_SLUG} $DOCDIR 2>&1 | sed -e "s/$GH_TOKEN/!REDACTED!/g"
git -C $DOCDIR checkout gh-pages || git -C $DOCDIR checkout --orphan gh-pages

rm -rf $DOCDIR/dev/*
cp _build/*.docdir/* $DOCDIR/dev

git -C $DOCDIR config user.email "travis@travis-ci.org"
git -C $DOCDIR config user.name "Travis"
git -C $DOCDIR add --all dev
git -C $DOCDIR commit --allow-empty -m "Travis build $TRAVIS_BUILD_NUMBER pushed docs to gh-pages"
git -C $DOCDIR push origin gh-pages 2>&1 | sed -e "s/$GH_TOKEN/!REDACTED!/g"

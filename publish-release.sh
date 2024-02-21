#!/usr/bin/env bash
# Tag and publish a new release of `purescm`
#
# Usage:
#
# ./publish-release.sh <major|minor|patch...>
#

set -eu

# Fail if we are not on trunk
BRANCH=$(git branch --show-current)
if [ "${BRANCH}" != "master" ]; then
  echo "Please checkout master branch"
  exit 1;
fi

if [ $# -eq 0 ]
  then
    echo "No arguments supplied"
    exit 1
fi

git pull

# bump npm version
npm version $1 --no-git-tag-version

# `npm version` also updates spago.yaml, so we grap the new
# tag name from there
NEW_TAG=$(grep -Po "(?<=version: )([\d.]+)" spago.yaml)

git add package.json package-lock.json spago.yaml
git commit -m "$NEW_TAG"
git tag "v$NEW_TAG"

rm -rf bundle output output-es
npm run build

npm publish --tag latest

git push origin

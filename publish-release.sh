#!/usr/bin/env bash
# Tag and publish a new release or `purescm` to npmjs.org
#
# Usage:
#
# ./publish-release.sh <major|minor|patch...>
#

set -eu

if [ -z "$1" ] then
  echo "No arguments supplied"
  exit 1
fi

# bump npm version and tag commit
npm version $1

# bump also in spago
git add spago.yaml
git commit --amend --no-edit

rm -rf bundle output output-es
npm run build

npm publish --tag latest

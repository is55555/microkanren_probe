#!/bin/sh
. ./bin/env.sh
for f in test/*.scm; do
  echo "==> Running $f"
  scheme --script "$f" || echo "❌ Error in $f"
done

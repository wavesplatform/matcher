#!/usr/bin/env sh
IMAGES_DIR="$( cd "$( dirname "$0" )" && pwd )/images"
# Can't run a loop or other script using entrypoint: a rights issue
# Also see https://dreampuf.github.io/GraphvizOnline/ for an online graphviz editor
cd ${IMAGES_DIR}
for f in *.dot
do
  echo "Converting $f..."
  docker run --rm --volume ${IMAGES_DIR}:/app --workdir=/app webuni/graphviz dot -Tsvg "$f" > $(echo $f | sed s/dot/svg/)
done

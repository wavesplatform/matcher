#!/usr/bin/env sh
IMAGES_DIR="$( cd "$( dirname "$0" )" && pwd )/images"
# Can't run a loop or other script using entrypoint: a rights issue
cd ${IMAGES_DIR}
for f in *.mmd
do
  echo "Converting $f..."
  docker run --rm --volume ${IMAGES_DIR}:/data minlag/mermaid-cli:8.8.0 -i "$f" -o $(echo $f | sed s/mmd/svg/)
done

#!/usr/bin/env sh
CURR_DIR="$( cd "$( dirname "$0" )" && pwd )"
docker run --rm --volume ${CURR_DIR}:/docs minlag/mermaid-cli:8.8.0 mmdc -i /docs/images/wni-ext.mmd -o /docs/images/wni-ext.svg

#!/bin/bash

trap 'kill -TERM $PID' TERM INT

echo Config file: ${WAVES_DEX_CONFIGPATH}
echo Options: ${WAVES_DEX_OPTS}
/opt/waves-dex/bin/waves-dex \
  -Dsun.zip.disableMemoryMapping=true \
  ${WAVES_DEX_OPTS} \
  -main com.wavesplatform.dex.Application -- ${WAVES_DEX_CONFIGPATH} &
PID=$!
wait ${PID}
trap - TERM INT
wait ${PID}
EXIT_STATUS=$?

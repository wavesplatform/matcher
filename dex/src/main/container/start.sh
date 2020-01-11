#!/bin/bash

trap 'kill -TERM $PID' TERM INT

echo "Starting process..." >> ${WAVES_DEX_DETAILED_LOG_PATH}
echo "Config file: ${WAVES_DEX_CONFIGPATH}" >> ${WAVES_DEX_DETAILED_LOG_PATH}
echo "Options: ${WAVES_DEX_OPTS}" >> ${WAVES_DEX_DETAILED_LOG_PATH}

# Remove sun.zip.disableMemoryMapping after migration to JRE 9+
# See https://bugs.openjdk.java.net/browse/JDK-8175192
/opt/waves-dex/bin/waves-dex \
  -Dsun.zip.disableMemoryMapping=true \
  ${WAVES_DEX_OPTS} \
  -main com.wavesplatform.dex.Application -- ${WAVES_DEX_CONFIGPATH} &

PID=$!
echo "PID: ${PID}" >> ${WAVES_DEX_DETAILED_LOG_PATH}
wait ${PID}

trap - TERM INT
wait ${PID}
EXIT_STATUS=$?
echo "Exit status: ${EXIT_STATUS}" >> ${WAVES_DEX_DETAILED_LOG_PATH}

#!/bin/bash

trap 'kill -TERM $PID' TERM INT

BASE_CONFIG_PATH="/opt/waves-dex/conf"
RUNTIME_CONFIG_PATH="/opt/waves-dex/runtime"

echo "Base settings:"
cat ${BASE_CONFIG_PATH}/dex.conf
echo; echo

if [ ! -f ${RUNTIME_CONFIG_PATH}/local.conf ] ; then
	echo "Custom settings '${RUNTIME_CONFIG_PATH}/local.conf' not found"
else
	echo "Found custom settings '${RUNTIME_CONFIG_PATH}/local.conf':"
	cat ${RUNTIME_CONFIG_PATH}/local.conf; echo
fi

/opt/waves-dex/bin/waves-dex \
  -Dsun.zip.disableMemoryMapping=true \
  -Dlogback.configurationFile=${BASE_CONFIG_PATH}/logback.xml \
  -main com.wavesplatform.dex.Application ${BASE_CONFIG_PATH}/dex.conf

PID=$!
echo "PID: ${PID}"
wait ${PID}

trap - TERM INT
wait ${PID}
EXIT_STATUS=$?
echo "Exit status: ${EXIT_STATUS}"

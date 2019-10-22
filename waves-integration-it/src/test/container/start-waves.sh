#!/bin/bash
# Overriding to be able to switch configs

trap 'kill -TERM $PID' TERM INT

echo Config file: ${WAVES_NODE_CONFIGPATH}
echo Options: ${WAVES_OPTS}
java ${WAVES_OPTS} -cp "/opt/waves/lib/*" com.wavesplatform.Application ${WAVES_NODE_CONFIGPATH} &
PID=$!
wait ${PID}
trap - TERM INT
wait ${PID}
EXIT_STATUS=$?

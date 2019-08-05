#!/bin/bash
# Overriding to be able to switch configs

trap 'kill -TERM $PID' TERM INT

$WAVES_NODE_CONFIGPATH=${$WAVES_NODE_CONFIGPATH:-/opt/waves-dex/default.conf}
echo Config file: $WAVES_DEX_CONFIGPATH
echo Options: $WAVES_DEX_OPTS
java $WAVES_OPTS -cp "/opt/waves/lib/*" com.wavesplatform.Application $WAVES_NODE_CONFIGPATH &
PID=$!
wait $PID
trap - TERM INT
wait $PID
EXIT_STATUS=$?

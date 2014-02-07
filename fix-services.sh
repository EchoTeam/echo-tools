#!/bin/sh
set -e

HOSTS=$@

MY_HOST="${LOGNAME}@`hostname -s`"
if [[ "$HOSTS" == "" ]]; then
	echo "WARNING: used default host."
	echo ""
	HOSTS="$MY_HOST"
fi

for HOST in ${HOSTS[@]}; do
        echo "==== CHECK HOST: $HOST ===="
	if [[ "$MY_HOST" == "$HOST" ]]; then
		echo "This is my host."
	else
	        ssh -t $HOST "cd /echo/.git/ \
        	                && sh -c 'if [[ \$ECHO_ENV == local ]]; then echo ok; else echo \"ERROR: \\\$ECHO_ENV=\$ECHO_ENV\"; exit 1; fi'"
	fi
done

function rdo {
	CMD="$1"
	if [[ "$MY_HOST" == "$HOST" ]]; then
		CMD="sudo $CMD"
	else
		CMD="ssh -t $HOST 'sudo $CMD'"
	fi
	echo "$ $CMD >"
	sleep 1
	eval $CMD
}

SERVICES=(rate_limit_server customers_service fbi as_parser ebcs console)
for HOST in ${HOSTS[@]}; do
	echo ""
	echo "==== HOST: $HOST; Service: memcached ===="
	rdo "/etc/init.d/memcached start"

	echo "==== HOST: $HOST; Service: riak ===="
	rdo "/etc/init.d/riak stop" || :
	rdo "killall -u riak" || :
	rdo "rm -rf /var/lib/riak/bitcask" || :
	rdo "/etc/init.d/riak start"

	echo "==== HOST: $HOST; Service: rabbitmq ===="
	rdo "/etc/init.d/rabbitmq-server stop" || :
	rdo "killall -u rabbitmq" || :
	rdo "rm -rf /var/lib/rabbitmq/mnesia" || :
	rdo "/etc/init.d/rabbitmq-server start"
	
	for SERVICE in ${SERVICES[@]}; do
		echo "==== HOST: $HOST; Service: $SERVICE ===="
		rdo "service $SERVICE stop" || :
		rdo "killall -u $SERVICE" || :

		FILENAME="/etc/echo/$SERVICE/.erlang.cookie"
		rdo "mkdir -p `dirname $FILENAME`"
		rdo "cp ~/.erlang.cookie $FILENAME"
		rdo "chown $SERVICE:$SERVICE $FILENAME"
		rdo "chmod 400 $FILENAME"
		rdo "service $SERVICE start"
	done
done

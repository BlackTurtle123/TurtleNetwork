#!/bin/bash
NETWORKS="mainnet testnet stagenet"

mkdir -p /var/lib/TN/log
if [ ! -f /etc/TN/TN.conf ] ; then
	echo "Custom '/etc/TN/TN.conf' not found. Using a default one for '${WAVES_NETWORK,,}' network." | tee -a /var/log/TN/TN.log
	if [[  $NETWORKS == *"${WAVES_NETWORK,,}"* ]] ; then
		cp /usr/share/TN/conf/TN-${WAVES_NETWORK}.conf /etc/TN/TN.conf
		# filtering default api-key-hash. remove the string below once 'node/TN-testnet.conf'is updated in the github repo
		sed -i 's/api-key-hash = "H6nsiifwYKYEx6YzYD7woP1XCn72RVvx6tC1zjjLXqsu"//' /etc/TN/TN.conf
	else
		echo "Network '${WAVES_NETWORK,,}' not found. Exiting."
		exit 1
	fi
else
	echo "Found custom '/etc/TN/TN.conf'. Using it."
fi


if [ "${WAVES_VERSION}" == "latest" ] ; then
	filename=$(find /usr/share/TN/lib -name TN-all* -printf '%f\n')
	export WAVES_VERSION=$(echo ${filename##*-} | cut -d\. -f1-3)
fi

[ -n "${WAVES_WALLET_PASSWORD}" ] && JAVA_OPTS="${JAVA_OPTS} -DTN.wallet.password=${WAVES_WALLET_PASSWORD}"
[ -n "${WAVES_WALLET_SEED}" ] && JAVA_OPTS="${JAVA_OPTS} -DTN.wallet.seed=${WAVES_WALLET_SEED}"

JAVA_OPTS="${JAVA_OPTS} -DTN.data-directory=/var/lib/TN/data -DTN.directory=/var/lib/TN"

echo "Node is starting..." | tee -a /var/log/TN/TN.log
echo "WAVES_HEAP_SIZE='${WAVES_HEAP_SIZE}'" | tee -a /var/log/TN/TN.log
echo "WAVES_LOG_LEVEL='${WAVES_LOG_LEVEL}'" | tee -a /var/log/TN/TN.log
echo "WAVES_VERSION='${WAVES_VERSION}'" | tee -a /var/log/TN/TN.log
echo "WAVES_NETWORK='${WAVES_NETWORK}'" | tee -a /var/log/TN/TN.log
echo "WAVES_WALLET_SEED='${WAVES_WALLET_SEED}'" | tee -a /var/log/TN/TN.log
echo "WAVES_WALLET_PASSWORD='${WAVES_WALLET_PASSWORD}'" | tee -a /var/log/TN/TN.log
echo "JAVA_OPTS='${JAVA_OPTS}'" | tee -a /var/log/TN/TN.log

java -Dlogback.stdout.level=${WAVES_LOG_LEVEL} \
	-XX:+ExitOnOutOfMemoryError \
	-Xmx${WAVES_HEAP_SIZE} \
	-Dlogback.file.directory=/var/log/TN \
	-Dconfig.override_with_env_vars=true \
	${JAVA_OPTS} \
	-cp "/usr/share/TN/lib/*" com.wavesplatform.Application /etc/TN/TN.conf
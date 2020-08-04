FROM openjdk:8-jdk-stretch as builder
ARG SBT_VERSION=1.2.8
ARG WAVES_VERSION="latest"
ARG BRANCH="version-0.17.x"
ARG DEB_PACKAGE_NETWORKS


RUN                                                                             \
{                                                                               \
    WAVES_VERSION=$WAVES_VERSION                                            &&  \
    test $WAVES_VERSION = "latest"                                          &&  \
    WAVES_VERSION=$(curl -fsSL                                                  \
        https://api.github.com/repos/BlackTurtle123/TurtleNetwork/releases/latest        \
        | tac | grep -m 1 'tag_name.:' | tr -cd '[0-9\.]')                  &&  \
    echo "Using latest version '${WAVES_VERSION}'"                              \
;} ;                                                                            \
{                                                                               \
    curl -fsSL https://api.github.com/repos/BlackTurtle123/TurtleNetwork/releases        \
        | tac | grep -q "tag_name.*${WAVES_VERSION}"                        &&  \
    echo "GitHub release '${WAVES_VERSION}' found"                              \
;} &&                                                                           \
{                                                                               \
    echo "Downloading Waves '${WAVES_VERSION}' from GitHub"                 &&  \
    mkdir -p /TN/node/target                                             &&  \
    releaseUrl="https://github.com/BlackTurtle123/TurtleNetwork/releases/download"   &&  \
    rawContentUrl="https://raw.githubusercontent.com/BlackTurtle123/TurtleNetwork"   &&  \
                                                                                \
    echo "Downloading jar file"                                             &&  \
    curl -fL ${releaseUrl}/v${WAVES_VERSION}/TN-all-${WAVES_VERSION}.jar     \
        -o /TN/node/target/TN-all-${WAVES_VERSION}.jar                &&  \
                                                                                \
    echo "Downloading mainnet config"                                       &&  \
    curl -fL ${rawContentUrl}/v${WAVES_VERSION}/node/TN-mainnet.conf         \
        -o /TN/node/TN-mainnet.conf                                   &&  \
                                                                                \
    echo "Downloading testnet config"                                       &&  \
    curl -fL ${rawContentUrl}/v${WAVES_VERSION}/node/TN-testnet.conf         \
        -o /TN/node/TN-testnet.conf                                     \
;} ||                                                                           \
{                                                                               \
    echo "Downloading sbt '${SBT_VERSION}'"                                 &&  \
    curl -fL -o sbt-$SBT_VERSION.deb                                            \
        https://dl.bintray.com/sbt/debian/sbt-$SBT_VERSION.deb              &&  \
    dpkg -i sbt-$SBT_VERSION.deb && rm sbt-$SBT_VERSION.deb                 &&  \
    git clone https://github.com/BlackTurtle123/TurtleNetwork.git                        \
        --branch $BRANCH TN && cd TN                                  &&  \
    git config --global user.name "Sbt Builder"                             &&  \
    git config --global user.email "sbt@builder.docker"                     &&  \
    git tag -a "v${WAVES_VERSION}" -m "Docker build"                        &&  \
    SBT_OPTS="-Xmx2g -XX:MaxPermSize=256m -XX:ReservedCodeCacheSize=128m"       \
    sbt "node/assembly"                                                     &&  \
    for network in $DEB_PACKAGE_NETWORKS ; do                                   \
        echo "Building '${network}' package"                                &&  \
        SBT_OPTS="-XX:ReservedCodeCacheSize=128m                                \
            -Xmx2g -Dnetwork=${network}" sbt 'packageAll'                   &&  \
        mkdir -p /out/${network}                                            &&  \
        mv node/target/TN*.deb /out/${network}/                          &&  \
        cp node/target/TN-all*.jar /out/${network}/                      &&  \
        mv grpc-server/target/universal/grpc-server*.tgz /out/${network}/   &&  \
        mv grpc-server/target/grpc-server*.deb /out/${network}/             ;   \
    done                                                                        \
;}

WORKDIR /home

ADD https://github.com/TurtleNetwork/dex/releases/download/v1.2.1/tn-dex-extension-1.2.1.zip .
RUN unzip tn-dex-extension-1.2.1.zip

FROM openjdk:11-jre-slim
ARG WAVES_VERSION="latest".jar
ARG WAVES_LOG_LEVEL="DEBUG"
ARG WAVES_HEAP_SIZE="2g"
ARG WAVES_NETWORK="mainnet"

ENV WAVES_VERSION=$WAVES_VERSION
ENV WAVES_LOG_LEVEL=$WAVES_LOG_LEVEL
ENV WAVES_HEAP_SIZE=$WAVES_HEAP_SIZE
ENV WAVES_NETWORK=$WAVES_NETWORK

COPY --from=builder /TN/node/target/TN-all-*.jar /usr/share/TN/lib/
COPY --from=builder /TN/node/TN-*.conf /usr/share/TN/conf/
COPY entrypoint.sh /bin/
COPY --from=builder /home/lib/ /usr/share/TN/lib/

RUN groupadd -g 143 TN                                                   &&  \
    useradd -d /var/lib/TN -g 143 -u 143 -s /bin/bash -M TN           &&  \
    mkdir -p /var/lib/TN /etc/TN                                      &&  \
    chown -R 143:143 /var/lib/TN /usr/share/TN /etc/TN             &&  \
    chmod -R 777 /var/lib/TN /usr/share/TN /etc/TN                 &&  \
    ln -fs /var/lib/TN/log /var/log/TN

RUN apt-get -y update && apt-get -y install vim

WORKDIR /var/lib/TN
EXPOSE 6861 6860 6560 6862 6887

USER TN
VOLUME /var/lib/TN
ENTRYPOINT ["/bin/entrypoint.sh"]
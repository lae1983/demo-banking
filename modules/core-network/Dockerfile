FROM olegkunitsyn/gnucobol:3.1
RUN mkdir /var/test
WORKDIR /var/test
COPY . .
RUN cobolget update
RUN cobolget install
RUN cobc -x -debug modules/gcblunit/gcblunit.cbl tests/network-test.cbl modules/modules.cpy --job='network-test'

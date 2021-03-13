       >>SOURCE FORMAT FREE
*>**
*>  Test core/network
*>**

>>DEFINE CONSTANT SYSLOG-SEVERITY-EMERGENCY 0
>>DEFINE CONSTANT SYSLOG-SEVERITY-ALERT 1
>>DEFINE CONSTANT SYSLOG-SEVERITY-CRITICAL 2
>>DEFINE CONSTANT SYSLOG-SEVERITY-ERRROR 3
>>DEFINE CONSTANT SYSLOG-SEVERITY-WARNING 4
>>DEFINE CONSTANT SYSLOG-SEVERITY-NOTICE 5
>>DEFINE CONSTANT SYSLOG-SEVERITY-INFORMATIONAL 6
>>DEFINE CONSTANT SYSLOG-SEVERITY-DEBUG 7

>>DEFINE CONSTANT SYSLOG-FACILITY-KERN 0
>>DEFINE CONSTANT SYSLOG-FACILITY-USER 8
>>DEFINE CONSTANT SYSLOG-FACILITY-MAIL 16
>>DEFINE CONSTANT SYSLOG-FACILITY-DAEMON 24
>>DEFINE CONSTANT SYSLOG-FACILITY-AUTH 32
>>DEFINE CONSTANT SYSLOG-FACILITY-SYSLOG 40
>>DEFINE CONSTANT SYSLOG-FACILITY-LPR 48
>>DEFINE CONSTANT SYSLOG-FACILITY-NEWS 56
>>DEFINE CONSTANT SYSLOG-FACILITY-UUCP 64
>>DEFINE CONSTANT SYSLOG-FACILITY-CRON 72
>>DEFINE CONSTANT SYSLOG-FACILITY-AUTHPRIV 80
>>DEFINE CONSTANT SYSLOG-FACILITY-FTP 88
>>DEFINE CONSTANT SYSLOG-FACILITY-LOCAL0 128
>>DEFINE CONSTANT SYSLOG-FACILITY-LOCAL1 136
>>DEFINE CONSTANT SYSLOG-FACILITY-LOCAL2 144
>>DEFINE CONSTANT SYSLOG-FACILITY-LOCAL3 152
>>DEFINE CONSTANT SYSLOG-FACILITY-LOCAL4 160
>>DEFINE CONSTANT SYSLOG-FACILITY-LOCAL5 168
>>DEFINE CONSTANT SYSLOG-FACILITY-LOCAL6 176
>>DEFINE CONSTANT SYSLOG-FACILITY-LOCAL7 184

identification division.
program-id. network-test.
environment division.
configuration section.
repository.
    function send-udp
    function syslog.
data division.
working-storage section.
    01 ws-syslog pic x(1024).
    01 pid usage binary-long.
procedure division.
    perform send-udp-test.
    perform syslog-test.
    perform receive-udp-test.
    *>perform receive-tcp-test.
    goback.

syslog-test section.
    move syslog(
        "logsource"
        "program"
        SYSLOG-FACILITY-USER
        SYSLOG-SEVERITY-ERRROR
        "test message"
    ) to ws-syslog.
    call "assert-equals" using "<11>", ws-syslog(1:5).
    move syslog(
        "logsource"
        "program"
        SYSLOG-FACILITY-LOCAL7
        SYSLOG-SEVERITY-DEBUG
        "test message"
    ) to ws-syslog.
    call "assert-equals" using "<191>", ws-syslog(1:5).
    call "assert-equals" using " logsource program test message: ", ws-syslog(21:33).

send-udp-test section.
    call "assert-equals" using 12, send-udp("ping.online.net", 514, "test message").

receive-udp-test section.
    call "fork" returning pid.
    if pid is ZERO
        call "receive-udp" using "localhost", 1514, 1, address of entry "receive-udp-callback" end-call
    end-if.
    call "sleep" using by value 1.
    call "assert-equals" using 12, send-udp("localhost", 1514, "test message").
    call "wait" using by value pid.

receive-tcp-test section.
    call "receive-tcp" using "localhost", 8000, 1, address of entry "receive-tcp-callback" end-call.
end program network-test.

identification division.
program-id. receive-udp-callback.
data division.
working-storage section.
linkage section.
    01 l-buffer pic x any length.
    01 l-length usage binary-int unsigned.
procedure division using l-buffer, l-length.
    call "assert-equals" using "test message", l-buffer(1:l-length).
end program receive-udp-callback.

identification division.
program-id. receive-tcp-callback.
data division.
working-storage section.
    78 NL value x"0A".
    78 CR value x"0D".
    01 ws-length usage binary-int unsigned.
linkage section.
    01 l-buffer pic x any length.
    01 l-length usage binary-int unsigned.
procedure division using l-buffer, l-length.
    move 1 to ws-length.
    string
        "HTTP/1.1 404" CR NL delimited by size
        "Content-Length: 0" CR NL delimited by size
        CR NL delimited by size
        into l-buffer with pointer ws-length.
    subtract 1 from ws-length.
    move ws-length to RETURN-CODE.
end program receive-tcp-callback.

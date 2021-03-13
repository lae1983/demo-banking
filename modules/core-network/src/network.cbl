       >>SOURCE FORMAT FREE
*>**
*>  Core library: network
*>
*>  @author Olegs Kunicins
*>  @license LGPL-3.0
*>
*>  This library is free software; you can redistribute it and/or
*>  modify it under the terms of the GNU Lesser General Public
*>  License as published by the Free Software Foundation; either
*>  version 3.0 of the License, or (at your option) any later version.
*>  
*>  This library is distributed in the hope that it will be useful,
*>  but WITHOUT ANY WARRANTY; without even the implied warranty of
*>  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*>  Lesser General Public License for more details.
*>  
*>  You should have received a copy of the GNU Lesser General Public
*>  License along with this library.
*>**

*>*
*> Send UDP datagram
*> 
*> @param l-host Remote host name or IP address
*> @param l-port Remote port
*> @param l-message Message
*> @return Number of bytes sent
*>*
identification division.
function-id. send-udp.
environment division.
configuration section.
repository. function byte-length intrinsic.
data division.
working-storage section.
    78 AF_INET value 2.
    78 SOCK_DGRAM value 2.
    01 ws-socket usage binary-int.
linkage section.
    01 l-host pic x(128).
    01 l-port  pic x(5).
    01 l-message pic x any length.
    01 l-result usage binary-long unsigned value 0.
procedure division using l-host, l-port, l-message returning l-result.
    initialize l-result all to value.
    call 'connecttoserver' using
        AF_INET
        SOCK_DGRAM
        l-host
        l-port
        ws-socket
    end-call.
    call 'send' using 
        by value ws-socket
        by reference l-message
        by value  byte-length(l-message)
        by value 0
    end-call.
    move RETURN-CODE to l-result.
    call 'close' using by value ws-socket.
end function send-udp.

*>*
*> Prepare a message in syslog format. RFC 3164
*> 
*> @param l-logsource Logsource name
*> @param l-program Program name
*> @param l-facility Facility code
*> @param l-severity Severity code
*> @param l-message String encoded in UTF-8
*> @return Syslog message
*>*
identification division.
function-id. syslog.
environment division.
configuration section.
repository. 
    function datetime-format
    function trim numval concatenate intrinsic.
data division.
working-storage section.
    01 ws-code usage binary-char unsigned.
    01 ws-syslog-code pic z(3) value SPACE.
linkage section.
    01 l-logsource pic x any length.
    01 l-program pic x any length.
    01 l-facility pic x any length.
    01 l-severity pic x any length.
    01 l-message pic x any length.
    01 l-syslog pic x(1024).
procedure division using l-logsource, l-program, l-facility, l-severity, l-message returning l-syslog.
    move numval(l-severity) to ws-code.
    call "CBL_OR" using numval(l-facility), ws-code by value 1.
    move ws-code to ws-syslog-code.
    move concatenate(
        "<"
        trim(ws-syslog-code)
        ">"
        trim(datetime-format("MMM DD hh:mm:ss", ZERO))
        SPACE
        trim(l-logsource)
        SPACE
        trim(l-program)
        SPACE
        trim(l-message)
        ":"
        SPACE
    ) to l-syslog.
end function syslog.

*>*
*> Receive UDP requests, a UDP server
*> 
*> @param l-host Local host name or IP address
*> @param l-port Local port
*> @param l-limit Limit of requests. 0 - unlimited
*> @param l-callback Program-pointer which handles content
*>*
identification division.
program-id. receive-udp.
environment division.
configuration section.
repository. function trim length intrinsic.
data division.
working-storage section.
    78 AF_INET value 2.
    78 SOCK_DGRAM value 2.
    78 SIGINT value 2.
    01 SOL_SOCKET usage binary-int value 1.
    01 SO_REUSEADDR usage binary-int value 2.
    01 YES usage binary-int value 1.
    01 ws-address.
        05 server-family usage binary-short.
        05 server-port usage binary-short unsigned.
        05 server-ip-address usage binary-int.
        05 filler pic x(8) value low-values.
    01 ws-udp-socket usage binary-int external.
    01 ws-buffer pic x(1024).
    01 ws-buffer-length usage binary-long signed.
    01 ws-error pic x(64).
    01 ws-signal-handler usage program-pointer.
linkage section.
    01 l-host pic x any length.
    01 l-port usage binary-short unsigned.
    01 l-limit usage binary-short unsigned.
    01 l-callback usage program-pointer.
procedure division using l-host, l-port, l-limit, l-callback.
server-start section.
    set ws-signal-handler to entry "server-stop".
    call "signal" using by value SIGINT by value ws-signal-handler.

    call "socket" using
        by value AF_INET
        by value SOCK_DGRAM
        by value 0
        giving ws-udp-socket
    end-call.
    if RETURN-CODE = -1
        move "socket failed" to ws-error
        perform server-error
    end-if.

    call "setsockopt" using
        by value ws-udp-socket
        by value SOL_SOCKET
        by value SO_REUSEADDR
        by reference YES
        by value length(YES)
    end-call. 
    if RETURN-CODE = -1
        move "setsockopt failed" to ws-error
        perform server-error
    end-if.

    call "htons" using by value l-port giving server-port.

    move AF_INET to server-family.
    
    if l-host = "localhost" or "INADDR_ANY"
        move 0 to server-ip-address
    else
        call "inet_addr" using by reference l-host giving server-ip-address end-call
    end-if.

    call "bind" using
        by value ws-udp-socket
        by reference ws-address
        by value length(ws-address)
    end-call.
    if RETURN-CODE = -1
        move "bind failed" to ws-error
        perform server-error
    end-if.

    display "UDP server started on " l-host ":" l-port ". Hit Ctrl+C to stop.".

    perform until exit
        move SPACES to ws-buffer
        call "recv" using
            by value ws-udp-socket
            by reference ws-buffer
            by value length(ws-buffer)
            by value 0
        end-call
        if RETURN-CODE = -1
            move "recv failed" to ws-error
            perform server-error
        end-if
        
        move RETURN-CODE to ws-buffer-length
        if ws-buffer-length = 0
            move SPACE to ws-buffer
            move 1 to ws-buffer-length
        end-if

        call l-callback using ws-buffer, ws-buffer-length on exception 
           display "Error occurred calling message-handler" upon syserr
        end-call

        if l-limit > 0
           subtract 1 from l-limit
           if l-limit = 0
               exit perform
           end-if
        end-if
    end-perform.
    call ws-signal-handler using by value SIGINT.
    goback.
server-error section.
    if ws-udp-socket <> 0
       display "Error: " ws-error upon syserr
    end-if.
    goback.
identification division.
program-id. server-stop.
data division.
working-storage section.
    01 ws-udp-socket usage binary-int external.
linkage section.
    01 l-signal usage binary-long.
procedure division using l-signal returning omitted.
    display SPACE.
    call "close" using by value ws-udp-socket.
    display "UDP server stopped.".
    move 0 to ws-udp-socket.
    goback.
end program server-stop.
end program receive-udp.

*>*
*> Receive TCP requests, a TCP server
*> 
*> @param l-host Local host name or IP address
*> @param l-port Local port
*> @param l-limit Limit of requests. 0 - unlimited
*> @param l-callback Program-pointer which handles content
*>*
identification division.
program-id. receive-tcp.
environment division.
configuration section.
repository. function all intrinsic.
data division.
working-storage section.
    78 AF_INET value 2.
    78 SOCK_STREAM value 1.
    78 SIGINT value 2.
    01 SOL_SOCKET usage binary-int value 1.
    01 SO_REUSEADDR usage binary-int value 2.
    01 YES usage binary-int value 1.
    01 QUEUE-LENGTH binary-char value 2.
    01 ws-server-address.
        05 server-family usage binary-short unsigned.
        05 server-port usage binary-short unsigned.
        05 server-ip-address usage binary-int unsigned.
        05 filler pic x(8) value low-values.
    01 ws-server-socket usage binary-int external.
    01 ws-client-address.
        05 client-family usage binary-short unsigned.
        05 client-port usage binary-short unsigned.
        05 client-ip-address usage binary-int unsigned.
        05 filler pic x(8) value low-values.
    01 ws-client-socket usage binary-int.
    01 ws-client-address-length usage binary-short unsigned.
    01 ws-buffer pic x(1024).
    01 ws-buffer-length usage binary-long signed.
    01 ws-error pic x(64).
    01 ws-signal-handler usage program-pointer.
linkage section.
    01 l-host pic x any length.
    01 l-port usage binary-short unsigned.
    01 l-limit usage binary-short unsigned.
    01 l-callback usage program-pointer.
procedure division using l-host, l-port, l-limit, l-callback.
start-tcpipserver.
    set ws-signal-handler to entry "server-stop".
    call "signal" using by value SIGINT by value ws-signal-handler.

    call "socket" using
        by value AF_INET
        by value SOCK_STREAM
        by value 0
        giving ws-server-socket
    end-call.
    if RETURN-CODE = -1
        move "socket failed" to ws-error
        perform server-error
    end-if.

    call "setsockopt" using
        by value ws-server-socket
        by value SOL_SOCKET
        by value SO_REUSEADDR
        by reference YES
        by value length(YES)
    end-call. 
    if RETURN-CODE = -1
        move "setsockopt failed" to ws-error
        perform server-error
    end-if.

    call "htons" using by value l-port giving server-port end-call.

    move AF_INET to server-family.

    if l-host = "localhost" or "INADDR_ANY"
        move 0 to server-ip-address
    else
        call "inet_addr" using by reference l-host giving server-ip-address end-call
    end-if.

    call "bind" using
        by value ws-server-socket
        by reference ws-server-address
        by value length(ws-server-address)
    end-call.
    if RETURN-CODE = -1
        move "bind failed" to ws-error
        perform server-error
    end-if.

    call "listen" using
        by value ws-server-socket
        by value QUEUE-LENGTH
    end-call.
    if RETURN-CODE = -1
        move "listen failed" to ws-error
        perform server-error
    end-if.

    display "TCP server started on " l-host ":" l-port ". Hit Ctrl+C to stop.".

    perform until exit
        move length(ws-client-address) to ws-client-address-length
        call "accept" using
            by value ws-server-socket
            by reference ws-client-address
            by reference ws-client-address-length
            giving ws-client-socket
        end-call
        if RETURN-CODE = -1
            move "accept failed" to ws-error
            perform server-error
        end-if

        call "setsockopt" using
            by value ws-client-socket
            by value SOL_SOCKET by value SO_REUSEADDR
            by reference YES by value length(YES)
        end-call 
        if RETURN-CODE = -1
            move "setsockopt failed" to ws-error
            perform server-error
        end-if

        perform server-read
        perform until ws-buffer-length = 0
            call l-callback using ws-buffer, ws-buffer-length on exception 
                display "Error occurred calling message-handler" upon syserr
            end-call
            perform server-send
            perform server-read
            move 0 to ws-buffer-length
        end-perform

        call "close" using by value ws-client-socket end-call
        if RETURN-CODE = -1
            move "close failed" to ws-error
            perform server-error
        end-if

        if l-limit > 0
            subtract 1 from l-limit
            if l-limit = 0
                exit perform
            end-if
        end-if
    end-perform.
    call ws-signal-handler using by value SIGINT.
    goback.
server-send section.
    call "send" using
        by value ws-client-socket
        by reference ws-buffer
        by value ws-buffer-length
        by value 0
    end-call.
    if RETURN-CODE = -1
        move "send failed" to ws-error
        perform server-error
    end-if.
server-read section.
    move spaces to ws-buffer.
    call "recv" using
        by value ws-client-socket
        by reference ws-buffer
        by value length(ws-buffer)
        by value 0
    end-call.
    if RETURN-CODE = -1
        move "recv failed" to ws-error
        perform server-error
    end-if.
    move RETURN-CODE to ws-buffer-length.
server-error section.
    if ws-server-socket <> 0
       display ws-error end-display
    end-if.
    goback.
identification division.
program-id. server-stop.
data division.
working-storage section.
    01 ws-server-socket usage binary-int external.
linkage section.
    01 l-signal usage binary-long.
procedure division using l-signal returning omitted.
    display SPACE.
    call 'close' using by value ws-server-socket.
    display "TCP server stopped.".
    move 0 to ws-server-socket.
    goback.
end program server-stop.
end program receive-tcp.

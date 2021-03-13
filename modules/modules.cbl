       >>SOURCE FORMAT FREE
identification division.
program-id. connecttoserver.
*> 
*>  Copyright (C) 2014 Steve Williams <stevewilliams38@gmail.com>
*> 
*>  This program is free software; you can redistribute it and/or
*>  modify it under the terms of the GNU General Public License as
*>  published by the Free Software Foundation; either version 2,
*>  or (at your option) any later version.
*>  
*>  This program is distributed in the hope that it will be useful,
*>  but WITHOUT ANY WARRANTY; without even the implied warranty of
*>  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*>  GNU General Public License for more details.
*>  
*>  You should have received a copy of the GNU General Public
*>  License along with this software; see the file COPYING.
*>  If not, write to the Free Software Foundation, Inc.,
*>  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

data division.
working-storage section.
01 general-message pic x(64).

01 gai-pointer pointer.
01 gai-message pic x(64) based.

01 address-hints.
   03  address-hints-flags binary-int sync.
   03  address-hints-family binary-int sync.
   03  address-hints-socktype binary-int sync.
   03  address-hints-protocol binary-int sync.
   03  address-hints-address-length binary-int sync.
   03  address-hints-address pointer sync.
   03  address-hints-canonname pointer sync.
   03  address-hints-next pointer.
01 getaddrinfo-result pointer.
01 address-info-pointer pointer.
01 address-info based.
   03  address-info-flags binary-int sync.
   03  address-info-family binary-int sync.
   03  address-info-socktype binary-int sync.
   03  address-info-protocol binary-int sync.
   03  address-info-address-length binary-int sync.
   03  address-info-address pointer sync.
   03  address-info-canonname pointer sync.
   03  address-info-next pointer.

01  address-host pic x(128).
01  address-host-service pic x(32).

linkage section.
01 address-family binary-int.
01 address-socktype binary-int.
01 host pic x(128).
01 host-service pic x(32).
01 socket-descriptor binary-int.

procedure division using address-family address-socktype
     host host-service socket-descriptor.

start-connecttoserver.
*>   get the linked list of selected addresses
*>   for this host and host-service
     initialize address-hints
     move address-family to address-hints-family
     move address-socktype to address-hints-socktype
     move spaces to address-host
     string host delimited by space
         x'00' delimited by size
         into address-host
     end-string
     move spaces to address-host-service
     string host-service delimited by space
         x'00' delimited by size
         into address-host-service
     end-string
     call 'getaddrinfo' using
         by content address-host
         by content address-host-service
         by reference address-hints
         by reference getaddrinfo-result
     end-call
     if return-code < 0
         call 'gai_strerror' using by value return-code giving gai-pointer end-call
         set address of gai-message to gai-pointer
         move spaces to general-message
         string 'getaddrinfo failure: ' delimited by size
             gai-message delimited by x'00'
             into general-message
         end-string
         display general-message end-display
         move 0 to socket-descriptor
         goback
     end-if

*>   scan the linked list until we have a connection
     move getaddrinfo-result to address-info-pointer
     perform until address-info-pointer = null
        move 0 to socket-descriptor
        set address of address-info to address-info-pointer
        call 'socket' using
            by value address-info-family
            by value address-info-socktype
            by value 0
            giving socket-descriptor
        end-call
        if return-code <> -1
            call 'connect' using
                by value socket-descriptor
                by value address-info-address
                by value address-info-address-length 
            end-call
            if return-code <> -1
*>              we have a connection
>>D             display 'successful connection '
>>D                 'socket-descriptor = ' socket-descriptor
>>D             end-display
                exit perform
            end-if
            call 'close' using by value socket-descriptor end-call
        end-if
*>      try the next linked list entry
        move address-info-next to address-info-pointer
    end-perform

*>  delete the linked list
    call 'freeaddrinfo' using by value getaddrinfo-result end-call

    if address-info-pointer = null
        move spaces to general-message
        string 'could not connect to ' delimited by size
            host delimited space
            into general-message
        end-string
        display general-message end-display
        move 0 to socket-descriptor
        goback
    end-if
    goback
    .
end program connecttoserver.


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

       >>SOURCE FORMAT FREE
*>**
*>  The KECCAK module, that uses the Keccak-f[1600] permutation.
*>
*>  @author Laszlo Erdos - https://www.facebook.com/wortfee
*>  @license LGPL-3.0
*>
*>  Date-Written: 2016-05-17
*>  Fields in LINKAGE SECTION:
*>    - LNK-KECCAK-RATE: The value of the rate r. The rate must be
*>      a multiple of 8 bits in this implementation.           
*>    - LNK-KECCAK-CAPACITY: The value of the capacity c. 
*>      The rate and capacity must have r+c=1600.       
*>    - LNK-KECCAK-INPUT: The input message.           
*>    - LNK-KECCAK-INPUT-BYTE-LEN: The number of input bytes provided
*>      in the input message.
*>    - LNK-KECCAK-DELIMITED-SUFFIX: Bits that will be automatically
*>      appended to the end of the input message, as in domain 
*>      separation.
*>    - LNK-KECCAK-OUTPUT: The buffer where to store the output.          
*>    - LNK-KECCAK-OUTPUT-BYTE-LEN: The number of output bytes desired.
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

 IDENTIFICATION DIVISION.
 PROGRAM-ID. KECCAK.

 ENVIRONMENT DIVISION.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 WS-STATE                           PIC X(200).
 01 WS-RATE-IN-BYTES                   BINARY-LONG UNSIGNED.
 01 WS-BLOCK-SIZE                      BINARY-LONG UNSIGNED.
 01 WS-IND-1                           BINARY-LONG UNSIGNED. 
 01 WS-INPUT-BYTE-LEN                  BINARY-DOUBLE UNSIGNED.
 01 WS-INPUT-IND                       BINARY-DOUBLE UNSIGNED.
 01 WS-OUTPUT-BYTE-LEN                 BINARY-DOUBLE UNSIGNED.
 01 WS-OUTPUT-IND                      BINARY-DOUBLE UNSIGNED.
 01 WS-CHECK-PADDING-BIT               PIC X.
 
*> linkage for the STATE-PERMUTE module
 01 LNK-STATE-PERMUTE.
   02 LNK-STATE                        PIC X(200).
 
 LINKAGE SECTION.
 01 LNK-KECCAK-RATE                    BINARY-LONG UNSIGNED.
 01 LNK-KECCAK-CAPACITY                BINARY-LONG UNSIGNED.
 01 LNK-KECCAK-INPUT                   PIC X ANY LENGTH.
 01 LNK-KECCAK-INPUT-BYTE-LEN          BINARY-DOUBLE UNSIGNED.
 01 LNK-KECCAK-DELIMITED-SUFFIX        PIC X.
 01 LNK-KECCAK-OUTPUT                  PIC X ANY LENGTH.
 01 LNK-KECCAK-OUTPUT-BYTE-LEN         BINARY-DOUBLE UNSIGNED.
 
 PROCEDURE DIVISION USING LNK-KECCAK-RATE            
                          LNK-KECCAK-CAPACITY        
                          LNK-KECCAK-INPUT           
                          LNK-KECCAK-INPUT-BYTE-LEN  
                          LNK-KECCAK-DELIMITED-SUFFIX
                          LNK-KECCAK-OUTPUT          
                          LNK-KECCAK-OUTPUT-BYTE-LEN. 
 
*>------------------------------------------------------------------------------
 MAIN-KECCAK SECTION.
*>------------------------------------------------------------------------------

*>  Check rate and capacity, they must have r+c=1600
    IF (LNK-KECCAK-RATE + LNK-KECCAK-CAPACITY) NOT = 1600
    THEN
       GOBACK
    END-IF    

*>  Check rate, it must be a multiple of 8 bits in this implementation
    IF FUNCTION MOD(LNK-KECCAK-RATE, 8) NOT = ZEROES
    THEN
       GOBACK
    END-IF    

*>  Initialize fields    
    COMPUTE WS-RATE-IN-BYTES = LNK-KECCAK-RATE / 8 END-COMPUTE
    MOVE LNK-KECCAK-INPUT-BYTE-LEN  TO WS-INPUT-BYTE-LEN
    MOVE ZEROES                     TO WS-INPUT-IND
    MOVE LNK-KECCAK-OUTPUT-BYTE-LEN TO WS-OUTPUT-BYTE-LEN
    MOVE ZEROES                     TO WS-OUTPUT-IND
    MOVE ZEROES                     TO WS-BLOCK-SIZE
    
*>  Initialize the state
    MOVE ALL X"00" TO WS-STATE

*>  Absorb all the input blocks
    PERFORM UNTIL WS-INPUT-BYTE-LEN <= 0
       MOVE FUNCTION MIN(WS-INPUT-BYTE-LEN, WS-RATE-IN-BYTES) TO WS-BLOCK-SIZE
       
       PERFORM VARYING WS-IND-1 FROM 1 BY 1 UNTIL WS-IND-1 > WS-BLOCK-SIZE
          COMPUTE WS-INPUT-IND = WS-INPUT-IND + 1 END-COMPUTE
          
          CALL "CBL_XOR" USING LNK-KECCAK-INPUT(WS-INPUT-IND:1) 
                               WS-STATE(WS-IND-1:1)
                         BY VALUE 1
          END-CALL
       END-PERFORM

       COMPUTE WS-INPUT-BYTE-LEN = WS-INPUT-BYTE-LEN - WS-BLOCK-SIZE 
       END-COMPUTE
       
       IF WS-BLOCK-SIZE = WS-RATE-IN-BYTES
       THEN
          MOVE WS-STATE TO LNK-STATE OF LNK-STATE-PERMUTE
          CALL "STATE-PERMUTE" USING LNK-STATE-PERMUTE END-CALL
          MOVE LNK-STATE OF LNK-STATE-PERMUTE TO WS-STATE

          MOVE ZEROES TO WS-BLOCK-SIZE
       END-IF
    END-PERFORM

*>  Do the padding and switch to the squeezing phase.
*>  Absorb the last few bits and add the first bit of padding (which coincides
*>  with the delimiter in delimitedSuffix)
    CALL "CBL_XOR" USING LNK-KECCAK-DELIMITED-SUFFIX 
                         WS-STATE(WS-BLOCK-SIZE + 1:1)
                   BY VALUE 1
    END-CALL

*>  If the first bit of padding is at position rate - 1, we need a whole
*>  new block for the second bit of padding
    MOVE LNK-KECCAK-DELIMITED-SUFFIX TO WS-CHECK-PADDING-BIT
    CALL "CBL_XOR" USING X"80" 
                         WS-CHECK-PADDING-BIT
                   BY VALUE 1
    END-CALL

    IF  WS-CHECK-PADDING-BIT NOT = X"00"
    AND WS-BLOCK-SIZE = WS-RATE-IN-BYTES - 1
    THEN
       MOVE WS-STATE TO LNK-STATE OF LNK-STATE-PERMUTE
       CALL "STATE-PERMUTE" USING LNK-STATE-PERMUTE END-CALL
       MOVE LNK-STATE OF LNK-STATE-PERMUTE TO WS-STATE
    END-IF
    
*>  Add the second bit of padding
    CALL "CBL_XOR" USING X"80"
                         WS-STATE(WS-RATE-IN-BYTES:1)
                   BY VALUE 1
    END-CALL
    
*>  Switch to the squeezing phase
    MOVE WS-STATE TO LNK-STATE OF LNK-STATE-PERMUTE
    CALL "STATE-PERMUTE" USING LNK-STATE-PERMUTE END-CALL
    MOVE LNK-STATE OF LNK-STATE-PERMUTE TO WS-STATE
    
*>  Squeeze out all the output blocks
    MOVE 1 TO WS-OUTPUT-IND
    PERFORM UNTIL WS-OUTPUT-BYTE-LEN <= 0
       MOVE FUNCTION MIN(WS-OUTPUT-BYTE-LEN, WS-RATE-IN-BYTES) TO WS-BLOCK-SIZE

       MOVE WS-STATE(1:WS-BLOCK-SIZE) 
         TO LNK-KECCAK-OUTPUT(WS-OUTPUT-IND:WS-BLOCK-SIZE)
       COMPUTE WS-OUTPUT-IND = WS-OUTPUT-IND + WS-BLOCK-SIZE END-COMPUTE

       COMPUTE WS-OUTPUT-BYTE-LEN = WS-OUTPUT-BYTE-LEN - WS-BLOCK-SIZE 
       END-COMPUTE
       
       IF WS-OUTPUT-BYTE-LEN > 0
       THEN
          MOVE WS-STATE TO LNK-STATE OF LNK-STATE-PERMUTE
          CALL "STATE-PERMUTE" USING LNK-STATE-PERMUTE END-CALL
          MOVE LNK-STATE OF LNK-STATE-PERMUTE TO WS-STATE
       END-IF
    END-PERFORM
    
    GOBACK
    
    .
 MAIN-KECCAK-EX.
    EXIT.
 END PROGRAM KECCAK.
 
 
*>******************************************************************************
*> Module that computes the Keccak-f[1600] permutation on the given state.
*>******************************************************************************
 IDENTIFICATION DIVISION.
 PROGRAM-ID. STATE-PERMUTE.

 ENVIRONMENT DIVISION.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 WS-ROUND                           BINARY-LONG UNSIGNED.
 01 WS-X                               BINARY-LONG UNSIGNED.
 01 WS-Y                               BINARY-LONG UNSIGNED.
 01 WS-Y-TMP                           BINARY-LONG UNSIGNED.
 01 WS-J                               BINARY-LONG UNSIGNED.
 01 WS-T                               BINARY-LONG UNSIGNED.
 01 WS-R                               BINARY-LONG UNSIGNED.
 01 WS-BIT-POSITION                    BINARY-LONG UNSIGNED.

 01 LFSR-STATE                         PIC X.

 01 WS-C-TAB.
   02 WS-C                             PIC X(8) OCCURS 5.
 01 WS-D                               PIC X(8).
 01 WS-CURRENT                         PIC X(8).
 01 WS-TEMP                            PIC X(8).
 01 WS-TMP-TAB.
   02 WS-TMP                           PIC X(8) OCCURS 5.
 
 01 WS-LANE-0                          PIC X(8).
 01 WS-LANE-1                          PIC X(8).
 01 WS-LANE-2                          PIC X(8).
 01 WS-LANE-3                          PIC X(8).
 01 WS-LANE-4                          PIC X(8).

 01 WS-LANE-X                          PIC X(8).
 01 WS-LANE-NUM REDEFINES WS-LANE-X BINARY-DOUBLE UNSIGNED. 
 
 01 WS-IND-1                           BINARY-LONG. 
 01 WS-IND-2                           BINARY-LONG. 
 
*> linkage for the READ-LANE module
 01 LNK-READ-LANE.
   02 LNK-X                            BINARY-LONG UNSIGNED.
   02 LNK-Y                            BINARY-LONG UNSIGNED.
   02 LNK-STATE                        PIC X(200).
   02 LNK-LANE                         PIC X(8).

*> linkage for the WRITE-LANE module
 01 LNK-WRITE-LANE.
   02 LNK-X                            BINARY-LONG UNSIGNED.
   02 LNK-Y                            BINARY-LONG UNSIGNED.
   02 LNK-STATE                        PIC X(200).
   02 LNK-LANE                         PIC X(8).

*> linkage for the XOR-LANE module
 01 LNK-XOR-LANE.
   02 LNK-X                            BINARY-LONG UNSIGNED.
   02 LNK-Y                            BINARY-LONG UNSIGNED.
   02 LNK-STATE                        PIC X(200).
   02 LNK-LANE                         PIC X(8).
   
*> linkage for the ROL-LANE module
 01 LNK-ROL-LANE.
   02 LNK-LANE                         PIC X(8).
   02 LNK-OFFSET                       BINARY-LONG UNSIGNED.

*> linkage for the LFSR86540 module
 01 LNK-LFSR86540.
   02 LNK-LFSR                         PIC X.
   02 LNK-RESULT                       BINARY-LONG.
   
 LINKAGE SECTION.
 01 LNK-STATE-PERMUTE.
   02 LNK-STATE                        PIC X(200).
 
 PROCEDURE DIVISION USING LNK-STATE-PERMUTE.

*>------------------------------------------------------------------------------
 MAIN-STATE-PERMUTE SECTION.
*>------------------------------------------------------------------------------
    
    MOVE X"01" TO LFSR-STATE
    
    PERFORM VARYING WS-ROUND FROM 0 BY 1 UNTIL WS-ROUND > 23

*>     Theta step (see [Keccak Reference, Section 2.3.2])
       PERFORM STEP-THETA
       
*>     Rho and pi steps (see [Keccak Reference, Sections 2.3.3 and 2.3.4])
       PERFORM STEP-RHO-AND-PI
       
*>     Chi step (see [Keccak Reference, Section 2.3.1])
       PERFORM STEP-CHI
       
*>     Iota step (see [Keccak Reference, Section 2.3.5])
       PERFORM STEP-IOTA
    END-PERFORM
    
    GOBACK
    
    .
 MAIN-STATE-PERMUTE-EX.
    EXIT.
    
*>------------------------------------------------------------------------------
 STEP-THETA SECTION.
*>------------------------------------------------------------------------------

    INITIALIZE WS-C-TAB
    INITIALIZE WS-D
    
*>  Compute the parity of the columns
    PERFORM VARYING WS-X FROM 0 BY 1 UNTIL WS-X > 4
       PERFORM READ-LANES
       PERFORM XOR-LANES
       MOVE WS-LANE-4 TO WS-C(WS-X + 1)
    END-PERFORM
    
    PERFORM VARYING WS-X FROM 0 BY 1 UNTIL WS-X > 4
*>     Compute the theta effect for a given column
       COMPUTE WS-IND-1 = 1 + FUNCTION MOD(WS-X + 4, 5) END-COMPUTE
       MOVE WS-C(WS-IND-1) TO WS-LANE-0
    
       COMPUTE WS-IND-1 = 1 + FUNCTION MOD(WS-X + 1, 5) END-COMPUTE
       MOVE WS-C(WS-IND-1) TO LNK-LANE   OF LNK-ROL-LANE
       MOVE 1              TO LNK-OFFSET OF LNK-ROL-LANE
       CALL "ROL-LANE" USING LNK-ROL-LANE END-CALL
       MOVE LNK-LANE OF LNK-ROL-LANE TO WS-LANE-1
       
       CALL "CBL_XOR" USING WS-LANE-0 WS-LANE-1
                      BY VALUE 8
       END-CALL
       
       MOVE WS-LANE-1 TO WS-D                    
       
*>     Add the theta effect to the whole column   
       PERFORM VARYING WS-Y FROM 0 BY 1 UNTIL WS-Y > 4
          MOVE WS-X 
            TO LNK-X     OF LNK-XOR-LANE
          MOVE WS-Y 
            TO LNK-Y     OF LNK-XOR-LANE
          MOVE LNK-STATE OF LNK-STATE-PERMUTE 
            TO LNK-STATE OF LNK-XOR-LANE
          MOVE WS-D 
            TO LNK-LANE  OF LNK-XOR-LANE
          CALL "XOR-LANE" USING LNK-XOR-LANE END-CALL
          MOVE LNK-STATE OF LNK-XOR-LANE
            TO LNK-STATE OF LNK-STATE-PERMUTE
       END-PERFORM
    END-PERFORM
      
    .
 STEP-THETA-EX.
    EXIT.
    
*>------------------------------------------------------------------------------
 READ-LANES SECTION.
*>------------------------------------------------------------------------------

    MOVE WS-X 
      TO LNK-X     OF LNK-READ-LANE
    MOVE 0 
      TO LNK-Y     OF LNK-READ-LANE
    MOVE LNK-STATE OF LNK-STATE-PERMUTE 
      TO LNK-STATE OF LNK-READ-LANE
    CALL "READ-LANE" USING LNK-READ-LANE END-CALL
    MOVE LNK-LANE  OF LNK-READ-LANE
      TO WS-LANE-0

    MOVE WS-X 
      TO LNK-X     OF LNK-READ-LANE
    MOVE 1 
      TO LNK-Y     OF LNK-READ-LANE
    MOVE LNK-STATE OF LNK-STATE-PERMUTE 
      TO LNK-STATE OF LNK-READ-LANE
    CALL "READ-LANE" USING LNK-READ-LANE END-CALL
    MOVE LNK-LANE  OF LNK-READ-LANE
      TO WS-LANE-1

    MOVE WS-X 
      TO LNK-X     OF LNK-READ-LANE
    MOVE 2 
      TO LNK-Y     OF LNK-READ-LANE
    MOVE LNK-STATE OF LNK-STATE-PERMUTE 
      TO LNK-STATE OF LNK-READ-LANE
    CALL "READ-LANE" USING LNK-READ-LANE END-CALL
    MOVE LNK-LANE  OF LNK-READ-LANE
      TO WS-LANE-2
      
    MOVE WS-X 
      TO LNK-X     OF LNK-READ-LANE
    MOVE 3 
      TO LNK-Y     OF LNK-READ-LANE
    MOVE LNK-STATE OF LNK-STATE-PERMUTE 
      TO LNK-STATE OF LNK-READ-LANE
    CALL "READ-LANE" USING LNK-READ-LANE END-CALL
    MOVE LNK-LANE  OF LNK-READ-LANE
      TO WS-LANE-3
      
    MOVE WS-X 
      TO LNK-X     OF LNK-READ-LANE
    MOVE 4 
      TO LNK-Y     OF LNK-READ-LANE
    MOVE LNK-STATE OF LNK-STATE-PERMUTE 
      TO LNK-STATE OF LNK-READ-LANE
    CALL "READ-LANE" USING LNK-READ-LANE END-CALL
    MOVE LNK-LANE  OF LNK-READ-LANE
      TO WS-LANE-4
    
    .
 READ-LANES-EX.
    EXIT.
    
*>------------------------------------------------------------------------------
 XOR-LANES SECTION.
*>------------------------------------------------------------------------------

    CALL "CBL_XOR" USING WS-LANE-0 WS-LANE-1
                   BY VALUE 8
    END-CALL
    
    CALL "CBL_XOR" USING WS-LANE-1 WS-LANE-2
                   BY VALUE 8
    END-CALL
    
    CALL "CBL_XOR" USING WS-LANE-2 WS-LANE-3
                   BY VALUE 8
    END-CALL
    
    CALL "CBL_XOR" USING WS-LANE-3 WS-LANE-4
                   BY VALUE 8
    END-CALL
      
    .
 XOR-LANES-EX.
    EXIT.
    
*>------------------------------------------------------------------------------
 STEP-RHO-AND-PI SECTION.
*>------------------------------------------------------------------------------

    INITIALIZE WS-CURRENT
    INITIALIZE WS-TEMP
        
*>  Start at coordinates (1 0)
    MOVE 1 TO WS-X
    MOVE 0 TO WS-Y
        
    MOVE WS-X 
      TO LNK-X     OF LNK-READ-LANE
    MOVE WS-Y 
      TO LNK-Y     OF LNK-READ-LANE
    MOVE LNK-STATE OF LNK-STATE-PERMUTE 
      TO LNK-STATE OF LNK-READ-LANE
    CALL "READ-LANE" USING LNK-READ-LANE END-CALL
    MOVE LNK-LANE  OF LNK-READ-LANE
      TO WS-CURRENT

*>  Iterate over ((0 1)(2 3))^t * (1 0) for 0 = t = 23
    PERFORM VARYING WS-T FROM 0 BY 1 UNTIL WS-T > 23
*>     Compute the rotation constant r = (t+1)(t+2)/2
       COMPUTE WS-R = FUNCTION MOD(((WS-T + 1) * (WS-T + 2) / 2), 64)
       END-COMPUTE
       
*>     Compute ((0 1)(2 3)) * (x y)
       COMPUTE WS-Y-TMP = FUNCTION MOD((2 * WS-X + 3 * WS-Y), 5)
       END-COMPUTE
       MOVE WS-Y     TO WS-X
       MOVE WS-Y-TMP TO WS-Y
       
*>     Swap current and state(x,y), and rotate
       MOVE WS-X 
         TO LNK-X     OF LNK-READ-LANE
       MOVE WS-Y 
         TO LNK-Y     OF LNK-READ-LANE
       MOVE LNK-STATE OF LNK-STATE-PERMUTE 
         TO LNK-STATE OF LNK-READ-LANE
       CALL "READ-LANE" USING LNK-READ-LANE END-CALL
       MOVE LNK-LANE  OF LNK-READ-LANE
         TO WS-TEMP
         
       MOVE WS-CURRENT TO LNK-LANE   OF LNK-ROL-LANE
       MOVE WS-R       TO LNK-OFFSET OF LNK-ROL-LANE
       CALL "ROL-LANE" USING LNK-ROL-LANE END-CALL
       MOVE LNK-LANE OF LNK-ROL-LANE TO WS-LANE-0
       
       MOVE WS-X 
         TO L
       >>SOURCE FORMAT FREE
*>**
*>  Core library: string
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
*> Find the position of the first occurrence of a substring in a string.
*> Case-sensitive.
*> 
*> @param l-haystack String to search in
*> @param l-needle String to search for
*> @return Position where the needle exists relative to the beginnning
*> of l-haystack. Returns 0 if not found.
*>*
identification division.
function-id. substr-pos.
environment division.
configuration section.
repository. function length intrinsic.
data division.
working-storage section.
    01 haystack-idx usage index value 1.
    01 needle-idx usage index value 1.
    01 haystack-len usage binary-double unsigned.
    01 needle-len usage binary-double unsigned.
linkage section.
    01 l-haystack pic x any length.
    01 l-needle pic x any length.
    01 l-result usage binary-long unsigned value 0.
procedure division using l-haystack, l-needle returning l-result.
    initialize haystack-idx, needle-idx, l-result all to value.
    move length(l-haystack) to haystack-len.
    move length(l-needle) to needle-len.

    if haystack-len < needle-len
        goback
    end-if.
    perform until haystack-idx > haystack-len
        if l-haystack(haystack-idx:1) = l-needle(needle-idx:1)
           if needle-idx = needle-len
               compute l-result = haystack-idx - needle-idx + 1
               exit perform
           end-if
           set needle-idx up by 1
        else
           initialize needle-idx all to value
        end-if
        set haystack-idx up by 1
    end-perform.
end function substr-pos.


*>*
*> Find the position of the first occurrence of a substring in a string.
*> Case-insensitive.
*> 
*> @param l-haystack String to search in
*> @param l-needle String to search for
*> @return Position where the needle exists relative to the beginnning
*> of l-haystack. Returns 0 if not found.
*>*
identification division.
function-id. substr-pos-case.
environment division.
configuration section.
repository. 
    function lower-case intrinsic
    function substr-pos.
data division.
working-storage section.
linkage section.
    01 l-haystack pic x any length.
    01 l-needle pic x any length.
    01 l-result usage binary-long unsigned value 0.
procedure division using l-haystack, l-needle returning l-result.
    move substr-pos(lower-case(l-haystack), lower-case(l-needle)) to l-result.
end function substr-pos-case.

*>*
*> Convert one byte into hexadecimal representation.
*> 
*> @param l-byte Byte
*> @return 2 hexadecimal chars
*>*
identification division.
function-id. byte-to-hex.
environment division.
configuration section.
data division.
working-storage section.
    01 CHARS pic x(16) value "0123456789ABCDEF".
    01 ws-remainder binary-char unsigned.
    01 ws-quotient binary-char unsigned.
linkage section.
    01 l-byte usage binary-char unsigned.
    01 l-hex pic x(2).
procedure division using l-byte returning l-hex.
    divide l-byte by 16 giving ws-quotient remainder ws-remainder.
    add 1 to ws-remainder.
    add 1 to ws-quotient.
    move CHARS(ws-remainder:1) to l-hex(2:1).
    move CHARS(ws-quotient:1) to l-hex(1:1).
end function byte-to-hex.

*>*
*> Convert one byte into hexadecimal representation.
*> 
*> @param l-hex 2 hexadecimal chars
*> @return Byte
*>*
identification division.
function-id. hex-to-byte.
environment division.
configuration section.
repository. 
    function ord upper-case intrinsic.
data division.
working-storage section.
    01 ws-remainder usage binary-char unsigned.
    01 ws-quotient usage binary-char unsigned.
linkage section.
    01 l-hex pic x(2).
    01 l-byte usage binary-char unsigned.
procedure division using l-hex returning l-byte.
    compute ws-quotient = ord(upper-case(l-hex(1:1))) - 49.
    if ws-quotient > 16
        subtract 7 from ws-quotient
    end-if.
    compute ws-remainder = ord(upper-case(l-hex(2:1))) - 49.
    if ws-remainder > 16
        subtract 7 from ws-remainder
    end-if.
    compute l-byte = ws-quotient * 16 + ws-remainder.
end function hex-to-byte.

*>*
*> Count the number of substring occurrences. Case-sensitive.
*> 
*> @param l-haystack String to search in
*> @param l-needle String to search for
*> @return Number of occurrences
*>*
identification division.
function-id. substr-count.
environment division.
configuration section.
repository. function length intrinsic.
data division.
working-storage section.
    01 haystack-idx usage index value 1.
    01 needle-idx usage index value 1.
    01 haystack-len usage binary-double unsigned.
    01 needle-len usage binary-double unsigned.
    01 needle-char pic x.
    01 haystack-char pic x.
linkage section.
    01 l-haystack pic x any length.
    01 l-needle pic x any length.
    01 l-result usage binary-long unsigned value 0.
procedure division using l-haystack, l-needle returning l-result.
    initialize haystack-idx, needle-idx, l-result all to value.
    move length(l-haystack) to haystack-len.
    move length(l-needle) to needle-len.

    if haystack-len < needle-len
        goback
    end-if.
    perform until haystack-idx > haystack-len or needle-idx > needle-len
        move l-haystack(haystack-idx:1) to haystack-char
        move l-needle(needle-idx:1) to needle-char
        if haystack-char = needle-char
           if needle-idx = needle-len
               add 1 to l-result
               initialize needle-idx all to value
           else
               set needle-idx up by 1
           end-if
        end-if
        set haystack-idx up by 1
    end-perform.
end function substr-count.

*>*
*> Count the number of substring occurrences. Case-insensitive.
*> 
*> @param l-haystack String to search in
*> @param l-needle String to search for
*> @return Number of occurrences
*>*
identification division.
function-id. substr-count-case.
environment division.
configuration section.
repository. function length lower-case intrinsic.
data division.
working-storage section.
    01 haystack-idx usage index value 1.
    01 needle-idx usage index value 1.
    01 haystack-len usage binary-double unsigned.
    01 needle-len usage binary-double unsigned.
    01 needle-char pic x.
    01 haystack-char pic x.
linkage section.
    01 l-haystack pic x any length.
    01 l-needle pic x any length.
    01 l-result usage binary-long unsigned value 0.
procedure division using l-haystack, l-needle returning l-result.
    initialize haystack-idx, needle-idx, l-result all to value.
    move length(l-haystack) to haystack-len.
    move length(l-needle) to needle-len.

    if haystack-len < needle-len
        goback
    end-if.
    perform until haystack-idx > haystack-len or needle-idx > needle-len
        move lower-case(l-haystack(haystack-idx:1)) to haystack-char
        move lower-case(l-needle(needle-idx:1)) to needle-char
        if haystack-char = needle-char
           if needle-idx = needle-len
               add 1 to l-result
               initialize needle-idx all to value
           else
               set needle-idx up by 1
           end-if
        end-if
        set haystack-idx up by 1
    end-perform.
end function substr-count-case.

*>*
*> Generate SHA3-256 message digest
*> 
*> @param l-buffer Input bytes
*> @return 64 hexadecimal chars
*>*
identification division.
function-id. sha3-256.
environment division.
configuration section.
repository. 
    function byte-to-hex 
    function byte-length intrinsic.
data division.
working-storage section.
    78 RATE value 1088.
    78 CAPACITY value 512.
    78 SUFFIX value x"06".
    01 LEN usage binary-double unsigned value 32.
    01 buffer-len usage binary-double unsigned.
    01 ws-idx usage index.
    01 ws-hash pic x(32).
linkage section.
    01 l-buffer pic x any length.
    01 l-hex.
        05 hex pic x(2) occurs 32 times.
procedure division using l-buffer returning l-hex.
    move byte-length(l-buffer) to buffer-len.
    call "KECCAK" using 
        RATE
        CAPACITY
        l-buffer
        buffer-len
        SUFFIX
        ws-hash
        LEN.
    perform varying ws-idx from 1 by 1 until ws-idx > LEN
        move byte-to-hex(ws-hash(ws-idx:1)) to hex(ws-idx)
    end-perform.
end function sha3-256.

*>*
*> Generate SHA3-512 message digest
*> 
*> @param l-buffer Input bytes
*> @return 128 hexadecimal chars
*>*
identification division.
function-id. sha3-512.
environment division.
configuration section.
repository. 
    function byte-to-hex 
    function byte-length intrinsic.
data division.
working-storage section.
    78 RATE value 576.
    78 CAPACITY value 1024.
    78 SUFFIX value x"06".
    01 LEN usage binary-double unsigned value 64.
    01 buffer-len usage binary-double unsigned.
    01 ws-idx usage index.
    01 ws-hash pic x(64).
linkage section.
    01 l-buffer pic x any length.
    01 l-hex.
        05 hex pic x(2) occurs 64 times.
procedure division using l-buffer returning l-hex.
    move byte-length(l-buffer) to buffer-len.
    call "KECCAK" using 
        RATE
        CAPACITY
        l-buffer
        buffer-len
        SUFFIX
        ws-hash 
        LEN.
    perform varying ws-idx from 1 by 1 until ws-idx > LEN
        move byte-to-hex(ws-hash(ws-idx:1)) to hex(ws-idx)
    end-perform.
end function sha3-512.

*>*
*> Convert urlencoded symbol into one byte.
*> 
*> @param l-symbol Urlencoded symbol (3 bytes)
*> @return Byte
*>*
identification division.
function-id. urlencoded-to-byte.
environment division.
configuration section.
repository. function hex-to-byte.
data division.
working-storage section.
linkage section.
    01 l-urlencoded.
        05 filler pic x(1).
        88 is-urlencoded value "%".
        05 hex pic x(2).
    01 l-byte usage binary-char unsigned.
procedure division using l-urlencoded returning l-byte.
    initialize l-byte all to value.
    if is-urlencoded
        move hex-to-byte(hex) to l-byte
    end-if.
end function urlencoded-to-byte.

*>*
*> Convert one byte into urlencoded symbol.
*> 
*> @param l-byte Byte
*> @return Urlencoded symbol (3 bytes)
*>*
identification division.
function-id. byte-to-urlencoded.
environment division.
configuration section.
repository. function byte-to-hex.
data division.
working-storage section.
linkage section.
    01 l-byte usage binary-char unsigned.
    01 l-urlencoded pic x(3).
procedure division using l-byte returning l-urlencoded.
    move "%" to l-urlencoded(1:1).
    move byte-to-hex(l-byte) to l-urlencoded(2:2).
end function byte-to-urlencoded.

*>*
*> Convert ECB exchange rates in CSV format to the list of currency-rate pairs.
*> https://www.ecb.europa.eu/stats/policy_and_exchange_rates/euro_reference_exchange_rates/html/index.en.html
*> 
*> @param l-byte CSV string
*> @return Urlencoded symbol Pointer to the list of 64 [pic x(3), pic 9(7)V9(8)] elements
*>*
identification division.
function-id. csv-ecb-rates.
environment division.
configuration section.
repository. function all intrinsic.
data division.
working-storage section.
    01 ws-header usage binary-char unsigned.
    01 ws-header-idx usage index.
    01 ws-field pic x(32).
    01 ws-csv-pointer usage binary-long unsigned.
    01 ws-field-pointer usage binary-long unsigned.
    01 ws-list.
        05 ws-rates occurs 64 times indexed by ws-rates-idx.
            10 ws-currency pic x(3).
            10 ws-rate pic 9(7)V9(8).
    01 csv-len usage binary-double unsigned.
linkage section.
    01 l-csv pic x any length.
    01 l-list.
        05 l-rates usage pointer.
procedure division using l-csv returning l-list.
    move byte-length(l-csv) to csv-len.
    set l-rates to address of ws-list.
    move 1 to ws-csv-pointer, ws-field-pointer.
    set ws-rates-idx to 1.
    set ws-header-idx to 0.
    move SPACES to ws-field.
    move 1 to ws-header.
    perform until ws-csv-pointer > byte-length(l-csv) - 1
        evaluate TRUE
        when l-csv(ws-csv-pointer:1) = "," and l-csv(1 + ws-csv-pointer:1) = " "
            if ws-rates-idx > 1
                if ws-header = 1
                    move ws-field to ws-currency(ws-rates-idx - 1)  
                else
                    move ws-field to ws-rate(ws-rates-idx - 1) 
                end-if
            end-if
            set ws-rates-idx up by 1
            move SPACES to ws-field
            move 1 to ws-field-pointer
            add 2 to ws-csv-pointer
        when l-csv(ws-csv-pointer:1) = x"0a"
            move 0 to ws-header
            set ws-rates-idx to 1
            add 1 to ws-csv-pointer
        when other
           move l-csv(ws-csv-pointer:1) to ws-field(ws-field-pointer:1)
           add 1 to ws-csv-pointer, ws-field-pointer
        end-evaluate
    end-perform.
end function csv-ecb-rates.

       >>SOURCE FORMAT FREE
*>**
*>  Core library: datetime
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
*> Format the given or current timestamp, replacing the tokens, such as
*> YY    Year                                      18
*> YYYY  Year                                      2018
*> M     Month of the year (1-12)                  7
*> MM    Month of the year (01-12)                 07
*> MMM   Month of the year textual                 Jul
*> D     Day of the month (1-31)                   9
*> DD    Day of the month (01-31)                  09
*> DDD   Day of the year (01-366)                  07
*> WW    Week of the year (01-53)                  05
*> U     Weekday (1-7)                             2
*> EEE   Weekday textual      	                   Tue
*> h     Hour of the day (0-23)                    5
*> hh    Hour of the day (00-23)                   05
*> m     Minute of the hour (0-59)                 9
*> mm    Minute of the hour (00-59)                09
*> s     Second of the minute (0-59)               4
*> ss    Second of the minute (00-59)              04
*> z     Timezone                                  GMT-08:00
*> x     Timezone ISO 8601                         -08:00
*> @param l-format 32-char long string
*> @param l-timestamp 21-char long current-date or ZERO
*> @return Formatted timestamp trailing by spaces, 32-char long
*>*
identification division.
function-id. datetime-format.
environment division.
configuration section.
repository. 
    function current-date numval substitute trim formatted-date integer-of-date intrinsic.
data division.
working-storage section.
    01 WEEKDAYS.
        05 filler pic x(3) value "Mon".
        05 filler pic x(3) value "Tue".
        05 filler pic x(3) value "Wed".
        05 filler pic x(3) value "Thu".
        05 filler pic x(3) value "Fri".
        05 filler pic x(3) value "Sat".
        05 filler pic x(3) value "Sun".
    01 filler redefines WEEKDAYS.
        05 ws-eee pic x(3) occurs 7 times indexed by ws-eee-idx.
    01 MONTHS.
        05 filler pic x(3) value "Jan".
        05 filler pic x(3) value "Feb".
        05 filler pic x(3) value "Mar".
        05 filler pic x(3) value "Apr".
        05 filler pic x(3) value "May".
        05 filler pic x(3) value "Jun".
        05 filler pic x(3) value "Jul".
        05 filler pic x(3) value "Aug".
        05 filler pic x(3) value "Sep".
        05 filler pic x(3) value "Oct".
        05 filler pic x(3) value "Nov".
        05 filler pic x(3) value "Dec".
    01 filler redefines MONTHS.
        05 ws-mmm pic x(3) occurs 12 times indexed by ws-mmm-idx.
    01 ws-timestamp.
        05 ts-yyyy.
           10 filler pic 9(2).
           10 ts-yy pic 9(2).
        05 ts-mm pic z(2).
        05 ts-dd pic z(2).
        05 ts-hh pic 9(2).
        05 ts-mmi pic 9(2).
        05 ts-ss pic 9(2).
        05 filler pic 9(2).
        05 ts-gmt-hours pic S9(2) sign leading separate.
        05 ts-gmt-minutes pic 9(2).
    01 ts-week.
        05 filler pic 9(5).
        05 ts-ww pic 9(2).
        05 ts-u pic 9(1).
    01 ts-d pic z(2) value space.
    01 ts-m pic z(2) value space.
    01 ts-h pic z(2) value space.
    01 ts-mi pic z(2) value space.
    01 ts-s pic z(2) value space.
    01 ts-z.
        05 filler value "GMT".
        05 ts-gmt-hours pic S9(2) sign leading separate.
        05 filler value ":".
        05 ts-gmt-minutes pic 9(2).
    01 ts-x.
        05 ts-gmt-hours pic S9(2) sign leading separate.
        05 filler value ":".
        05 ts-gmt-minutes pic 9(2).
linkage section.
    01 l-format pic x any length.
    01 l-timestamp pic x any length.
    01 l-result pic x(32).
procedure division using l-format, l-timestamp returning l-result.
    if l-timestamp is ZERO
        move current-date to ws-timestamp
    else
        move l-timestamp to ws-timestamp
    end-if.

    move ts-mm to ts-m.
    move ts-dd to ts-d.
    move ts-hh to ts-h.
    move ts-mmi to ts-mi.
    move ts-ss to ts-s.
    move corresponding ws-timestamp to ts-z.
    move corresponding ws-timestamp to ts-x.
    move numval(ts-mm) to ws-mmm-idx.
    move formatted-date("YYYYWwwD", integer-of-date(numval(ws-timestamp(1:8)))) to ts-week.
    move numval(ts-u) to ws-eee-idx.

    move substitute(
        l-format
        "YYYY" ts-yyyy "YY" ts-yy
        "MMM" ws-mmm(ws-mmm-idx) "MM" ts-mm "M" trim(ts-m)
        "DDD" formatted-date("YYYY-DDD", integer-of-date(numval(ws-timestamp(1:8))))(6:3) 
           "DD" ts-dd "D" trim(ts-d)
        "WW" ts-ww
        "U" trim(ts-u)
        "EEE" ws-eee(ws-eee-idx) 
        "hh" ts-hh "h" trim(ts-h)
        "mm" ts-mmi "m" trim(ts-mi)
        "ss" ts-ss "s" trim(ts-s)
        "z" ts-z
        "x" ts-x
    ) to l-result.
end function datetime-format.


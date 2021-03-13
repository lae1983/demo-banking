# Core Network
GnuCOBOL 3.x core library with network functions

## Module Summary

| Name | Description |
| ----------- | ----------- | 
| [receive-udp](#receive-udp) | Receive UDP requests, a UDP server @param l-host Local host name or IP address @param l-port Local port @param l-limit Limit of requests. 0 - unlimited @param l-callback Program-pointer which handles content | 
| [server-stop](#server-stop) |  | 
| [receive-tcp](#receive-tcp) | Receive TCP requests, a TCP server @param l-host Local host name or IP address @param l-port Local port @param l-limit Limit of requests. 0 - unlimited @param l-callback Program-pointer which handles content | 
| [server-stop](#server-stop) |  | 

## Function Summary

| Name | Description |
| ----------- | ----------- | 
| [send-udp](#send-udp) | Send UDP datagram | 
| [syslog](#syslog) | Prepare a message in syslog format. RFC 3164 | 

## Module Details

### receive-udp

Receive UDP requests, a UDP server @param l-host Local host name or IP address @param l-port Local port @param l-limit Limit of requests. 0 - unlimited @param l-callback Program-pointer which handles content

### server-stop



### receive-tcp

Receive TCP requests, a TCP server @param l-host Local host name or IP address @param l-port Local port @param l-limit Limit of requests. 0 - unlimited @param l-callback Program-pointer which handles content

### server-stop



## Function Details

### send-udp

*send-udp(l-host, l-port, l-message)*

Send UDP datagram

#### Parameters

> **l-host** Remote host name or IP address 
> **l-port** Remote port 
> **l-message** Message 

#### Returns

> Number of bytes sent

### syslog

*syslog(l-logsource, l-program, l-facility, l-severity, l-message)*

Prepare a message in syslog format. RFC 3164

#### Parameters

> **l-logsource** Logsource name 
> **l-program** Program name 
> **l-facility** Facility code 
> **l-severity** Severity code 
> **l-message** String encoded in UTF-8 

#### Returns

> Syslog message


# Usage
Install and initialize [COBOL Package Manager](https://cobolget.com):
```
$ npm install -g cobolget
$ cobolget init
```
Add the package to the `Manifest`:
```
$ cobolget add core-network
$ cobolget update
```
Install the package and its dependencies:
```
$ cobolget -t bca12d6c4efed0627c87f2e576b72bdb5ab88e34 install
....
Modules modules.cpy and modules.cbl updated.
```
Directory `modules` contains complete COBOL source-code and `modules.cpy` Copybook ready for inclusion into your project.

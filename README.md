# M3UA Testtool
A test tool for M2UA as specified in [RFC 3331](https://tools.ietf.org/html/rfc3331)
and the ETSI specification
[ETSI TS 102 141](http://www.etsi.org/deliver/etsi_ts/102100_102199/102141/01.01.01_60/ts_102141v010101p.pdf).
The tests are based on the ETSI test specification
[ETSI TS 102 380](http://www.etsi.org/deliver/etsi_ts/102300_102399/102380/01.01.01_60/ts_102380v010101p.pdf).

## Requirements
This tool uses [guile](https://www.gnu.org/software/guile/) and its extension [guile-sctp](https://github.com/nplab/guile-sctp) for SCTP.

## Supported Platforms
It runs on Unix operating systems providing kernel SCTP support:
* FreeBSD.
* Linux, using the `libsctp-dev`package.
* Mac OS X, using the [SCTP-NKE](https://github.com/sctplab/SCTP_NKE_ElCapitan) for adding kernel SCTP support.
* Solaris.

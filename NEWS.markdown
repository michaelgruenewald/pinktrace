## News for pinktrace

This file lists the major changes between versions. For a more detailed list of
every change, see git log.

* New function pink\_name\_lookup\_with\_length()

### 0.1.1
* Include pinktrace.cabal.in and Setup.lhs.in in the tarball
* Include examples in the tarball

### 0.1.0
* Add initial implementation of a higher-level library on top with the name pinktrace-easy
* Simplify GCC macros
* Implement basic pink\_event\_decide() for FreeBSD
* New functions pink\_trace\_lwpinfo() and pink\_trace\_followfork() for FreeBSD
* Add new event PINK\_EVENT\_TRAP for genuine `SIGTRAP`
* Merge Haskell bindings

### 0.0.5
* ruby: Various enhancements
* New function pink\_util\_set\_arg()
* Extend simple-strace examples to decode `bind()` and `connect()` calls
* Extend bindings to cover all functionality and members of pink\_socket\_address\_t
* New member length for pink\_socket\_address\_t
* Add support for decoding netlink socket addresses
* New functions pink\_trace\_sysemu() and pink\_trace\_sysemu\_singlestep()

### 0.0.4
* Fix pink\_decode\_socket\_{address,fd} on ppc64

### 0.0.3
* Add an API to decode NULL-terminated string array members
* Fix pink\_util\_movestr\_persistent(), fixes #7
* New function: pink\_bitness\_wordsize()

### 0.0.2
* Fix compilation on ARM due to missing header

### 0.0.1
* Initial public release

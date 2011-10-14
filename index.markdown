---
layout: default
title: Home
---

* [About](#about)
* [Overview](#overview)
* [Current Status](#current_status)
* [Documentation](#documentation)
* [Bindings](#bindings)
* [Building](#building)
* [Compiling C Code](#compiling_c_code)
* [Examples](#examples)
* [Contribute](#contribute)
* [Supported Platforms](#supported_platforms)
* [License](#license)
* [News](#news)

## About
**pinktrace** is a *ptrace()* wrapper library.

## Overview
**pinktrace** is a lightweight [C99](http://en.wikipedia.org/wiki/C99) library that eases the writing of tracing
applications. It consists of the following parts:

- Wrappers around different *ptrace()* requests.
- An API for decoding arguments (strings, socket addresses, ...)
- An **experimental** API for encoding arguments.
- An initial implementation of a callback-driven higher-level library called pinktrace-easy.

**pinktrace** is currently used by [sydbox](http://git.exherbo.org/?p=sydbox.git;a=summary).

## Current Status
Version {{ site.version }} has an unstable [API](http://en.wikipedia.org/wiki/Api) and
[ABI](http://en.wikipedia.org/wiki/Application_binary_interface).

## Documentation
An extensive API reference is [available]({{ site.url }}api/c).

## Bindings
Bindings are available for:

- [Ruby]({{ site.url }}api/ruby)
- [Python]({{ site.url }}api/python)

**Note**: Bindings for pinktrace-easy have not been written yet.

## Building
This package is made with the GNU autotools, you should run `./configure` inside the distribution directory for
configuring the source tree. Some notable options you may pass to `./configure` are:

* `--enable-easy` Build pinktrace-easy (default)
* `--enable-ipv6` Enable support for [IPV6](http://en.wikipedia.org/wiki/Ipv6)
* `--enable-doxygen` Build API documentation using [Doxygen](http://www.doxygen.org/)
* `--enable-python` Build [Python](http://www.python.org/) bindings
* `--enable-python-doc` Build API documentation of [Python](http://www.python.org/) using
  [epydoc](http://epydoc.sourceforge.net/)
* `--enable-ruby` Build [Ruby](http://ruby-lang.org/) bindings
* `--enable-ruby-doc` Build API documentation of [Ruby](http://ruby-lang.org/) using [rdoc](http://rdoc.sourceforge.net/)

After that you should run `make` for compilation and `make install` (as **root**) for installation of **pinktrace**.
Optionally you may run `make check` to run the unit tests.

## Compiling C Code
You will need to specify various compiler flags when compiling C code. The
usual way to do this is via **pkg-config**:

    $ gcc -c $(pkg-config --cflags pinktrace) -o example.o example.c
    $ gcc $(pkg-config --libs pinktrace) -o example example.o

To use pinktrace-easy use pinktrace\_easy, e.g. `pkg-config --cflags pinktrace_easy`

If you are using autotools, consider using **PKG\_CHECK\_MODULES** rather than
calling **pkg-config** by hand.

## Examples
There are examples how to use the various parts of the library.

<table border="0" summary="examples">
    <tr>
        <th>Example</th>
        <th colspan="3">Language</th>
        <th>Description</th>
    </tr>
    <tr>
        <td>about</td>
        <td><a href="{{ site.url }}c-pink-about.html">C</a></td>
        <td><a href="{{ site.url }}py-pink-about.html">Python</a></td>
        <td><a href="{{ site.url }}rb-pink-about.html">Ruby</a></td>
        <td>How to use pinktrace version macros</td>
    </tr>
    <tr>
        <td>fork_freebsd</td>
        <td><a href="{{ site.url }}c-pink-fork-freebsd.html">C</a></td>
        <td><a href="{{ site.url }}py-pink-fork-freebsd.html">Python</a></td>
        <td><a href="{{ site.url }}rb-pink-fork-freebsd.html">Ruby</a></td>
        <td>How to do tracing fork on FreeBSD</td>
    </tr>
    <tr>
        <td>fork_linux</td>
        <td><a href="{{ site.url }}c-pink-fork-linux.html">C</a></td>
        <td><a href="{{ site.url }}py-pink-fork-linux.html">Python</a></td>
        <td><a href="{{ site.url }}rb-pink-fork-linux.html">Ruby</a></td>
        <td>How to do tracing fork on Linux</td>
    </tr>
    <tr>
        <td>simple_strace_freebsd</td>
        <td><a href="{{ site.url }}c-pink-simple-strace-freebsd.html">C</a></td>
        <td><a href="{{ site.url }}py-pink-simple-strace-freebsd.html">Python</a></td>
        <td><a href="{{ site.url }}rb-pink-simple-strace-freebsd.html">Ruby</a></td>
        <td>A simple strace-like program for FreeBSD</td>
    </tr>
    <tr>
        <td>simple_strace_linux</td>
        <td><a href="{{ site.url }}c-pink-simple-strace-linux.html">C</a></td>
        <td><a href="{{ site.url }}py-pink-simple-strace-linux.html">Python</a></td>
        <td><a href="{{ site.url }}rb-pink-simple-strace-linux.html">Ruby</a></td>
        <td>A simple strace-like program for Linux</td>
    </tr>
</table>

**Note**: There are not any examples for pinktrace-easy at the moment.

## Contribute
Clone [{{ site.url_scm }}]({{ site.url_scm }}).  
Format patches are preferred. Either send a mail to me or poke me on IRC.  
My personal e-mail address is [{{ site.mail }}](mailto://{{ site.mail }})  

## Supported Platforms
FreeBSD and Linux operating systems are supported.  
The supported architectures are:

- [x86](http://en.wikipedia.org/wiki/X86)
- [x86\_64](http://en.wikipedia.org/wiki/X86_64)
- [ia64](http://en.wikipedia.org/wiki/Ia64) (Linux only)
- [ppc](http://en.wikipedia.org/wiki/PowerPC) (Linux only)
- [ppc64](http://en.wikipedia.org/wiki/Ppc64) (Linux only)
- [arm](http://en.wikipedia.org/wiki/ARM_architecture) (Linux only)

**Note**: pinktrace-easy does **not** support FreeBSD at the moment, but support is planned.

## License

Copyright &copy; 2010, 2011 {{ site.author }} &lt;[{{ site.mail }}](mailto:{{ site.mail }})&gt;  
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products
   derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR \`\`AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

## News

*Tue May 03 2011:*

[Version 0.1.1]({{ site.url }}release/pinktrace-0.1.1.tar.bz2) released;

- Include pinktrace.cabal.in and Setup.lhs.in in the tarball
- Include examples in the tarball


*Mon May 02 2011:*

[Version 0.1.0]({{ site.url }}release/pinktrace-0.1.0.tar.bz2) released;

- Add initial implementation of a higher-level library on top with the name pinktrace-easy
- Simplify GCC macros
- Implement basic pink\_event\_decide() for FreeBSD
- New functions pink\_trace\_lwpinfo() and pink\_trace\_followfork() for FreeBSD
- Add new event PINK\_EVENT\_TRAP for genuine `SIGTRAP`
- Merge Haskell bindings


*Sat Oct 30 2010:*

[Version 0.0.5]({{ site.url }}release/pinktrace-0.0.5.tar.bz2) released;
changes include:
- Add new function `pink_util_set_arg()`
- Add new functions `pink_trace_sysemu()` and `pink_trace_sysemu_singlestep()`
- Add support for decoding [Netlink](http://en.wikipedia.org/wiki/Netlink) socket addresses
- Various enhancements for the [bindings](#bindings)

*Sun Oct 17 2010:*

[Version 0.0.4]({{ site.url }}release/pinktrace-0.0.4.tar.bz2) released;
fixes socket decoding functions for PPC64.

*Tue Oct 12 2010:*

[Version 0.0.3]({{ site.url }}release/pinktrace-0.0.3.tar.bz2) released;
fixes a few bugs and adds an [API](http://en.wikipedia.org/wiki/Api) to decode NULL-terminated string arrays.

*Sun Oct 3 2010:*

[Version 0.0.2]({{ site.url }}release/pinktrace-0.0.2.tar.bz2) released;
fixes compilation on ARM due to missing header

*Sun Oct 3 2010:*

[Version 0.0.1]({{ site.url }}release/pinktrace-0.0.1.tar.bz2) released

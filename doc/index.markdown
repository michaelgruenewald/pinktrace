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
**PinkTrace** is a *ptrace()* wrapper library.

## Overview
**PinkTrace** is a lightweight [C99](http://en.wikipedia.org/wiki/C99) library that eases the writing of tracing
applications. It consists of the following parts:

- Wrappers around different *ptrace()* requests.
- An API for decoding arguments (strings, socket addresses, ...)
- An **experimental** API for encoding arguments.
- An initial implementation of a callback-driven higher-level library called pinktrace-easy.

**PinkTrace** is currently used by [sydbox](http://git.exherbo.org/?p=sydbox.git;a=summary).

[Pandora](http://github.com/alip/pandora) uses **PinkTrace-Easy**.

## Current Status
Version {{ site.version }} has an unstable [API](http://en.wikipedia.org/wiki/Api) and
[ABI](http://en.wikipedia.org/wiki/Application_binary_interface).

## Documentation
An extensive API reference is [available]({{ site.uri }}api/c).

## Bindings
Bindings are available for:

- [Haskell]({{ site.uri }}api/haskell)
- [Ruby]({{ site.uri }}api/ruby)
- [Python]({{ site.uri }}api/python)

**Note**: Bindings for pinktrace-easy have not been written yet.

## Building
This package is made with the GNU autotools, you should run `./configure` inside the distribution directory for
configuring the source tree. Some notable options you may pass to `./configure` are:

* `--enable-easy` Build pinktrace-easy (default)
* `--enable-ipv6` Enable support for [IPV6](http://en.wikipedia.org/wiki/Ipv6)
* `--enable-doxygen` Build API documentation using [Doxygen](http://www.doxygen.org/)
* `--enable-haskell` Checks for cabal and generates *Setup.lhs*
* `--enable-python` Build [Python](http://www.python.org/) bindings
* `--enable-python-doc` Build API documentation of [Python](http://www.python.org/) using
  [epydoc](http://epydoc.sourceforge.net/)
* `--enable-ruby` Build [Ruby](http://ruby-lang.org/) bindings
* `--enable-ruby-doc` Build API documentation of [Ruby](http://ruby-lang.org/) using [rdoc](http://rdoc.sourceforge.net/)

After that you should run `make` for compilation and `make install` (as **root**) for installation of **PinkTrace**.
Optionally you may run `make check` to run the unit tests.

As an exception, you should use [Cabal](http://www.haskell.org/cabal/) to build
[Haskell](http://www.haskell.org/haskellwiki/Haskell) bindings. After running `make` to build **PinkTrace** use
`cabal configure` to configure [Haskell](http://www.haskell.org/haskellwiki/Haskell) bindings. Some notable flags you
may pass to `cabal configure` are:

* `-fexample` Build example programs
* `-ftest` Build test programs

After that run `cabal build` for compilation and `cabal install` (as **root**) for installation of
[Haskell](http://www.haskell.org/haskellwiki/Haskell) bindings. Optionally you may run `cabal test` to run the unit
tests.

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

<table border="0">
    <tr>
        <th>Example</th>
        <th>C</th>
        <th>Haskell</th>
        <th>Python</th>
        <th>Ruby</th>
        <th>Description</th>
    </tr>
    <tr>
        <td>about</td>
        <td>
            <a href="{{ site.uri }}c-pink-about.html">about.c</a>
            (<a href="{{ site.uri }}examples/c/pink-about.c">raw</a>)
        </td>
        <td>
            <a href="{{ site.uri }}hs-pink-about.html">about.hs</a>
            (<a href="{{ site.uri }}examples/haskell/pink-about.hs">raw</a>)
        </td>
        <td>
            <a href="{{ site.uri }}py-pink-about.html">about.py</a>
            (<a href="{{ site.uri }}examples/python/pink-about.py">raw</a>)
        </td>
        <td>
            <a href="{{ site.uri }}rb-pink-about.html">about.rb</a>
            (<a href="{{ site.uri }}examples/ruby/pink-about.rb">raw</a>)
        </td>
        <td>How to use PinkTrace version macros</td>
    </tr>
    <tr>
        <td>fork (FreeBSD)</td>
        <td>
            <a href="{{ site.uri }}c-pink-fork-freebsd.html">fork-freebsd.c</a>
            (<a href="{{ site.uri }}examples/c/pink-fork-freebsd.c">raw</a>)
        </td>
        <td>
            <a href="{{ site.uri }}hs-pink-fork-freebsd.html">fork-freebsd.hs</a>
            (<a href="{{ site.uri }}examples/haskell/pink-fork-freebsd.hs">raw</a>)
        </td>
        <td>
            <a href="{{ site.uri }}py-pink-fork-freebsd.html">fork-freebsd.py</a>
            (<a href="{{ site.uri }}examples/python/pink-fork-freebsd.py">raw</a>)
        </td>
        <td>
            <a href="{{ site.uri }}rb-pink-fork-freebsd.html">fork-freebsd.rb</a>
            (<a href="{{ site.uri }}examples/ruby/pink-fork-freebsd.rb">raw</a>)
        </td>
        <td>How to do tracing fork on FreeBSD</td>
    </tr>
    <tr>
        <td>fork (Linux)</td>
        <td>
            <a href="{{ site.uri }}c-pink-fork-linux.html">fork-linux.c</a>
            (<a href="{{ site.uri }}examples/c/pink-fork-linux.c">raw</a>)
        </td>
        <td>
            <a href="{{ site.uri }}hs-pink-fork-linux.html">fork-linux.hs</a>
            (<a href="{{ site.uri }}examples/haskell/pink-fork-linux.hs">raw</a>)
        </td>
        <td>
            <a href="{{ site.uri }}py-pink-fork-linux.html">fork-linux.py</a>
            (<a href="{{ site.uri }}examples/python/pink-fork-linux.py">raw</a>)
        </td>
        <td>
            <a href="{{ site.uri }}rb-pink-fork-linux.html">fork-linux.rb</a>
            (<a href="{{ site.uri }}examples/ruby/pink-fork-linux.rb">raw</a>)
        </td>
        <td>How to do tracing fork on Linux</td>
    </tr>
    <tr>
        <td>simple-strace (FreeBSD)</td>
        <td>
            <a href="{{ site.uri }}c-pink-simple-strace-freebsd.html">simple-strace-freebsd.c</a>
            (<a href="{{ site.uri }}examples/c/pink-simple-strace-freebsd.c">raw</a>)
        </td>
        <td>
            simple-strace-freebsd.hs (TODO)
        </td>
        <td>
            <a href="{{ site.uri }}py-pink-simple-strace-freebsd.html">simple-strace-freebsd.py</a>
            (<a href="{{ site.uri }}examples/python/pink-simple-strace-freebsd.py">raw</a>)
        </td>
        <td>
            <a href="{{ site.uri }}rb-pink-simple-strace-freebsd.html">simple-strace-freebsd.rb</a>
            (<a href="{{ site.uri }}examples/ruby/pink-simple-strace-freebsd.rb">raw</a>)
        </td>
        <td>A simple strace-like program for FreeBSD</td>
    </tr>
    <tr>
        <td>simple-strace (Linux)</td>
        <td>
            <a href="{{ site.uri }}c-pink-simple-strace-linux.html">simple-strace-linux.c</a>
            (<a href="{{ site.uri }}examples/c/pink-simple-strace-linux.c">raw</a>)
        </td>
        <td>
            simple-strace-linux.hs (TODO)
        </td>
        <td>
            <a href="{{ site.uri }}py-pink-simple-strace-linux.html">simple-strace-linux.py</a>
            (<a href="{{ site.uri }}examples/python/pink-simple-strace-linux.py">raw</a>)
        </td>
        <td>
            <a href="{{ site.uri }}rb-pink-simple-strace-linux.html">simple-strace-linux.rb</a>
            (<a href="{{ site.uri }}examples/ruby/pink-simple-strace-linux.rb">raw</a>)
        </td>
        <td>A simple strace-like program for Linux</td>
    </tr>
</table>

**Note**: There are not any examples for pinktrace-easy at the moment.

## Contribute
Clone [{{ site.uri_scm }}]({{ site.uri_scm }}).  
Format patches are preferred. Either send a mail to me or poke me on IRC.  
My personal e-mail address is [{{ site.author }}](mailto://{{ site.mail }})  

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

[Version 0.1.1]({{ site.uri }}release/pinktrace-0.1.1.tar.bz2) released;

- Include pinktrace.cabal.in and Setup.lhs.in in the tarball
- Include examples in the tarball


*Mon May 02 2011:*

[Version 0.1.0]({{ site.uri }}release/pinktrace-0.1.0.tar.bz2) released;

- Add initial implementation of a higher-level library on top with the name pinktrace-easy
- Simplify GCC macros
- Implement basic pink\_event\_decide() for FreeBSD
- New functions pink\_trace\_lwpinfo() and pink\_trace\_followfork() for FreeBSD
- Add new event PINK\_EVENT\_TRAP for genuine `SIGTRAP`
- Merge Haskell bindings


*Sat Oct 30 2010:*

[Version 0.0.5]({{ site.uri }}release/pinktrace-0.0.5.tar.bz2) released;
changes include:
- Add new function `pink_util_set_arg()`
- Add new functions `pink_trace_sysemu()` and `pink_trace_sysemu_singlestep()`
- Add support for decoding [Netlink](http://en.wikipedia.org/wiki/Netlink) socket addresses
- Various enhancements for the [bindings](#bindings)

*Sun Oct 17 2010:*

[Version 0.0.4]({{ site.uri }}release/pinktrace-0.0.4.tar.bz2) released;
fixes socket decoding functions for PPC64.

*Tue Oct 12 2010:*

[Version 0.0.3]({{ site.uri }}release/pinktrace-0.0.3.tar.bz2) released;
fixes a few bugs and adds an [API](http://en.wikipedia.org/wiki/Api) to decode NULL-terminated string arrays.

*Sun Oct 3 2010:*

[Version 0.0.2]({{ site.uri }}release/pinktrace-0.0.2.tar.bz2) released;
fixes compilation on ARM due to missing header

*Sun Oct 3 2010:*

[Version 0.0.1]({{ site.uri }}release/pinktrace-0.0.1.tar.bz2) released

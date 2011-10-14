## PinkTrace TODO list

PinkTrace is a work in progress.  
To contribute start by searching the source tree for TODO and FIXME.
In addition you may:

### Porting
Port pinktrace to different architectures.  
Currently we support:

- [x86](http://en.wikipedia.org/wiki/X86): stable
- [x86\_64](http://en.wikipedia.org/wiki/X86_64): stable
- [ia64](http://en.wikipedia.org/wiki/Ia64): unstable, needs testing
- [ppc](http://en.wikipedia.org/wiki/PowerPC): unstable, needs testing
- [ppc64](http://en.wikipedia.org/wiki/Ppc64): unstable, needs testing
- [arm](http://en.wikipedia.org/wiki/ARM_architecture): preliminary, needs testing

### Bindings
Write bindings for interpreted languages.  
Currently we have bindings for:

- [Python](http://www.python.org/): stable, in master
- [Ruby](http://www.ruby-lang.org/): stable, in master
- [Haskell](http://www.haskell.org/): unstable, in master

### /proc & ioctl() based tracing
Right now, pinktrace is just a **ptrace()** wrapper library.  
Long term ideas include writing a */proc* && <tt>ioctl()</tt> based tracing interface as well.  
Some preliminary work was done in [proc](http://github.com/alip/pinktrace/tree/proc) branch.

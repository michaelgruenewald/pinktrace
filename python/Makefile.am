if ENABLE_PYTHON
CLEANFILES= *.py *.pyc
SUBDIRS= pinktrace .
TESTS= TEST_trace.py \
       TEST_bitness.py \
       TEST_syscall.py \
       TEST_string.py \
       TEST_socket.py
if FREEBSD
TESTS+= \
	TEST_syscall_freebsd.py \
	TEST_string_freebsd.py \
	TEST_socket_freebsd.py
endif
if LINUX
TESTS+= \
	TEST_event.py \
	TEST_syscall_linux.py \
	TEST_string_linux.py
	TEST_socket_linux.py
endif

%.py: %.py.in
	$(AM_V_GEN)
	$(AM_V_at)$(SED) -e "s,##PYTHON##,$(PYTHON)," $< > $@
	$(AM_V_at)chmod +x $@
endif

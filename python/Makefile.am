CLEANFILES= *.pyc
SUBDIRS= pinktrace .
IF_PYTHON_TESTS= \
		 python-test.sh \
		 TEST_trace.py \
		 TEST_bitness.py \
		 TEST_syscall.py \
		 TEST_string.py \
		 TEST_socket.py \
		 TEST_syscall_freebsd.py \
		 TEST_string_freebsd.py \
		 TEST_socket_freebsd.py \
		 TEST_event.py \
		 TEST_syscall_linux.py \
		 TEST_string_linux.py \
		 TEST_socket_linux.py
EXTRA_DIST= $(IF_PYTHON_TESTS)

if ENABLE_PYTHON
TESTS_ENVIRONMENT= PYTHON="$(PYTHON)" ./python-test.sh
TESTS= \
       TEST_trace.py \
       TEST_bitness.py \
       TEST_syscall.py \
       TEST_string.py \
       TEST_socket.py

if FREEBSD
TESTS+= \
	TEST_syscall_freebsd.py \
	TEST_string_freebsd.py \
	TEST_socket_freebsd.py
endif # FREEBSD

if LINUX
TESTS+= \
	TEST_event.py \
	TEST_syscall_linux.py \
	TEST_string_linux.py \
	TEST_socket_linux.py
endif # LINUX

endif # ENABLE_PYTHON

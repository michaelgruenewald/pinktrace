if ENABLE_PYTHON
CLEANFILES= *.py *.pyc
SUBDIRS= pinktrace .
TESTS= TEST_trace.py \
       TEST_event.py \
       TEST_bitness.py \
       TEST_syscall.py \
       TEST_string.py \
       TEST_socket.py
%.py: %.py.in
	$(AM_V_GEN)
	$(AM_V_at)$(SED) -e "s,##PYTHON##,$(PYTHON)," $< > $@
	$(AM_V_at)chmod +x $@
endif

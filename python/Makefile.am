if ENABLE_PYTHON
CLEANFILES= *.py *.pyc
SUBDIRS= pinktrace .
TESTS= TEST_01_trace.py TEST_02_fork.py \
       TEST_03_event.py TEST_04_bitness.py \
       TEST_05_syscall.py TEST_06_string.py \
       TEST_07_socket.py
%.py: %.py.in
	$(AM_V_GEN)
	$(AM_V_at)$(SED) -e "s,##PYTHON##,$(PYTHON)," $< > $@
	$(AM_V_at)chmod +x $@
endif

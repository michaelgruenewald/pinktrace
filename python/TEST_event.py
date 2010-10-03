#!/usr/bin/env python
# coding: utf-8
# vim: set sw=4 ts=4 sts=4 et tw=80 :

import os, signal, sys, unittest

sys.path.insert(0, '.')
from pinktrace import event, trace

class TestEvent_01_Invalid(unittest.TestCase):

    def test_01_decide(self):
        self.assertRaises(TypeError, event.decide)
        self.assertRaises(TypeError, event.decide, 'pink')

class TestEvent_02(unittest.TestCase):

    def test_01_event_unknown(self):
        ev = event.decide(-1)
        self.assertEqual(ev, event.EVENT_UNKNOWN)

    def test_02_event_stop(self):
        pid = os.fork()
        if not pid: # child
            trace.me()
            os.kill(os.getpid(), signal.SIGSTOP)

            os._exit(0)
        else: # parent
            pid, status = os.waitpid(pid, 0)
            ev = event.decide(status)
            self.assertEqual(ev, event.EVENT_STOP)

            try: trace.kill(pid)
            except OSError: pass

    def test_03_event_syscall(self):
        pid = os.fork()
        if not pid: # child
            trace.me()
            os.kill(os.getpid(), signal.SIGSTOP)

            os.sleep(1)
            os._exit(0)
        else: # parent
            os.waitpid(pid, 0)
            trace.setup(pid)

            trace.syscall(pid)
            pid, status = os.waitpid(pid, 0)
            ev = event.decide(status)
            self.assertEqual(ev, event.EVENT_SYSCALL)

            try: trace.kill(pid)
            except OSError: pass

    def test_04_event_fork(self):
        pid = os.fork()
        if not pid: # child
            trace.me()
            os.kill(os.getpid(), signal.SIGSTOP)

            os.fork()
            os._exit(0)
        else: # parent
            os.waitpid(pid, 0)
            trace.setup(pid, trace.OPTION_SYSGOOD | trace.OPTION_FORK)

            trace.cont(pid)
            pid, status = os.waitpid(pid, 0)
            ev = event.decide(status)
            self.assertEqual(ev, event.EVENT_FORK)

            try: trace.kill(pid)
            except OSError: pass

    def test_05_event_vfork(self):
        pass

    def test_06_event_clone(self):
        pass

    def test_07_event_vfork_done(self):
        pass

    def test_08_event_exec(self):
        pid = os.fork()
        if not pid: # child
            trace.me()
            os.kill(os.getpid(), signal.SIGSTOP)

            os.execvp("/bin/true", ("true",))
            os._exit(0)
        else: # parent
            os.waitpid(pid, 0)
            trace.setup(pid, trace.OPTION_SYSGOOD | trace.OPTION_EXEC)

            trace.cont(pid)
            pid, status = os.waitpid(pid, 0)
            ev = event.decide(status)
            self.assertEqual(ev, event.EVENT_EXEC)

            try: trace.kill(pid)
            except OSError: pass

    def test_09_event_exit(self):
        pid = os.fork()
        if not pid: # child
            trace.me()
            os.kill(os.getpid(), signal.SIGSTOP)

            os._exit(13)
        else: # parent
            os.waitpid(pid, 0)
            trace.setup(pid, trace.OPTION_SYSGOOD | trace.OPTION_EXIT)

            trace.cont(pid)
            pid, status = os.waitpid(pid, 0)
            ev = event.decide(status)
            self.assertEqual(ev, event.EVENT_EXIT)
            msg = trace.geteventmsg(pid)
            self.assertEqual(os.WEXITSTATUS(msg), 13)

            try: trace.kill(pid)
            except OSError: pass

    def test_10_event_genuine(self):
        pid = os.fork()
        if not pid: # child
            trace.me()
            os.kill(os.getpid(), signal.SIGSTOP)

            os.kill(os.getpid(), signal.SIGTSTP)
            os._exit(0)
        else: # parent
            os.waitpid(pid, 0)
            trace.setup(pid)

            trace.cont(pid)
            pid, status = os.waitpid(pid, 0)
            ev = event.decide(status)
            self.assertEqual(ev, event.EVENT_GENUINE)

            try: trace.kill(pid)
            except OSError: pass

    def test_11_event_exit_genuine(self):
        pid = os.fork()
        if not pid: # child
            trace.me()
            os.kill(os.getpid(), signal.SIGSTOP)

            os._exit(0)
        else: # parent
            os.waitpid(pid, 0)
            trace.setup(pid)

            trace.cont(pid)
            pid, status = os.waitpid(pid, 0)
            ev = event.decide(status)
            self.assertEqual(ev, event.EVENT_EXIT_GENUINE)

            try: trace.kill(pid)
            except OSError: pass

    def test_12_event_exit_signal(self):
        pid = os.fork()
        if not pid: # child
            trace.me()
            os.kill(os.getpid(), signal.SIGSTOP)

            os.kill(os.getpid(), signal.SIGKILL)
        else: # parent
            os.waitpid(pid, 0)
            trace.setup(pid)

            trace.cont(pid)
            pid, status = os.waitpid(pid, 0)
            ev = event.decide(status)
            self.assertEqual(ev, event.EVENT_EXIT_SIGNAL)

            try: trace.kill(pid)
            except OSError: pass

if __name__ == '__main__':
    unittest.main()

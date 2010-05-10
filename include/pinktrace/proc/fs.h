/* vim: set cino= fo=croql sw=8 ts=8 sts=0 noet cin fdm=syntax : */

/*
 * Copyright (c) 2010 Ali Polatel <alip@exherbo.org>
 *
 * This file is part of Pink's Tracing Library. pinktrace is free software; you
 * can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License version 2.1, as published by the Free Software
 * Foundation.
 *
 * pinktrace is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef PINKTRACE_GUARD_PROC_FS_H
#define PINKTRACE_GUARD_PROC_FS_H 1

#if defined(PINKTRACE_FREEBSD) || defined(DOXYGEN)

#include <stdbool.h>

#include <sys/types.h>
#include <sys/pioctl.h>

/**
 * \file Pink's /proc & ioctl based tracing interface
 *
 * Availability: FreeBSD
 **/

/**
 * This define represents the proc option LINGER.
 * If this flag is set in options argument of pink_proc_open(), pinktrace keeps
 * stops around after last close of the file descriptors
 **/
#define PINK_PROC_OPTION_LINGER     (PF_LINGER)
/**
 * This define represents the proc option ISUGID.
 * If this flag is set in options argument of pink_proc_open(), pinktrace
 * ignores UID/GID changes.
 **/
#define PINK_PROC_OPTION_ISUGID     (PF_ISUGID)
/**
 * This define represents the proc option FOLLOW_FORK
 * If this flag is set in options argument of pink_proc_open(), pinktrace
 * retains settings on fork().
 **/
#define PINK_PROC_OPTION_FOLLOW_FORK (PF_FORK)

/** stop-on-exec **/
#define PINK_PROC_EVENT_EXEC   (S_EXEC)
/** stop-on-signal **/
#define PINK_PROC_EVENT_SIGNAL (S_SIG)
/** stop on syscall entry **/
#define PINK_PROC_EVENT_SCE    (S_SCE)
/** stop on syscall exit **/
#define PINK_PROC_EVENT_SCX    (S_SCX)
/** stop on coredump **/
#define PINK_PROC_EVENT_CORE   (S_CORE)
/** stop on exit **/
#define PINK_PROC_EVENT_EXIT   (S_EXIT)
/** stop on all events **/
#define PINK_PROC_EVENT_ALL    (PINK_PROC_EVENT_EXEC |\
				PINK_PROC_EVENT_SIGNAL |\
				PINK_PROC_EVENT_SCE |\
				PINK_PROC_EVENT_SCX |\
				PINK_PROC_EVENT_CORE |\
				PINK_PROC_EVENT_EXIT)

/**
 * Open the /proc file descriptor
 *
 * \param pid Process ID of the child, if this argument is smaller than zero,
 * /proc/curproc/mem is opened instead of /proc/$pid/mem.
 * \param fd Address to save the /proc file descriptor
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_proc_open(pid_t pid, int *fd);

/**
 * Get flags
 *
 * \param fd /proc file descriptor
 * \param flags Address to save the flags
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_proc_get_flags(int fd, int *flags);

/**
 * Set flags
 *
 * \param fd /proc file descriptor
 * \param flags Bitwise OR'ed PINK_PROC_* flags
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_proc_set_flags(int fd, int flags);

/**
 * Set event flags
 *
 * \param fd /proc file descriptor
 * \param flags Bitwise OR'ed PINK_PROC_EVENT_* flags
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_proc_set_event_flags(int fd, int flags);

/**
 * Clear event flags
 *
 * \param fd /proc file descriptor
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_proc_clear_event_flags(int fd);

/**
 * Let the child continue its execution.
 *
 * \param fd /proc file descriptor
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_proc_cont(int fd);

/**
 * Wait for events
 *
 * \param fd /proc file descriptor
 * \param status Address the save the procfs_status structure
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_proc_wait(int fd, struct procfs_status *status);

/**
 * Get status information
 *
 * \param fd /proc file descriptor
 * \param status Address the save the procfs_status structure
 *
 * \return true on success, false on failure and sets errno accordingly.
 **/
bool
pink_proc_status(int fd, struct procfs_status *status);

#endif /* defined(PINKTRACE_FREEBSD)... */
#endif /* !PINKTRACE_GUARD_ABOUT_H */

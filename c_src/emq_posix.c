// Copyright (c) 2010, João Henrique Ferreira de Freitas
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
//  * Redistributions of source code must retain the above copyright notice,
//    this list of conditions and the following disclaimer.
//  * Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.x

// Includes
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>
#include <fcntl.h>           /* For O_* constants */
#include <sys/stat.h>        /* For mode constants */
#include <mqueue.h>
#include <sys/queue.h>

#include "emq_posix.h"
#include "emq_posix_commands.h"
#include "erl_driver.h"
#include "erl_interface.h"
#include "ei.h"

/* Queue state structure */
typedef struct state_queue_s {
  int inuse;
  mqd_t fd_queue;
  struct mq_attr attr_queue;
  LIST_ENTRY(state_queue_s) queues;
} state_queue;

/* State structure */
typedef struct {
  LIST_HEAD(mq_list, state_queue_s) mq_head;
  ei_x_buff eixb;
  char *args;
  int argslen;
  int index;
  int version;
  ErlDrvPort drv_port;
} state;

static void data_dump(intptr_t *p, size_t n);
static void init_state(state *st, char *args, int argslen);
static state_queue *insert_next_queue(state *st, int qdesc);
static int remove_next_queue(state *st, int qdesc);
static void ok(state *st);
static void error_tuple(state *st, int code);
static void boolean(state *st, int code);
static void queue_data(ei_x_buff *eixb, const void *msg, int size, unsigned int prio);
static void encode_ok_queue(state *st, int queue);
static void encode_queue_attr(state *st, long mq_flags, long mq_maxmsg, long mq_msgsize, long mq_curmsgs);

void tuple(ei_x_buff *eixb, int size);
void atom(ei_x_buff *eixb, const char *str, int size);
void integer(ei_x_buff *eixb, int integer);
void string(ei_x_buff *eixb, const char *str);
void encode_ok_reply(state *st, int code);
int findfreewindowslot(state *st);
void loop_getch(void *arg);

static void do_open_queue(state *st, int flag);
static void do_create_queue(state *st, int flag);
static void do_close_queue(state *st);
static void do_remove_queue(state *st);
static void do_getattr_queue(state *st);
static void do_send_queue(state *st);
static void do_receive_queue(state *st);
static void do_initselect_queue(state *st);
static void do_deselect_queue(state *st);

#define P(x) do { printf x;} while(0);
#define ENABLE_DATA_DUMP 1

// =============================================================================
// Erlang Callbacks
// =============================================================================
static ErlDrvData start(ErlDrvPort port, char *command) {
  state *drvstate = (state *)driver_alloc(sizeof(state));

  memset(drvstate, 0, sizeof(state));

  drvstate->drv_port = port;
  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

  LIST_INIT(&drvstate->mq_head);

  P(("Start\n"));

  return (ErlDrvData)drvstate;
}

static void stop(ErlDrvData drvstate) {
  state *st = (state *)drvstate;
  state_queue *q = NULL;

  /* Delete. */
  while (!LIST_EMPTY(&st->mq_head)) {
	  q = (state_queue *) LIST_FIRST(&st->mq_head);

	  if (q->inuse) {
		P(("List in use, desc '%d'\n", q->fd_queue));
	    driver_select(st->drv_port, (ErlDrvEvent)(q->fd_queue), ERL_DRV_READ, 0);

	    mq_close(q->fd_queue);
	  }

	  LIST_REMOVE(q, queues);

	  driver_free(q);
	  q = NULL;
  }

  driver_free(drvstate);

  P(("Stop\n"));
}

static void do_readmq(ErlDrvData drvstate, ErlDrvEvent event) {
  state *st = (state *)drvstate;
  ei_x_buff eixb;
  ssize_t smq = 0;
  char *msg_ptr = NULL;
  size_t msg_len = 0;
  unsigned int msg_prio = 0;
  int rs = 0;
  struct mq_attr attr;

  ei_x_new_with_version(&eixb);

  rs = mq_getattr((mqd_t) event, &attr);
  if ( rs < 0 ) {
	  goto mq_error;
  }

  if ( !(attr.mq_msgsize > 0) ) {
	  goto mq_error;
  }

  msg_len = attr.mq_msgsize;
  msg_ptr = driver_alloc(msg_len);
  if (msg_ptr == NULL) {
	  goto error;
  }

  smq = mq_receive((mqd_t) event, msg_ptr, msg_len, &msg_prio);
  if ( smq < 0 ) {
	  goto mq_error;
  }

  queue_data(&eixb, msg_ptr, smq, msg_prio);

  driver_output(st->drv_port, eixb.buff, eixb.index);

  driver_free(msg_ptr);

  error:
  return;

  mq_error:
  return;
}

static int control(ErlDrvData drvstate, unsigned int command, char *args,
		int argslen, char **rbuf, int rbuflen) {
  state *st = (state *)drvstate;
  init_state(st, args, argslen);

  P(("comando: %d\n", command));

  switch (command) {
  case OPEN_QUEUE_RDONLY: do_open_queue(st, O_RDONLY); break;
  case OPEN_QUEUE_WDONLY: do_open_queue(st, O_WRONLY); break;
  case OPEN_QUEUE_RDWR: do_open_queue(st, O_RDWR); break;

  case CREATE_QUEUE_RDONLY: do_create_queue(st, O_RDONLY | O_CREAT); break;
  case CREATE_QUEUE_WDONLY: do_create_queue(st, O_WRONLY | O_CREAT); break;
  case CREATE_QUEUE_RDWR: do_create_queue(st, O_RDWR | O_CREAT); break;

  case RECEIVE_QUEUE: do_receive_queue(st); break;
  case SEND_QUEUE: do_send_queue(st); break;

  case CLOSE_QUEUE: do_close_queue(st); break;
  case REMOVE_QUEUE: do_remove_queue(st); break;
  case GETATTR_QUEUE: do_getattr_queue(st); break;

  case SELECT_QUEUE: do_initselect_queue(st); break;
  case DESELECT_QUEUE: do_deselect_queue(st); break;

  default: break;
  }

  int rlen = st->eixb.index;
  ErlDrvBinary *response = driver_alloc_binary(rlen);
  memcpy(response->orig_bytes, st->eixb.buff, rlen);
  ei_x_free(&(st->eixb));
  *rbuf = (char *)response;
  return rlen;
}


// =============================================================================
// MQ Posix function wrappers
// ===========================================================================

static void do_initselect_queue(state *st) {

  int arity = 0;
  long qdesc = 0;
  state_queue *q;

  ei_decode_tuple_header(st->args, &(st->index), &arity);
  /* queue desc */
  ei_decode_long(st->args, &(st->index), &qdesc);

  q = insert_next_queue(st, qdesc);

  if (q == NULL) {
    goto error;
  }

  encode_ok_reply(st, 0);

  driver_select(st->drv_port, (ErlDrvEvent)qdesc, DO_READ, 1);

  return;

  error:
  encode_ok_reply(st, -1);
  return;

}

static void do_deselect_queue(state *st) {

  int rs = 0;
  int arity = 0;
  long qdesc = 0;

  ei_decode_tuple_header(st->args, &(st->index), &arity);
  /* queue desc */
  ei_decode_long(st->args, &(st->index), &qdesc);

  rs = remove_next_queue(st, qdesc);

  if (rs < 0) {
    goto error;
  }

  driver_select(st->drv_port, (ErlDrvEvent)qdesc, DO_READ, 0);

  encode_ok_reply(st, 0);

  return;

  error:
  encode_ok_reply(st, -1);
  return;

}

static void do_open_queue(state *st, int flag) {

	char qname[255];
	int rs = 0;
	int arity = 0;
	long isblocking = 0;

	ei_decode_tuple_header(st->args, &(st->index), &arity);
	/* name of queue? */
	ei_decode_string(st->args, &(st->index), qname);
	/* is blocking? */
	ei_decode_long(st->args, &(st->index), &isblocking);

	/* do the job */
	rs = mq_open(qname, flag | isblocking);

	if (rs < 0) {
      goto error_mq;
	}

    encode_ok_queue(st, rs);
    return;

	error_mq:
	encode_ok_reply(st, errno);
	return;
}

static void do_create_queue(state *st, int flag) {

	char qname[255];
	long qmode = 0;
	struct mq_attr qattr;
	int rs = 0;
	int arity = 0;
	long isblocking = 0;

	memset(&qattr, 0, sizeof(qattr));

	/* name isblocking mode nqueue squeue */
	ei_decode_tuple_header(st->args, &(st->index), &arity);
	/* name of queue? */
	ei_decode_string(st->args, &(st->index), qname);
	/* is blocking? */
	ei_decode_long(st->args, &(st->index), &isblocking);
	/* mode? */
	ei_decode_long(st->args, &(st->index), &qmode);
	/* Max. # of messages on queue */
	ei_decode_long(st->args, &(st->index), &qattr.mq_maxmsg);
    /* Max. message size (bytes) */
    ei_decode_long(st->args, &(st->index), &qattr.mq_msgsize);

    P(("criando fila: %s %ld %d %ld %ld %ld\n", qname, isblocking, flag, qmode, qattr.mq_maxmsg, qattr.mq_msgsize));

	/* do the job */
	rs = mq_open(qname, flag | isblocking, qmode, &qattr);

	if (rs < 0) {
      goto error_mq;
	}

	P(("fila '%s' desc '%d'\n", qname, rs));

    encode_ok_queue(st, rs);
    return;

	error_mq:
	encode_ok_reply(st, errno);
	return;
}

static void do_close_queue(state *st) {

	int rs = 0;
	int arity = 0;
	long qdesc = 0;

	ei_decode_tuple_header(st->args, &(st->index), &arity);
	/* queue desc */
	ei_decode_long(st->args, &(st->index), &qdesc);

	rs = mq_close((mqd_t)qdesc);

	if (rs < 0) {
      goto error_mq;
	}

    encode_ok_reply(st, rs);
    return;

	error_mq:
	encode_ok_reply(st, errno);
	return;
}

static void do_remove_queue(state *st) {
	int rs = 0;
	int arity = 0;
	char qname[255];;

	ei_decode_tuple_header(st->args, &(st->index), &arity);
	/* name of queue? */
	ei_decode_string(st->args, &(st->index), qname);

	rs = mq_unlink(qname);

	if (rs < 0) {
      goto error_mq;
	}

    encode_ok_reply(st, rs);
    return;

	error_mq:
	encode_ok_reply(st, errno);
	return;
}

static void do_getattr_queue(state *st) {
	int rs = 0;
	int arity = 0;
	long qdesc = -1;
	struct mq_attr qstat;

	ei_decode_tuple_header(st->args, &(st->index), &arity);
	/* queue desc */
	ei_decode_long(st->args, &(st->index), &qdesc);

	rs = mq_getattr(qdesc, &qstat);

	if (rs < 0) {
      goto error_mq;
	}

    encode_queue_attr(st, qstat.mq_flags, qstat.mq_maxmsg, qstat.mq_msgsize, qstat.mq_curmsgs);
    return;

	error_mq:
	encode_ok_reply(st, errno);
	return;
}

static void do_send_queue(state *st) {

	int arity;
	long qdesc;
	long qprio;
	long qsize;
	long qmsg_size;
	void *qmsg = NULL;
	int rs = 0;
	int type = 0;
	int type_size = 0;

	ei_decode_tuple_header(st->args, &(st->index), &arity);
	ei_decode_long(st->args, &(st->index), &qdesc);
	ei_decode_long(st->args, &(st->index), &qprio);
	ei_decode_long(st->args, &(st->index), &qsize);

	ei_get_type(st->args, &(st->index), &type, &type_size);

	if (type != ERL_BINARY_EXT) {
		goto error;
	}

    qmsg = driver_alloc(type_size);
	if (qmsg == NULL) {
		goto error;
	}

	if (qsize != type_size) {
		P(("Send size differ size '%ld' type_size '%d'\n", qsize, type_size));
	}

	ei_decode_binary(st->args, &(st->index), qmsg, &qmsg_size);

	P(("Enviando %ld %ld %ld\n", qdesc, qmsg_size, qprio));
	data_dump(qmsg, qmsg_size);

	rs = mq_send(qdesc, qmsg, qmsg_size, qprio);
	if ( rs < 0) {
		goto error_mq;
	}

	driver_free(qmsg);

	encode_ok_reply(st, rs);
	return;

	error:
	P(("error: %s:%d\n", __FUNCTION__, __LINE__));
	encode_ok_reply(st, -1);
	return;

	error_mq:
	if (qmsg != NULL) {
	  driver_free(qmsg);
	}
	P(("error: %d %s:%d\n", errno, __FUNCTION__, __LINE__));
	encode_ok_reply(st, errno);
	return;
}

static void do_receive_queue(state *st) {
	ssize_t smq = 0;
	char *msg_ptr = NULL;
	size_t msg_len = 0;
	unsigned int msg_prio = 0;
	int rs = 0;
	struct mq_attr attr;

	int arity = 0;
	long qdesc = -1;

	ei_decode_tuple_header(st->args, &(st->index), &arity);
	/* queue desc */
	ei_decode_long(st->args, &(st->index), &qdesc);

	rs = mq_getattr((mqd_t) qdesc, &attr);
	if ( rs < 0 ) {
		goto mq_error;
	}

	if ( !(attr.mq_msgsize > 0) ) {
		goto mq_error;
	}

	msg_len = attr.mq_msgsize;
	msg_ptr = driver_alloc(msg_len);
	if (msg_ptr == NULL) {
		goto error;
	}

	smq = mq_receive(qdesc, msg_ptr, msg_len, &msg_prio);
	if ( smq < 0 ) {
		goto mq_error;
	}

	queue_data(&(st->eixb), msg_ptr, smq, msg_prio);

	driver_free(msg_ptr);

	return;

	error:
	encode_ok_reply(st, -1);
	return;

	mq_error:
	if (msg_ptr != NULL) {
	  driver_free(msg_ptr);
	}
	encode_ok_reply(st, errno);
	return;
}

// =============================================================================
// Utility functions
// =============================================================================

static void data_dump(intptr_t *p, size_t n)
{
#ifdef ENABLE_DATA_DUMP
#define EOS '\0';

	size_t memsize = n;
	char errmsg[1024];
	unsigned llen = 0;

	errmsg[0] = EOS;

	char *cp = (char *) p;

	fprintf(stderr, "\n------------------- dump begin ------------------\n");
	fprintf(stderr, "sop_erl_dump:\n");
	while (memsize) {
		if (llen >= 16) {
			strcat(errmsg, "\n");
			llen = 0;
			fprintf(stderr, "%s", errmsg);
			errmsg[0] = EOS;
		}
		sprintf(errmsg + strlen(errmsg), " %02X",
				(*cp++) & 0xFF);
		llen++;
		memsize--;
	}

	fprintf(stderr, "%s\n", errmsg);
	fprintf(stderr, "------------------- dump end -------------------\n");
#endif
}

void init_state(state *st, char *args, int argslen) {
  st->index = 0;
  st->version = 0;
  st->args = args;
  st->argslen = argslen;
  ei_decode_version(st->args, &(st->index), &(st->version));
  assert(st->version != 0);
  assert(st->index != 0);
  ei_x_new_with_version(&(st->eixb));
}

static state_queue *insert_next_queue(state *st, int qdesc) {

  state_queue *q = NULL;

  q = driver_alloc(sizeof(state_queue));
  if (q == NULL) {
	  goto error;
  }

  q->fd_queue = qdesc;
  q->inuse = 1;

  LIST_INSERT_HEAD(&st->mq_head, q, queues);

  error:
  return q;
}

static int remove_next_queue(state *st, int qdesc) {

	state_queue *q = NULL;

	LIST_FOREACH(q, &st->mq_head, queues) {

		if (q->inuse) {
			if (q->fd_queue == qdesc) {
				goto found;
			}
		}
	}

	return 0;

	found:
	LIST_REMOVE(q, queues);
	driver_free(q);
	q = NULL;
	return 1;

}

static void ok(state *st) {
  atom(&(st->eixb), "ok", 2);
}

void error_tuple(state *st, int code) {
  tuple(&(st->eixb), 2);
  atom(&(st->eixb), "error", 5);
  integer(&(st->eixb), code);
}

void queue_tuple(state *st, int queue) {
  tuple(&(st->eixb), 2);
  atom(&(st->eixb), "mqd", 3);
  integer(&(st->eixb), queue);
}

void boolean(state *st, int code) {
  if (code == true)
    atom(&(st->eixb),"true",4);
  else
    atom(&(st->eixb),"false",5);
}

static void queue_data(ei_x_buff *eixb, const void *msg, int size, unsigned int prio) {
  ei_x_encode_tuple_header(eixb, 4);

  atom(eixb, "data", 4);

  ei_x_encode_long(eixb, prio);

  ei_x_encode_long(eixb, size);

  ei_x_encode_binary(eixb, msg, size);
}

void tuple(ei_x_buff *eixb, int size) {
  ei_x_encode_tuple_header(eixb, size);
}

void atom(ei_x_buff *eixb, const char *str, int size) {
  ei_x_encode_atom_len(eixb, str, size);
}

void integer(ei_x_buff *eixb, int integer) {
  ei_x_encode_long(eixb, (long)integer);
}

void string(ei_x_buff *eixb, const char *str) {
  ei_x_encode_string(eixb, str);
}

void encode_ok_reply(state *st, int code) {
  if (code == _OK) {
    ok(st);
  } else {
    error_tuple(st, code);
  }
}

static void encode_ok_queue(state *st, int queue) {
    queue_tuple(st, queue);
}

static void encode_queue_attr(state *st, long mq_flags, long mq_maxmsg, long mq_msgsize, long mq_curmsgs) {
	tuple(&(st->eixb), 2);
	atom(&(st->eixb), "attr", 4);
	integer(&(st->eixb), mq_flags);
	integer(&(st->eixb), mq_maxmsg);
	integer(&(st->eixb), mq_msgsize);
	integer(&(st->eixb), mq_curmsgs);
}

#if 0
int findfreewindowslot(state *st) {
  int i;
  for (i = 0; i < _MAXWINDOWS; i++)
    if (st->win[i] == NULL) return i;
  return -1;
}
#endif

// =============================================================================
// Erlang driver_entry Specification
// ===========================================================================
ErlDrvEntry driver_entry = {
  NULL,
  start,
  stop,
  NULL,
  do_readmq,
  NULL,
  "emq_posix",
  NULL,
  NULL,
  control,
  NULL
};

// =============================================================================
// Erlang Driver Name
// =============================================================================
DRIVER_INIT(cecho) {
  return &driver_entry;
}

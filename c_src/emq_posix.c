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
#include <fcntl.h>           /* For O_* constants */
#include <sys/stat.h>        /* For mode constants */
#include <mqueue.h>

#include "emq_posix.h"
#include "emq_posix_commands.h"
#include "erl_driver.h"
#include "erl_interface.h"
#include "ei.h"

/* Queue state structure */
typedef struct {
  int inuse;
  mqd_t fd_queue;
  struct mq_attr attr_queue;
} state_queue;

/* State structure */
typedef struct {
  state_queue queues[_MAXQUEUES+1];
  ei_x_buff eixb;
  char *args;
  int argslen;
  int index;
  int version;
  ErlDrvPort drv_port;
} state;

static void init_state(state *st, char *args, int argslen);
static state_queue *get_next_queue(state *st);
static void set_queue_inuse_state(state_queue *q);
static void ok(state *st);
static void error_tuple(state *st, int code);
static void boolean(state *st, int code);

void tuple(ei_x_buff *eixb, int size);
void atom(ei_x_buff *eixb, const char *str, int size);
void integer(ei_x_buff *eixb, int integer);
void string(ei_x_buff *eixb, const char *str);
void encode_ok_reply(state *st, int code);
int findfreewindowslot(state *st);
void loop_getch(void *arg);

static void do_open_queue(state *st, int flag);

// =============================================================================
// Erlang Callbacks
// =============================================================================
static ErlDrvData start(ErlDrvPort port, char *command) {
  state *drvstate = (state *)driver_alloc(sizeof(state));
  drvstate->drv_port = port;
  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
  memset(&drvstate.queues, 0, sizeof(drvstate.queues));
  return (ErlDrvData)drvstate;
}

static void stop(ErlDrvData drvstate) {
  state *st = (state *)drvstate;
  int i;
  for (i = 0; i < _MAXQUEUES; i++) {
	if (st.queues[i].inuse) {
      driver_select(st->drv_port, (ErlDrvEvent)fileno(st.queues[i].fd_queue), ERL_DRV_READ, 0);
	}
  }
  driver_free(drvstate);
}

static void do_getch(ErlDrvData drvstate, ErlDrvEvent event) {
  state *st = (state *)drvstate;
  ei_x_buff eixb;
  int keycode;
  ei_x_new_with_version(&eixb);
  keycode = getch();
  integer(&eixb, keycode);
  driver_output(st->drv_port, eixb.buff, eixb.index);
}

static int control(ErlDrvData drvstate, unsigned int command, char *args,
		int argslen, char **rbuf, int rbuflen) {
  state *st = (state *)drvstate;
  init_state(st, args, argslen);

  switch (command) {
  case OPEN_QUEUE_RDONLY: do_open_queue(st, O_RDONLY); break;
  case OPEN_QUEUE_WDONLY: do_open_queue(st, O_WDONLY); break;
  case OPEN_QUEUE_RDWR: do_open_queue(st, O_RDWR); break;

  case CREATE_QUEUE_RDONLY: do_create_queue(st, O_RDONLY | O_CREAT); break;
  case CREATE_QUEUE_WDONLY: do_create_queue(st, O_WDONLY | O_CREAT); break;
  case CREATE_QUEUE_RDWR: do_create_queue(st, O_RDWR | O_CREAT); break;

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

void do_initselect(state *st, state_queue *q) {
  st->win[0] = (WINDOW *)initscr();
  driver_select(st->drv_port, (ErlDrvEvent)fileno(stdin), DO_READ, 1);
  if (st->win[0] == NULL) {
    encode_ok_reply(st, -1);
  } else {
    encode_ok_reply(st, 0);
  }
}

static void do_open_queue(state *st, int flag) {

	state_queue *q;
	char qname[255];
	int rs;
	int arity;
	long isblocking = 0;

	ei_decode_tuple_header(st->args, &(st->index), &arity);

	/* name of queue? */
	ei_decode_string(st->args, &(st->index), qname);
	/* is blocking? */
	ei_decode_long(st->args, &(st->index), &isblocking);

	/* do the job */
	q = get_next_queue(st);

	if (q == NULL) {
	  goto error;
	}

	rs = mq_open(qname, flag | isblocking);

	if (rs < 0) {
      goto error_mq;
	}

    q->fd_queue = rs;

    encode_ok_queue(st, rs);
    return;

	error:
	encode_ok_reply(st, _ERROR_QS_ROOM);
	return;

	error_mq:
	set_queue_inuse_state(q);
	encode_ok_reply(st, errno);
	return;

}

void do_create_queue(state *st, int flag) {

	char *qname  = NULL;
	mode_t qmode = 0;
	struct mq_attr qattr;
	struct mq_attr *qattr_aux = NULL;
	int rs;

	rs = mq_open(qname, flag, qmode, qattr_aux);

	if (rs < 0) {

	}

	q = get_next_queue(st);

	q->fd_queue = rs;
	q->inuse = 1;
}

#if 0
void do_endwin(state *st) {
  encode_ok_reply(st, endwin());
}

void do_initscr(state *st) {
  st->win[0] = (WINDOW *)initscr();
  driver_select(st->drv_port, (ErlDrvEvent)fileno(stdin), DO_READ, 1);
  if (st->win[0] == NULL) {
    encode_ok_reply(st, -1);
  } else {
    encode_ok_reply(st, 0);
  }
}

void do_refresh(state *st) {
  encode_ok_reply(st, refresh());
}

void do_cbreak(state *st) {
  encode_ok_reply(st, cbreak());
}
void do_nocbreak(state *st) {
  encode_ok_reply(st, nocbreak());
}

void do_echo(state *st) {
  encode_ok_reply(st, echo());
}

void do_noecho(state *st) {
  encode_ok_reply(st, noecho());
}

void do_addch(state *st) {
  long ch;
  ei_decode_long(st->args, &(st->index), &ch);
  encode_ok_reply(st, addch((chtype)ch));
}

void do_addstr(state *st) {
  int arity;
  long strlen;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &strlen);
  char str[strlen];
  ei_decode_string(st->args, &(st->index), str);
  encode_ok_reply(st, addnstr(str, strlen));
}

void do_move(state *st) {
  int arity;
  long y, x;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &y);
  ei_decode_long(st->args, &(st->index), &x);
  encode_ok_reply(st, move((int)y, (int)x));
}

void do_getyx(state *st) {
  long slot;
  int x, y;
  ei_decode_long(st->args, &(st->index), &slot);
  getyx(st->win[slot], y, x);
  tuple(&(st->eixb), 2);
  integer(&(st->eixb), y);
  integer(&(st->eixb), x);
}

void do_getmaxyx(state *st) {
  long slot;
  int x, y;
  ei_decode_long(st->args, &(st->index), &slot);
  getmaxyx(st->win[slot], y, x);
  tuple(&(st->eixb), 2);
  integer(&(st->eixb), y);
  integer(&(st->eixb), x);
}

void do_curs_set(state *st) {
  long flag;
  ei_decode_long(st->args, &(st->index), &flag);
  curs_set((int)flag);
  ok(st);
}

void do_werase(state *st) {
  long slot;
  ei_decode_long(st->args, &(st->index), &slot);
  encode_ok_reply(st, werase(st->win[slot]));
}

void do_has_colors(state *st) {
  boolean(st, has_colors());
}

void do_start_color(state *st) {
  encode_ok_reply(st, start_color());
}

void do_init_pair(state *st) {
  int arity;
  long pairnum, fcolor, bcolor;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &pairnum);
  ei_decode_long(st->args, &(st->index), &fcolor);
  ei_decode_long(st->args, &(st->index), &bcolor);
  encode_ok_reply(st, init_pair((int)pairnum, (int)fcolor, (int)bcolor));
}

void do_wattron(state *st) {
  int arity;
  long slot, attrs;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &attrs);
  encode_ok_reply(st, wattron(st->win[slot], (int)attrs));
}

void do_wattroff(state *st) {
  int arity;
  long slot, attrs;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &attrs);
  encode_ok_reply(st, wattroff(st->win[slot], (int)attrs));
}

void do_nl(state *st) {
  encode_ok_reply(st, nl());
}

void do_nonl(state *st) {
  encode_ok_reply(st, nonl());
}

void do_scrollok(state *st) {
  int arity;
  int bf;
  long slot;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_boolean(st->args, &(st->index), &bf);
  encode_ok_reply(st, scrollok(st->win[slot], bf));
}

void do_mvaddch(state *st) {
  int arity;
  long y, x, ch;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &y);
  ei_decode_long(st->args, &(st->index), &x);
  ei_decode_long(st->args, &(st->index), &ch);
  encode_ok_reply(st, mvaddch((int)y, (int)x, (chtype)ch));
}

void do_mvaddstr(state *st) {
  int arity;
  long strlen, y, x;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &y);
  ei_decode_long(st->args, &(st->index), &x);
  ei_decode_long(st->args, &(st->index), &strlen);
  char str[strlen];
  ei_decode_string(st->args, &(st->index), str);
  encode_ok_reply(st, mvaddnstr((int)y, (int)x, str, (int)strlen));
}

void do_newwin(state *st) {
  int slot = findfreewindowslot(st);
  if (slot > 0) {
    int arity;
    long height, width, starty, startx;
    ei_decode_tuple_header(st->args, &(st->index), &arity);
    ei_decode_long(st->args, &(st->index), &height);
    ei_decode_long(st->args, &(st->index), &width);
    ei_decode_long(st->args, &(st->index), &starty);
    ei_decode_long(st->args, &(st->index), &startx);
    st->win[slot] = newwin(height, width, starty, startx);
    integer(&(st->eixb), slot);
  } else {
    integer(&(st->eixb), -1);
  }
}

void do_delwin(state *st) {
  long slot;
  ei_decode_long(st->args, &(st->index), &slot);
  if (slot == 0) {
    boolean(st, FALSE);
  } else if (st->win[slot] == NULL) {
    boolean(st, FALSE);
  } else if (st->win[slot] != NULL) {
    delwin(st->win[slot]);
    st->win[slot] = NULL;
    boolean(st, TRUE);
  }
}

void do_wmove(state *st) {
  int arity;
  long slot, y, x;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &y);
  ei_decode_long(st->args, &(st->index), &x);
  encode_ok_reply(st, wmove(st->win[slot], (int)y, (int)x));
}

void do_waddstr(state *st) {
  int arity;
  long slot, strlen;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &strlen);
  char str[strlen];
  ei_decode_string(st->args, &(st->index), str);
  encode_ok_reply(st, waddnstr(st->win[slot], str, strlen));
}

void do_waddch(state *st) {
  int arity;
  long slot;
  char ch = 0;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_char(st->args, &(st->index), &ch);
  encode_ok_reply(st, waddch(st->win[slot], ch));
}

void do_mvwaddstr(state *st) {
  int arity;
  long slot, y, x, strlen;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &y);
  ei_decode_long(st->args, &(st->index), &x);
  ei_decode_long(st->args, &(st->index), &strlen);
  char str[strlen];
  ei_decode_string(st->args, &(st->index), str);
  encode_ok_reply(st, mvwaddnstr(st->win[slot], (int)y, (int)x, str, strlen));
}

void do_mvwaddch(state *st) {
  int arity;
  long slot, y, x;
  char ch = 0;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &y);
  ei_decode_long(st->args, &(st->index), &x);
  ei_decode_char(st->args, &(st->index), &ch);
  encode_ok_reply(st, mvwaddch(st->win[slot], (int)y, (int)x, ch));
}

void do_wrefresh(state *st) {
  long slot;
  ei_decode_long(st->args, &(st->index), &slot);
  encode_ok_reply(st, wrefresh(st->win[slot]));
}

void do_whline(state *st) {
  int arity;
  long slot, ch, max;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &ch);
  ei_decode_long(st->args, &(st->index), &max);
  encode_ok_reply(st, whline(st->win[slot], (chtype)ch, (int)max));
}

void do_wvline(state *st) {
  int arity;
  long slot, ch, max;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &ch);
  ei_decode_long(st->args, &(st->index), &max);
  encode_ok_reply(st, wvline(st->win[slot], (chtype)ch, (int)max));
}

void do_wborder(state *st) {
  int arity;
  long slot, ls, rs, ts, bs, tl, tr, bl, br;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &ls);
  ei_decode_long(st->args, &(st->index), &rs);
  ei_decode_long(st->args, &(st->index), &ts);
  ei_decode_long(st->args, &(st->index), &bs);
  ei_decode_long(st->args, &(st->index), &tl);
  ei_decode_long(st->args, &(st->index), &tr);
  ei_decode_long(st->args, &(st->index), &bl);
  ei_decode_long(st->args, &(st->index), &br);
  encode_ok_reply(st, wborder(st->win[slot], (chtype)ls, (chtype)rs, (chtype)ts,
			      (chtype)bs, (chtype)tl, (chtype)tr, (chtype)bl,
			      (chtype)br));
}

void do_box(state *st) {
  int arity;
  long slot, verch, horch;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_long(st->args, &(st->index), &verch);
  ei_decode_long(st->args, &(st->index), &horch);
  encode_ok_reply(st, box(st->win[slot], (chtype)verch, (chtype)horch));
}

void do_keypad(state *st) {
  int arity, bf;
  long slot;
  ei_decode_tuple_header(st->args, &(st->index), &arity);
  ei_decode_long(st->args, &(st->index), &slot);
  ei_decode_boolean(st->args, &(st->index), &bf);
  encode_ok_reply(st, keypad(st->win[slot], bf));
}
#endif
// =============================================================================
// Utility functions
// =============================================================================
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

static state_queue *get_next_queue(state *st) {

  state_queue *q;

  q->inuse = 1;

  return &st.queues[0];
}

static void set_queue_inuse_state(state_queue *q) {
	memset(q, 0, sizeof(state_queue));
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
  if (code == TRUE)
    atom(&(st->eixb),"true",4);
  else
    atom(&(st->eixb),"false",5);
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

void encode_ok_queue(state *st, int queue) {
    queue_tuple(st, queue);
}


int findfreewindowslot(state *st) {
  int i;
  for (i = 0; i < _MAXWINDOWS; i++)
    if (st->win[i] == NULL) return i;
  return -1;
}

// =============================================================================
// Erlang driver_entry Specification
// ===========================================================================
ErlDrvEntry driver_entry = {
  NULL,
  start,
  stop,
  NULL,
  do_getch,
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

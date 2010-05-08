#! /usr/bin/env python
# -*- coding: utf-8 -*-

# ibus-mode-agent.py --- helper program of iBus client for GNU Emacs
# Copyright (c) 2009 and onwards, S. Irie

# Author: S. Irie
# Maintainer: S. Irie
# Version: 0.0.1.3

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Commentary:

# iBus is a new input method framework under active development
# which is designed to overcome the limitations of SCIM.

# iBus uses D-Bus protocol for communication between the ibus-daemon
# and clients (engines, panel, config tools). Since the components
# run in separate processes there is enhanced modularity and stability.
# Client processes can be loaded, started and stopped independently.
# iBus supports Gtk2 and XIM, and has input method engines for anthy,
# chewing, hangul, m17n, pinyin, rawcode, and large tables. Engines
# and clients can be written in any language with a dbus binding.

# This program is iBus client for GNU Emacs. It is, however,
# not part of official iBus distribution.

# ChangeLog:

# 2010-04-12
#        * Version 0.0.1
#        * Initial experimental version

# Code:

import sys
import glib
import ibus
from ibus import modifier

# D-Bus

bus = ibus.Bus()
path = bus.create_input_context("IBusAgentIMContext")

# Miscellaneous functions

def lisp_boolean(boolean):
    return "t" if boolean else "nil"

# Class definition

class IBusAgentIMContext(ibus.InputContext):

    def __init__(self):
        super(IBusAgentIMContext, self).__init__(bus, path, True)

        self.id_no = 0
        self.lookup_table = None

        self.connect('commit-text', self.__commit_text_cb)
        self.connect('update-preedit-text', self.__update_preedit_text_cb)
        self.connect('show-preedit-text', self.__show_preedit_text_cb)
        self.connect('hide-preedit-text', self.__hide_preedit_text_cb)
        self.connect('update-auxiliary-text', self.__update_auxiliary_text_cb)
        self.connect('show-auxiliary-text', self.__show_auxiliary_text_cb)
        self.connect('hide-auxiliary-text', self.__hide_auxiliary_text_cb)
        self.connect('update-lookup-table', self.__update_lookup_table_cb)
        self.connect('show-lookup-table', self.__show_lookup_table_cb)
        self.connect('hide-lookup-table', self.__hide_lookup_table_cb)
        self.connect('page-up-lookup-table', self.__page_up_lookup_table_cb)
        self.connect('page-down-lookup-table', self.__page_down_lookup_table_cb)
        self.connect('cursor-up-lookup-table', self.__cursor_up_lookup_table_cb)
        self.connect('cursor-down-lookup-table', self.__cursor_down_lookup_table_cb)
        self.connect('enabled', self.__enabled_cb)
        self.connect('disabled', self.__disabled_cb)

    # Callbacks

    def __commit_text_cb(self, ic, text):
        print '(ibus-commit-text-cb %d "%s")'%(ic.id_no, text.text.encode("utf-8"))

    def __update_preedit_text_cb(self, ic, text, cursor_pos, visible):
        attrs = ['%s %d %d %d'%
                 (["nil", "'underline", "'foreground", "'background"][attr.type],
                  attr.value & 0xffffff, attr.start_index, attr.end_index)
                 for attr in text.attributes]
        print '(ibus-update-preedit-text-cb %d "%s" %d %s %s)'% \
            (ic.id_no, text.text.encode("utf-8"),
             cursor_pos, lisp_boolean(visible), ' '.join(attrs))

    def __show_preedit_text_cb(self, ic):
        print '(ibus-show-preedit-text-cb %d)'%(ic.id_no)

    def __hide_preedit_text_cb(self, ic):
        print '(ibus-hide-preedit-text-cb %d)'%(ic.id_no)

    def __update_auxiliary_text_cb(self, ic, text, visible):
        print '(ibus-update-auxiliary-text-cb %d "%s" %s)'% \
            (ic.id_no, text.text.encode("utf-8"), lisp_boolean(visible))

    def __show_auxiliary_text_cb(self, ic):
        print '(ibus-show-auxiliary-text-cb %d)'%(ic.id_no)

    def __hide_auxiliary_text_cb(self, ic):
        print '(ibus-hide-auxiliary-text-cb %d)'%(ic.id_no)

    def __update_lookup_table_cb(self, ic, lookup_table, visible):
        ic.lookup_table = lookup_table
        if visible:
            self.__show_lookup_table_cb(ic)
        else:
            self.__hide_lookup_table_cb(ic)

    def __show_lookup_table_cb(self, ic):
        print "(ibus-show-lookup-table-cb %d '(%s) %s)"% \
            (ic.id_no,
             " ".join(map(lambda item : '"%s"'%item.text,
                          ic.lookup_table.get_candidates_in_current_page())
                      ).encode("utf-8"),
             ic.lookup_table.get_cursor_pos_in_current_page())

    def __hide_lookup_table_cb(self, ic):
        print '(ibus-hide-lookup-table-cb %d)'%(ic.id_no)

    def __page_up_lookup_table_cb(self, ic):
        print '(ibus-log "page up lookup table")'

    def __page_down_lookup_table_cb(self, ic):
        print '(ibus-log "page down lookup table")'

    def __cursor_up_lookup_table_cb(self, ic):
        print '(ibus-log "cursor up lookup table")'

    def __cursor_down_lookup_table_cb(self, ic):
        print '(ibus-log "cursor down lookup table")'

    def __enabled_cb(self, ic):
        print '(ibus-enabled-cb %d "%s")'%(ic.id_no, ic.get_engine().name)

    def __disabled_cb(self, ic):
        print '(ibus-disabled-cb %d)'%ic.id_no


# Process methods from client

imcontexts = []

def create_imcontext():
    ic = IBusAgentIMContext()
    imcontexts.append(ic)
    ic.id_no = len(imcontexts)-1
    ic.set_capabilities(7)
    print "(ibus-create-imcontext-cb %d)"%ic.id_no

def destroy_imcontext(id_no):
    imcontexts[id_no].destroy()
    imcontexts[id_no] = None

def process_key_event(id_no, keyval, keycode, modifier_mask, pressed):
    if pressed:
        mask = modifier_mask
    else:
        mask = modifier_mask & modifier.RELEASE_MASK
    handled = imcontexts[id_no].process_key_event(keyval, keycode, mask)
    print "(ibus-process-key-event-cb %d %s)"%(id_no, lisp_boolean(handled))

def set_cursor_location(id_no, x, y, w, h):
    imcontexts[id_no].set_cursor_location(x, y, w, h)

def focus_in(id_no):
    imcontexts[id_no].focus_in()

def focus_out(id_no):
    imcontexts[id_no].focus_out()

def reset(id_no):
    print ';(ibus-log "reset: %r")'%imcontexts[id_no].reset()

def enable(id_no):
    imcontexts[id_no].enable()

def disable(id_no):
    imcontexts[id_no].disable()

def get_engine(id_no):
    print ';(ibus-log "get_engine: %s")'%imcontexts[id_no].get_engine().name

# Main loop

mainloop = None

def quit_ibus():
    mainloop.quit()

def stdin_cb(fd, condition):
    try:
        exec sys.stdin.readline()
    except:
        import traceback
        print '(error "%s")'%traceback.format_exc().replace('"', '\\"')
    return True

def main():
    global mainloop
    mainloop = glib.MainLoop()
    glib.io_add_watch(0, glib.IO_IN, stdin_cb)
    while True:
        try:
            mainloop.run()
        except:
            import traceback
            print '(error "%s")'%traceback.format_exc().replace('"', '\\"')
        else:
            break
    for ic in imcontexts:
        if ic:
            ic.destroy()
    print '(message "Bye.")'

if __name__ == "__main__":
    main()

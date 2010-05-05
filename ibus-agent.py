#! /usr/bin/env python
# -*- coding: utf-8 -*-

# Version 0.0.1.1

import sys
import glib
import ibus

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
        print "(ibus-show-lookup-table-cb %d '(%s))"% \
            (ic.id_no,
             " ".join(map(lambda item : '"%s"'%item.text,
                          ic.lookup_table.get_candidates_in_current_page())
                      ).encode("utf-8"))

    def __hide_lookup_table_cb(self, ic):
        print '(ibus-hide-lookup-table-cb %d)'%(ic.id_no)

    def __page_up_lookup_table_cb(self, ic):
        pass

    def __page_down_lookup_table_cb(self, ic):
        pass

    def __cursor_up_lookup_table_cb(self, ic):
        pass

    def __cursor_down_lookup_table_cb(self, ic):
        pass

    def __enabled_cb(self, ic):
        print '(set-cursor-color "red")'

    def __disabled_cb(self, ic):
        print "(set-cursor-color (frame-parameter nil 'foreground-color))"


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

def process_key_event(id_no, keyval, keycode, modifier_mask):
    handled = imcontexts[id_no].process_key_event(keyval, keycode, modifier_mask)
    print "(ibus-process-key-event-cb %d %s)"%(id_no, lisp_boolean(handled))

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

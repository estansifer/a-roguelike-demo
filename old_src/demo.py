
import curses
import curses.wrapper

import locale
locale.setlocale(locale.LC_ALL, '')
code = locale.getpreferredencoding()

import level_gen

def do_demo(stdscr):

    h = 60
    w = 150

    l = level_gen.Level(w, h)

    for i in xrange(0, h):
        for j in xrange(0, w):
            stdscr.addstr(i, j, l.tiles[i][j])

    while True:
        c = stdscr.getch()
        if c == ord('q'): break

def main():
    curses.wrapper(do_demo)

main()

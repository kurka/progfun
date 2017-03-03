import curses


def piGenerator():
    q, r, t, k, n, l = 1, 0, 1, 1, 3, 3
    while True:
        if 4*q+r-t < n*t:
            yield n
            nr = 10*(r-n*t)
            n = ((10*(3*q+r))//t)-10*n
            q *= 10
            r = nr
        else:
            nr = (2*q+r)*l
            nn = (q*(7*k)+2+(r*l))//(t*l)
            q *= k
            t *= l
            l += 2
            k += 1
            n = nn
            r = nr


def game(stdscr):
    stdscr.clear()

    stdscr.addstr(0, 0, "Ready? You can start typing how many digits of Pi you want from now. I will stop you when you get it wrong")
    stdscr.move(1,0)
    stdscr.refresh()

    piDigits = piGenerator()
    score = 0
    while True:
        nextDigit = piDigits.__next__()
        nextDigitCode = ord(str(nextDigit))
        guessDigit = stdscr.getch()
        if guessDigit == nextDigitCode:
            score += 1
        else:
            stdscr.addstr(3, 0, "Game Over! Expected: {}, Score: {}".format(nextDigit, score))
            contextGen = piGenerator()
            context = "".join([str(contextGen.__next__()) for _ in range(score+5)])
            stdscr.addstr(4, 0, "Here is some context:")
            stdscr.addstr(5, 0, "{}".format(context))
            break
    # while key != ord('q'):
    #     key = stdscr.getch()
    #     stdscr.addch(20,25,key)
    #     stdscr.refresh()
    #     if key == curses.KEY_UP: 
    #         stdscr.addstr(2, 20, "Up")
    #     elif key == curses.KEY_DOWN: 
    #         stdscr.addstr(3, 20, "Down")


if __name__ == "__main__":
    # init ncurses
    stdscr = curses.initscr()
    curses.cbreak()
    stdscr.keypad(1)

    while True:
        game(stdscr)
        stdscr.addstr(7, 0, "Play again? ")
        ans = stdscr.getch()
        if ans == ord("n") or ans == ord("q"):
            break

    # close ncurses
    curses.endwin()

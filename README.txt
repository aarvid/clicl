Clicl (common lisp(s) in common lisp, pronounced clickle) is a library
that allows the creation of sandboxes to the underlying lisp.  Thus
allowing multiple 3rd parties access to the lisp image.  It
allows for the existence of multiple sandboxes each which could have a
different configuration.  It also allows for the addition of libraries
such as alexandria by the programmer.

Originally this was intended to extended ideas presented in the
sandbox portion of cl-eval-bot.  This is totally new code as
cl-eval-bot is gpl and this is intended to have an extremely liberal
license to encourage others to participate.  Also to achieve what I
desired, I needed to change the approach.

Warning: this code is in development and is definitely pre-pre-alpha.





== What is patty?

It's a small Common Lisp library to facilitate the work with
functional data structures on top of CLOS. For more information
visit the project page at http://common-lisp.net/project/patty/.

== Installing

ASDF is required to load patty. Your CL might bundle it, try:

    (require :asdf)

If this doesn't work, look at http://www.cl-user.net/asp/libs/ASDF.
Then you can load patty via:

    (asdf:oos 'asdf:load-op "patty")

The public API of patty consists of the symbols exported from the
packages "PATTY" and "PATTY.MOP".

== Version numbers

The version consists of the three numbers "major", "minor" and
"patchlevel", separated by a dot.

A change of major means a non backwards compatible change was
introduced.

A change of minor means that new features were introduced, possible
additional symbols exported and new packages introduced. Old
functionality is backwards compatible to all previous versions with
the same major number.

A change of patchlevel means that a bug was fixed or another small
improvement was made. No backwards incompatibility is introduced, no
new symbols exported and no new packages created.

All released versions have an even patchlevel. An odd patchlevel
indicates a development version and no backwards compatibility or
other stability commitments are made.

== Author, Licence

Patty is Copyright (c) 2007, Stefan Lang and is licenced under the
BSD licence. Read the LICENCE.txt file for details.

The files patty/mop/package.lisp, patty/mop/abstract-class.lisp and
patty/mop/singleton-class.lisp are Copyright (c) 2000-2001, Tim
Bradshaw.

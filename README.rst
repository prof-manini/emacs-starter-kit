..  -*- coding: utf-8 -*-

=====================================
Yet another fork of Emacs Starter Kit
=====================================

This is a fork of the original `Emacs Starter Kit`_, more precisely of
`Gabriele Lanaro's`__ branch of it.

Introduction
============

I don't like ``git``, and after a brief period with ``bzr`` I decided
to switch back to ``darcs``, that simple and awesome expecially for
this kind of project, where usually each single user will want to
customize this and that with as little as possible interferences with
whatever upstream is doing, and still being able to record his own
changes and freely exchange those with co-workers. ``darcs`` simply
excels at that.

Installation
============

First of all, you may want to move your current configuration in a
safe place, maybe simply::

  $ cd ~
  $ mv .emacs.d .emacs.d.old

Then fetch a copy of this repository::

  $ darcs get --lazy http://darcs.metapensiero.it/our/lele/emacs-starter-kit/ .emacs.d

The very first time you execute Emacs with the new configuration it
will download further packages from ELPA_; I usually do that from the
command line executing the following two commands in row::

  $ emacs -q -l ~/.emacs.d/init.el
  $ emacs -q -l ~/.emacs.d/init.el -f recompile-init

I recommend redoing the latter whenever seems reasonable, that is
after any configuration tweak you may do.

Java script lint
----------------

If you are going to code in Javascript you'll need to install the
excellent javascriptlint_ tool, written in Python on top of the
spidermonkey engine.

I suggest these further steps::

  $ cd ~/.emacs.d
  $ virtualenv .
  $ source bin/activate

As I'm writing these notes, `SVN revision 302`__ has some quirks, that
I cured in this branch::

  $ cd /tmp
  $ darcs get --lazy http://darcs.metapensiero.it/our/lele/javascriptlint
  $ cd javascriptlint
  $ python setup.py install

I'd expect that in a very short time the upstream will be fixed, and
when that happens it should be installed from sourceforge_ instead::

  $ cd /tmp
  $ svn co https://javascriptlint.svn.sourceforge.net/svnroot/javascriptlint/trunk javascriptlint
  $ cd javascriptlint
  $ python setup.py install

Customization
=============

You may want to create a file named after your own account name, as I
did in ``.emacs.d/lele.el``, where you can put your specific
tweaks. That file will be loaded only by your instances of Emacs. In
particular, it should contain your preferred `user-mail-address`.

.. _emacs starter kit: http://github.com/technomancy/emacs-starter-kit/
__ http://github.com/gabrielelanaro/emacs-starter-kit/
.. _elpa: http://tromey.com/elpa
.. _javascriptlint: http://www.javascriptlint.com/
__ http://javascriptlint.svn.sourceforge.net/viewvc/javascriptlint?view=revision&revision=302
.. _sourceforge: http://sourceforge.net/projects/javascriptlint/

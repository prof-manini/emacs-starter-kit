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

  $ emacs --batch -l ~/.emacs.d/init.el
  $ emacs --batch -l ~/.emacs.d/init.el -f recompile-init

I recommend redoing the latter whenever seems reasonable, that is
after any configuration tweak you may do.

If you prefer, the above instructions are executed more simply by a
single ``make``::

  $ make

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
particular, it should contain your preferred `user-mail-address`. As
this file is loaded very late by ``init.el`` (in particular, after any
``starter-kit-xxx``) it can overwrite any previous configuration
detail.

You can also create a sub directory of ``.emacs.d`` named after your
account, for example I have a ``.emacs.d/lele/`` where I store `GNUS`
details and state. If it exists, that directory is also added to the
Emacs's `load-path`.

If you need to override some module provided either by the
emacs-starter-kit or by your system, or maybe you just wanna evaluate
some elisp package, you can use the ``.emacs.d/overrides/`` sub
directory that is added *in front* of the Emacs's `load-path` so that
it has the highest priority.

nxhtml mode
-----------

nXhtml_ is a set of utilities, expecially useful when authoring web
applications where ``HTML`` pages are usually built with some
*template engine*.

One of the components is in fact ``MuMaMo``, *multi major mode*, on
which (for example) ``mako-nxhtml-mode`` is built.

Another example is ``python-rst-mode`` that may be handy when writing
ReST_ documentation within Python modules and classes.

Since it is too big to be included and I wasn't able to distill a
recipe based on its ``web-vcs`` system, here are the steps I suggest::

  $ cd ~/.emacs.d
  $ bzr branch lp:nxhtml
  $ emacs --batch -l nxhtml/autostart.el -f nxhtmlmaint-byte-recompile

At this point nXhtml is ready to use: you can either manually load it
with ``M-x load-file RET ~/.emacs.d/nxhtml/autostart.elc`` or
automatically load it in every Emacs session::

  $ echo '(load (concat dotfiles-dir "nxhtml/autostart.elc"))' >> your-user-name.el

.. _emacs starter kit: http://github.com/technomancy/emacs-starter-kit/
__ http://github.com/gabrielelanaro/emacs-starter-kit/
.. _elpa: http://tromey.com/elpa
.. _javascriptlint: http://www.javascriptlint.com/
__ http://javascriptlint.svn.sourceforge.net/viewvc/javascriptlint?view=revision&revision=302
.. _sourceforge: http://sourceforge.net/projects/javascriptlint/
.. _nxhtml: http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html
.. _rest: http://docutils.sourceforge.net/

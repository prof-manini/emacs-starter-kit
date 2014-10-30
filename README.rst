..  -*- coding: utf-8 -*-

=====================================
Yet another fork of Emacs Starter Kit
=====================================

This is a fork of the original `Emacs Starter Kit`_, more precisely of
`Gabriele Lanaro's`__ branch of it.

Introduction
============

I don't like ``git``, and after a brief period with ``bzr`` I decided
to switch back to ``darcs``, that is simple and awesome expecially for
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

  $ darcs get --lazy http://hub.darcs.net/lelit/emacs-starter-kit .emacs.d

Eventually tweak the file ``.emacs.d/esk/elpa.el`` adjusting the list
of external packages.

Finally run::

  $ make

The very first execution will download a few packages from ELPA_. It
will also recompile elisp sources as needed.

I recommend redoing the latter whenever seems reasonable, that is
after any configuration tweak you may do.

Python lint
-----------

The ``flymake``-based syntax checker for Python relies on ``pyflakes`` being
present on your system. You can either install it system-wide (for example
installing it with ``apt-get install pyflakes``) or have it inside the
specific virtualenv associated with the `virtual desktop` that gets activated
with ``M-x activate-virtual-desktop`` (by default bound to ``F11``), located
either at the same level or under that in an ``env`` subdirectory::

  $ cd /path/to/project
  $ virtualenv env
  $ source env/bin/activate
  $ pip install -U pyflakes

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
details and state. This directory may contain specific overrides to any
of the customization modules: for example, you can see in
``.emacs.d/lele/elpa.el`` how I augmented the set of external packages
loaded from the ELPA archives. That file gets loaded just after
``.emacs.d/elpa.el``.

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
__ https://github.com/gabrielelanaro/emacs-starter-kit
.. _elpa: http://tromey.com/elpa
.. _nxhtml: http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html
.. _rest: http://docutils.sourceforge.net/

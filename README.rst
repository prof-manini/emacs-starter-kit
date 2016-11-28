.. -*- coding: utf-8 -*-

=====================================
Yet another fork of Emacs Starter Kit
=====================================

This is a fork of the original `Emacs Starter Kit`_, more precisely of `Gabriele Lanaro's`__
branch of it.

__ https://github.com/gabrielelanaro/emacs-starter-kit

Installation
============

First of all, you may want to move your current configuration in a safe place, maybe simply::

  $ cd ~
  $ mv .emacs.d .emacs.d.old

Then fetch a copy of this repository::

  $ git clone --depth 1 https://github.com/lelit/emacs-starter-kit.git .emacs.d

You may want to tweak the file ``.emacs.d/esk/elpa.el`` adjusting the list of external
packages.

Finally run::

  $ make

The very first execution will download a few packages from ELPA_. It will also recompile elisp
sources as needed.

I recommend redoing the latter whenever seems reasonable, that is after any configuration tweak
you may do.

Python lint
-----------

The ``flymake``-based syntax checker for Python relies on ``pyflakes`` being present on your
system. The best option is to install it system-wide, for example installing it with ``apt-get
install pyflakes``. Alternatively you can install it inside the specific virtualenv associated
with the `virtual desktop` that gets activated with ``M-x activate-virtual-desktop`` (by
default bound to ``F11``), located either at the same level or under that in an ``env``
subdirectory::

  $ cd /path/to/project
  $ virtualenv env
  $ source env/bin/activate
  $ pip install -U pyflakes

Python completion
-----------------

Currently I'm using the excellent Jedi_ library for this task. It gets exposed to Emacs thru
its ``jedi-core`` and ``company-jedi`` packages. You can either follow the instruction__ or do
a system-wide installation with ``apt-get install python-jedi`` and then executing a ``python
setup.py install`` inside the ``~/.emacs.d/elpa/jedi-core-xyz/`` subdirectory. In the latter
case you shall adjust the ``jedi:server-command`` setting, as I did in the customization
section of ``~/.emacs.d/lele.el``.

__ http://tkf.github.io/emacs-jedi/latest/#pyinstall

Customization
=============

You may want to create a file named after your own account name, as I did in
``.emacs.d/lele.el``, where you can put your specific tweaks. That file will be loaded only by
your instances of Emacs. In particular, it should contain your preferred
`user-mail-address`. As this file is loaded very late by ``init.el`` (in particular, after any
``starter-kit-xxx``) it can overwrite any previous configuration detail.

You can also create a sub directory of ``.emacs.d`` named after your account, for example I
have a ``.emacs.d/lele/`` where I store `GNUS` details and state. This directory may contain
specific overrides to any of the customization modules: for example, you can see in
``.emacs.d/lele/elpa.el`` how I augmented the set of external packages loaded from the ELPA
archives. That file gets loaded just after ``.emacs.d/elpa.el``.

If you need to override some module provided either by the emacs-starter-kit or by your system,
or maybe you just wanna evaluate some elisp package, you can use the ``.emacs.d/overrides/``
sub directory that is added *in front* of the Emacs's `load-path` so that it has the highest
priority.

.. _emacs starter kit: http://github.com/technomancy/emacs-starter-kit/
.. _elpa: http://tromey.com/elpa
.. _jedi: https://jedi.readthedocs.org/en/latest/

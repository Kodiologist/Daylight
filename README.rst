.. image:: http://i.imgur.com/drZLi6C.png
  :alt: Princess Celestia raising the sun
  :align: center

Daylight is the software package I use to produce `research notebooks, research papers, presentations`__, and `essays`__ in HTML5 and PDF. I use it for pretty much all my documents, but it's especially suited for `open-notebook science`_, hence its name: it casts research into the harsh glare of public scrutiny. It's implemented as an export backend for `Org mode`_. The Emacs Lisp code is supplemented with pre- and post-processors written in Python 3 and a little bit of `Hy`_ and R for helping with Hy and R code blocks.

.. __: http://arfer.net/projects
.. __: http://arfer.net/w

The features that Daylight provides over plain Org include (see ``example/document.org``, ``example/document.html``, and ``example/slideshow.org`` for a demonstration):

- Automatically generated APA-style bibliographies, with linked inline citations and COinS_
- APA-style table and figure references, when the table or figure has a caption
- Production of APA-style manuscripts as PDFs
- Production of slideshows as PDFs (without TeX or an office program)
- Simplification of LaTeX fragments to other Org constructs (when this is possible, and translation to MathML otherwise)
- `Hy`_ support

  - ``daylight.el`` provides ``org-babel-execute:hy`` etc. in lieu of a real ``ox-hy.el``
  - Encapsulation of evaluated code
  - Pretty-printing of objects from dictionaries to numpy arrays to pandas DataFrames
  - Easy creation of matplotlib graphics from code blocks (just specify the  ``file:`` header argument)

- New features for R

  - R code blocks evaluated with ``:results silent`` will not print huge assigned objects to the echo area
  - Color-coded ``TRUE`` and ``FALSE``
  - ``:results graphics`` is implied by ``:file``

- Linkable labels to figures, generated based on the file name
- Custom postprocessors written in Python
- Shortcut link types for the English Wikipedia and DOIs
- Mid-paragraph comments
- Named instead of numbered ``id``\s for sections and footnotes (which ought to be a lot more future-proof)
- Headers with creation and modification dates
- License footers
- More meta tags

There's also a variety of tweaks to existing features, from how R objects are represented as Org tables to wording (e.g., "Table of Contents" is replaced with "Contents").

I've published Daylight more for the sake of making my research reproducible than because I expect other people to use it for their own work. For this reason, no hooks or Custom variables are provided. If you want to change anything, you'll have to edit the code directly. That said, I encourage you to get in contact with me if you're thinking of doing something with Daylight. Then, perhaps, we can make something more generally useful out of it together.

The inclusion of special features for both R and Hy is a reflection of how I used R extensively for data analysis from 2011 to 2014 and have switched over to Hy for new projects starting in 2015.

Installation and use
============================================================

**Warning:** Don't export untrusted Daylight files. At least one feature (POSTPROC blocks) allows arbitrary code execution.

Oh boy, I hope you packed a lunch.

- You'll need Emacs and Org. For Hy, you'll need Hy (hy-mode is nice but not really required). For R code, you'll need R and Emacs Speaks Statistics (ESS). Here's a known-good combination of versions:

  - GNU Emacs 27.1
  - Org 9.4.6
  - Hy 1.0a3+133.gd15b3eb7
  - R 4.0.4
  - ESS 18.10

- Make sure Python 3 can find the ``daylight`` package in Daylight's ``python3-lib`` directory, and make sure Hy can find the ``daylight_hy`` package in Daylight's ``hy-lib`` directory. Also get `Kodhy`_ and make sure Hy can find the ``kodhy`` package.
- Download http://arfer.net/daylight/kodi-bibliography.yaml. Move it to ``$HOME/.daylight/bibliographies/arfer.net:daylight:kodi-bibliography.yaml``. (Daylight identifies bibliographies by URL, but I haven't implemented automatic retrieval of them.)
- Create the directory ``$HOME/.daylight/py-cache``.
- Install quickbib and citematic_coins (from `Citematic`_; the Perl parts of Citematic are not required by Daylight).
- Install `Kodi.R`_ (you must set the R option ``Kodi.R.path`` so R can find it, presumably in your .Rprofile).
- In your .emacs, write::

      (require 'daylight) ; And make sure Emacs can find daylight.el.
      (setq daylight-ess t)

  Also, add R to ``org-babel-load-languages``.

You should now be able to export a Daylight buffer to HTML with ``(daylight-export-to-file "/tmp/daylight-output.html")``.

To produce a PDF in APA style, say ``(daylight-export-to-file "/tmp/daylight-output.html" (list :daylight-apa t))``, open the resulting HTML page in Firefox, and print it to a file. Yes, this system is a little silly considering that Org also supports LaTeX and OpenDocument export. But, having used both LaTeX and LibreOffice's scripting interface extensively in the past, I can attest that the combination of Firefox, HTML, and CSS is much pleasanter to work with.

Similarly, if a Daylight buffer has ``daylight_slideshow:t`` in its ``#+OPTIONS`` line, ``daylight-export-to-file`` will produce an HTML file suitable for printing to a file to create a slideshow. Daylight expects the paper size for slideshows to be 10 inches wide by 7½ inches tall.

The ``example`` directory contains various informal tests. A few tests of R encapsulation can be run by doing ``ess-load-file`` on ``tests.R``. As for ``document.org``, try exporting it or evaluating code blocks in it after ``daylight-hy-load-buffer``\-ing ``setup.hy`` or ``ess-load-file``\-ing ``setup.R``. If you disable syntax highlighting [with, say, ``(daylight-aliasing 'org-html-fontify-code (lambda (code lang) (org-html-encode-plain-text code)) '(daylight-export-to-file "/tmp/daylight-output.html"))``] and ensure your filesystem's modification date for ``document.org`` is set appropriately, the result of exporting ``document.org`` should be identical to ``document.html``.

License
============================================================

This program is copyright 2013–2021 Kodi Arfer. Portions are copyright 2009–2021 Free Software Foundation, Inc.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the `GNU General Public License`_ for more details.

.. _`open-notebook science`: http://en.wikipedia.org/wiki/Open_notebook_science
.. _`Org mode`: http://orgmode.org/
.. _citeproc-py: https://github.com/brechtm/citeproc-py
.. _COinS: http://ocoins.info/
.. _Citematic: https://github.com/Kodiologist/Citematic
.. _Hy: http://hylang.org
.. _Kodhy: https://github.com/Kodiologist/Kodhy
.. _Kodi.R: https://github.com/Kodiologist/Kodi.R
.. _`GNU General Public License`: http://www.gnu.org/licenses/

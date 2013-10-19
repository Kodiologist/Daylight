.. image:: http://i.imgur.com/drZLi6C.png
  :alt: Princess Celestia raising the sun
  :align: center

Daylight is the software package I use to produce `research notebooks, research papers`__, and `essays`__ in HTML5 and PDF. I use it for pretty much all my documents, but it's especially suited for `open-notebook science`_, hence its name: it casts research into the harsh glare of public scrutiny. It's implemented as an export backend for `Org mode`_. The Emacs Lisp code is supplemented with pre- and post-processors written in Python 3 (because citeproc-py_ is written in Python 3) and a little bit of R for helping with R code blocks.

.. __: http://arfer.net/projects
.. __: http://arfer.net/w

The features that Daylight provides over plain Org include (see ``example/document.org`` and ``example/document.html`` for a demonstration):

- Automatically generated APA-style bibliographies, with linked inline citations and COinS_
- APA-style table and figure references, when the table or figure has a caption
- Production of APA-style manuscripts as PDFs
- Simplification of LaTeX fragments to other Org constructs (when this is possible, and translation to MathML otherwise)
- Encapsulation of evaluated R code (so you don't forget to include a dependency, or get surprised by interactions between objects that belong to different projects, or get tripped up by your own .Rprofile)
- R code blocks evaluated with ``:results silent`` will not print huge assigned objects to the echo area
- Streamlined production of graphics from R code blocks

  - ``:results graphics`` is implied by ``:file``
  - No outer ``print(...)`` is needed for ``ggplot2``

- Linkable labels to figures, generated based on the file name
- Shortcut link types for the English Wikipedia and DOIs
- Named instead of numbered ``id``\s for sections (which ought to be a lot more future-proof)
- Headers with creation and modification dates
- License footers
- More meta tags

There's also a variety of tweaks to existing features, from how R objects are represented as Org tables to wording (e.g., "Table of Contents" is replaced with "Contents").

I've published Daylight more for the sake of making my research reproducible than because I expect other people to use it for their own work. For this reason, no hooks or Custom variables are provided. If you want to change anything, you'll have to edit the code directly. That said, I encourage you to get in contact with me if you're thinking of doing something with Daylight. Then, perhaps, we can make something more generally useful out of it together.

Installation and use
============================================================

Oh boy, I hope you packed a lunch.

- You'll need Emacs, Org, Emacs Speaks Statistics (ESS), and R. Here's a known-good combination of versions:

  - GNU Emacs 24.3.1
  - Org 8.2.1
  - ESS 13.09
  - R 3.0.2

- Download http://arfer.net/daylight/kodi-bibliography.yaml. Move it to ``$HOME/daylight/bibliographies/arfer.net:daylight:kodi-bibliography.yaml``. (Daylight identifies bibliographies by URL, but I haven't implemented automatic retrieval of them.)
- Install quickbib and citematic_coins (from `Citematic`_; the Perl parts of Citematic are not required by Daylight).
- Install `Kodi.R`_ (you must set the R option ``Kodi.R.path`` so R can find it, presumably in your .Rprofile).
- In your .emacs, write::

      (require 'daylight) ; And make sure Emacs can find daylight.el.
      (setq daylight-ess t)
      (setq daylight-simplify-math-path ".../daylight_simplify_math.py")
      (setq daylight-bibgen-path ".../daylight_bibgen.py")
      (setq daylight-html-cleanup-path ".../daylight_html_cleanup.py")
      (setq org-export-babel-evaluate nil)
        ; Probably not required, but strongly recommend. If
        ; `org-export-babel-evaluate' is on, exporting can take eons.

  Also, add R (and any other languages you need for the document you want to work with) to ``org-babel-load-languages``.

You should now be able to export a Daylight buffer to HTML with ``(daylight-export-to-file "/tmp/daylight-output.html")``.

To produce a PDF in APA style, say ``(daylight-export-to-file "/tmp/daylight-output.html" (list :daylight-apa t))``, open the resulting HTML page in Firefox, and print it to a file. Yes, this system is a little silly considering that Org also supports LaTeX and OpenDocument export. But, having used both LaTeX and LibreOffice's scripting interface extensively in the past, I can attest that the combination of Firefox, HTML, and CSS is much pleasanter to work with.

The ``example`` directory contains various informal tests. A few tests of R encapsulation can be run by doing ``ess-load-file`` on ``tests.R``. As for ``document.org``, try exporting it or evaluating code blocks in it after ``ess-load-file``\-ing ``setup.R``. If you disable syntax highlighting [with, say, ``(daylight-aliasing 'org-html-fontify-code (lambda (code lang) (org-html-encode-plain-text code)) '(daylight-export-to-file "/tmp/daylight-output.html"))``] and ensure your filesystem's modification date for ``document.org`` is set appropriately, the result of exporting ``document.org`` should be identical to ``document.html``.

License
============================================================

This program is copyright 2013 Kodi Arfer.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the `GNU General Public License`_ for more details.

.. _`open-notebook science`: http://en.wikipedia.org/wiki/Open_notebook_science
.. _`Org mode`: http://orgmode.org/
.. _citeproc-py: https://github.com/brechtm/citeproc-py
.. _COinS: http://ocoins.info/
.. _Citematic: https://github.com/Kodiologist/Citematic
.. _Kodi.R: https://github.com/Kodiologist/Kodi.R
.. _`GNU General Public License`: http://www.gnu.org/licenses/

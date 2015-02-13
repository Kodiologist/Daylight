#!/usr/bin/python3

from sys import stdin, argv
import os.path
import re
from itertools import count
from datetime import datetime, date
from cgi import escape
import json
from daylight.mathml_worker import to_mathml

license_html = {
    'http://creativecommons.org/licenses/by-sa/4.0/':
        'a <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">Creative Commons Attribution-ShareAlike 4.0 International License</a>',
    'http://creativecommons.org/licenses/by-sa/3.0/us/deed.en_US':
        'a <a rel="license" href="http://creativecommons.org/licenses/by-sa/3.0/us/deed.en_US">Creative Commons Attribution-ShareAlike 3.0 United States License</a>'}

stopwords = ('a', 'an', 'and', 'as', 'at', 'but', 'by', 'down', 'for', 'from', 'i', 'in', 'into', 'nor', 'of', 'on', 'onto', 'or', 'over', 'so', 'the', 'till', 'to', 'up', 'via', 'with', 'yet')

def english_list(l): return (
    '' if len(l) == 0 else
    l[0] if len(l) == 1 else
    '{} and {}'.format(*l) if len(l) == 2 else
    '{}, and {}'.format(', '.join(map(str, l[:-1])), l[-1]))
def undo_name_inversion(name):
    return ' '.join(reversed(name.split(', ', 1)))

# --------------------------------------------------

text = stdin.read()
info = json.loads(argv[1])

apa = info.get('daylight-apa') is True
slideshow = info.get('daylight-slideshow') is True

# Remove the class "mcolon", which is needed for intermediate
# processing but not in the output.
text = re.sub(r"\s+class='mcolon'", '', text)

# In APA mode, remove "notapa" blocks.
if apa:
    text = re.sub(r'\n<div class="notapa">\n.+?\n\n</div>\n', '', text, flags = re.DOTALL)
      # Yes, it's only a matter of time before this regex does
      # the wrong thing. Whatever. I'll cross that bridge when I
      # come to it.

# Convert <latexfrag>s and environments to MathML.
text = re.sub(r'<latexfrag>(.+?)</latexfrag>',
    lambda m: to_mathml(m.group(1), delimited = True),
    text,
    flags = re.DOTALL)
text = re.sub(r'^\\begin\{aligned\}$.+?^\\end\{aligned\}$',
    lambda m: to_mathml(m.group(0), delimited = False, block = True),
    text,
    flags = re.DOTALL | re.MULTILINE)

# Add <meta>s for Google Scholar.
# http://scholar.google.com/intl/en-US/scholar/inclusion.html#indexing
# I don't think Google Scholar actually supports dcterms.* (yet),
# but at least it's valid HTML5.
if not apa:
    date_modified = date.fromtimestamp(os.path.getmtime(info['input-file']))
    date_created = None
    if info.get('daylight-date-created'):
        try:
            date_created = datetime.strptime(info['daylight-date-created'], "%d %b %Y").date()
        except ValueError:
          # We failed to parse the creation date. We can still use
          # it in the subtitle, but not in a meta tag.
            pass
    if info.get('daylight-include-meta'):
        text = text.replace('</head>',
            '<link rel="schema.dcterms" href="http://purl.org/dc/terms/">\n' +
            '<meta name="dcterms.title" content="{}">\n'.format(escape(info['title'])) +
                ''.join(['<meta name="dcterms.creator" content="{}">\n'.format(escape(a))
                    for a in info['author'].split('; ')]) +
                ('<meta name="dcterms.created" content="{}">\n'.format(date_created)
                    if date_created else '') +
                '<meta name="dcterms.modified" content="{}">\n'.format(date_modified) +
            '</head>')

# Remove '<meta name="author" …', which is redundant with dcterms.creator.
text = re.sub(r'<meta\s+name="author"[^>]+>', '', text, 1)

# Remove 'align' attributes from <caption>s, which are obsolete in
# HTML5.
text = re.sub(r'(<caption\b[^>]*) align="[^"]+"', r'\1', text)

# Change the text of table and figure references. For each
# reference, if the table or figure has a caption, change the
# link text to something like "Figure 3". Otherwise, change it to
# something like "fig--mona-lisa".
def f(m):
    idd, objnum = '{}--{}'.format(m.group(1), m.group(2)), m.group(3)
    if m.group(1) == 'tab':
        m2 = re.search('<table\s+id="{}">\s+<(\w+)'.format(re.escape(idd)), text)
        if m2.group(1) == 'caption':
            desc = 'Table {}'.format(objnum)
            cls = 'table-ref-pretty'
        else:
            desc = idd
            cls = 'table-ref-id'
    elif m.group(1) == 'fig':
        m2 = re.search('<figure id="{}">(.+?)</figure>'.format(re.escape(idd)), text, re.DOTALL)
        if '<figcaption>' in m2.group(1):
            desc = 'Figure {}'.format(objnum)
            cls = 'figure-ref-pretty'
        else:
            desc = idd
            cls = 'figure-ref-id'
    else:
        raise ValueError("Can't happen")
    return '<a class="{}" href="#{}">{}</a>'.format(cls, idd, desc)
text = re.sub(r'<a href="#(fig|tab)--([^"]+)">(\d+)</a>', f, text)

# Add linkable labels to <figure>s with 'id's.
text = re.sub(r'<figure\s+id="fig--([^"]+)">',
    r'<figure id="fig--\1"><div class="figure-label">'
        r'<a class="figure-label-text" href="#fig--\1">\1</a></div>',
    text)

# Take the ID from empty <a>s that begin paragraphs and put it on
# the paragraph instead.
text = re.sub(r'<p>(\s*)<a id="([^"]+)"></a>\s*',
    r'<p id="\2">\1',
    text)

if apa:
   # Replace the "Introduction" header with the title, and put
   # "Abstract" at the top.
   def f(m):
       global title_html
       title_html = m.group(1)
       return '<h2 id="abstract-header">Abstract</h2>'
   text = re.sub('<h1 class="title">(.+?)</h1>', f, text, count = 1)
   text = re.sub('(<h2 [^>]+>)Introduction</h2>',
       r'\1{}</h2>'.format(title_html),
       text, count = 1)
else:
    # Put the title in a <header> and add the subtitle.
    authors_html = escape(english_list(
        [undo_name_inversion(a) for a in info['author'].split('; ')]))
    if not slideshow and info['daylight-include-meta']:
        subtitle = '<p class="subtitle">{}<br>{}{}{}</p>'.format(
            authors_html,
            'Created {}'.format(info['daylight-date-created'])
                if date_created else '',
            ' • ' if date_created and date_modified != date_created else '',
            'Last modified {}'.format(date_modified.strftime("%d %b %Y").lstrip('0'))
                if date_modified != date_created else '')
    else:
        subtitle = ''
    text = re.sub('<h1 class="title">(.+?)</h1>',
        r'<header><h1 class="title">\1</h1>' + subtitle + '</header>',
        text)

# Use names instead of numbers for section IDs.
sections = {}
def f(m):
   hn, ident, title = m.groups()
   words = [w for
       w in [''.join(c for c in s if c.isalnum())
           for s in title.lower().strip().split()]
       if w != '']
   words[1 : len(words) - 1] = [w
       for w in words[1 : len(words) - 1]
       if w not in stopwords]
   while sum(len(w) for w in words) > 30 and len(words) > 2:
       words.pop()
   basename = 'sec--' + '-'.join(words)
   if basename in sections.values():
       for n in count(2):
           name = basename + str(n)
           if name not in sections.values():
               break
   else:
       name = basename
   sections[ident] = name
   return '<h{} id="{}">{}</h{}>'.format(hn, name, title, hn)
text = re.sub(r'<h(\d) id="(sec-[^"]+)">(.+?)</h\1>',
    f, text, flags = re.DOTALL)
text = re.sub(r'<a href="#(sec-[^"]+)"',
    lambda m: '<a href="#{}"'.format(sections[m.group(1)]),
    text)

if '<div id="footnotes">' in text:
    # Get rid of the redundant "Notes" header.
    text = re.sub(r'<div id="outline-container-sec-\d+" class="outline-2">\s*<h2 id="[^"]+">Notes</h2>\s*<div [^>]+>\s*</div>\s*</div>', '', text)
    # Fix the link in the table of contents.
    text = re.sub(r'<li><a href="[^"]+">Notes(</a></li>\s*</ul>\s*</div>)',
        r'<li><a href="#footnotes">Notes\1',
        text)
    # Replace numbered footnote IDs with names.
    footnotes = {}
    def f(m):
        label, preface, fn_n, rest = m.groups()
        r_id  = 'fnr--' + label
        if fn_n in footnotes:
            footnotes[fn_n]['seen'] += 1
            r_id += '.' + str(footnotes[fn_n]['seen'])
        else:
            footnotes[fn_n] = dict(seen = 1, label = label)
        return '{}<a id="{}"{} href="#fn--{}">'.format(
            preface, escape(r_id), rest, escape(label))
    text = re.sub(r'<footenotelabel fn:([^>]+)>(.*?)<a id="fnr\.(\d+)(?:\.\d+)?"([^>]+?) href="#fn.\d+">',
        f, text)
    text = re.sub(r'<a id="fn.(\d+)"([^>]+?) href="[^"]+">',
        lambda m: '<a id="fn--{0}"{1} href="#fnr--{0}">'.format(
            escape(footnotes[m.group(1)]['label']), m.group(2)),
        text)

if apa:
    # The header must be "Footnotes", not "Notes".
    text = text.replace(
        '<h2 class="footnotes">Notes</h2>',
        '<h2 class="footnotes">Footnotes</h2>')

if apa:
   # Move tables to the end. Make sure there's a line break between
   # "Table N" and the body of each caption.
   tables = []
   def f(m):
       tables.append('\n\n' +
           re.sub('(<span class="table-number">.+?</span>)',
               r'\1<div>', count = 1, string =
           re.sub('</caption>', '</div></caption>', count = 1, string =
           m.group(0))))
       return ''
   text = re.sub(r'<table.+?</table>', f, text, flags = re.DOTALL)
   if tables:
       text = text.replace('</body>', '<h2 id="tables-header">Tables</h2>' +
           ''.join(tables) + '</body>')
   # Remove figures, but move the figure captions to the end.
   figcaptions = []
   def f(m):
       figcaptions.extend(re.findall('<figcaption>(.+?)</figcaption>', m.group(0), flags = re.DOTALL))
       return ''
   text = re.sub(r'<figure.+?</figure>', f, text, flags = re.DOTALL)
   if figcaptions:
       text = text.replace('</body>', '<h2 id="figure-captions-header">Figure Captions</h2>' +
           ''.join('\n\n<p class="moved-figcaption">' + s for s in figcaptions) + '</body>')

# Move the table of contents to just before the first headline.
# (Or, in APA or slideshow mode, just delete it.)
contents = ''
def f(mo):
    global contents
    contents = mo.group(0)
    return ''
text = re.sub(
    '<nav id="table-of-contents">'
        '.+?'
        '<div id="text-table-of-contents">'
        r'\s+<ul>'
        '.+?'
        r'</ul>\s*</div>\s*</nav>',
    f, text, 1, re.DOTALL)
if contents and not (apa or slideshow):
   text = re.sub('<div id="outline-container-sec-1" class="outline-2">',
       lambda mo: contents + mo.group(0),
       text, 1)

# Give a class to all internal links.
def f(m):
    s = m.group(0)
    if not re.search(r'''href=['"]#''', s):
        return s
    if re.search(r'''\bclass=["']''', s):
        return re.sub(r'''\b(class=["'])''', r'\1internal ', s)
    else:
        return re.sub(r'\A<a', '<a class="internal"', s)
text = re.sub(r'<a\b[^>]+>', f, text)

# Add a license footer.
license_url = info.get('daylight-license-url')
if license_url and not apa:
    year_created = (int(re.search('\d\d\d\d', info['daylight-date-created']).group(0))
        if 'daylight-date-created' in info
        else None)
    year_modified = date_modified.year
    rep = '<div id="outline-container-sec-1"' if slideshow else '</body>'
    if info.get('daylight-now-copyright'):
        cyear, centity = info['daylight-now-copyright'].split(' ', 1)
        guts = 'This work was licensed irrevocably under {} in {}. It is now copyright {} {}.'.format(
            license_html[license_url],
            cyear,
            cyear,
            centity)
    else:
        guts = 'This work is licensed under {}. Copyright {} {}.'.format(
            license_html[license_url],
            '{}–{}'.format(year_created, year_modified)
                if year_created and year_created != year_modified
                else year_modified,
            authors_html)
    text = text.replace(rep,
        '<footer id="license-footer"><p>{}</p></footer>'.format(guts) +
        rep)

# Apply any post-processing block defined by the file.
if info['postproc']:
    exec(info['postproc'])
    text = POSTPROC(text)

# Done.
print(text, end = '')

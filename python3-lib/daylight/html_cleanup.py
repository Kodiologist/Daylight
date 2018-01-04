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
    'http://creativecommons.org/licenses/by/4.0/':
        'a <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">Creative Commons Attribution 4.0 International License</a>',
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
apa_onefile = info.get('daylight-apa-onefile') is True
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

# If daylight-citation-meta is provided, add <meta>s for Google
# Scholar.
#
# daylight-citation-meta can be "t" to use just information
# provided by the file itself, or a "[[bib:…]]" link to use
# information from that bibliography entry. In the latter case,
# daylight.bibgen has already replaced the link with
# JSON-formatted bibliographic data so we don't have to open the
# bibliographic database ourselves.
date_modified = date.fromtimestamp(os.path.getmtime(info['input-file']))
date_created_str = info.get('daylight-date-created')
if date_created_str:
    try:
        date_created = datetime.strptime(date_created_str, "%d %b %Y").date()
    except ValueError:
      # We failed to parse the creation date. Probably it's
      # an imprecise date like "Jan 2010".
        date_created = None
if info.get('daylight-citation-meta'):
    text = text.replace('</head>',
        '<meta name="citation_title" content="{}">\n'.format(escape(info['title'])) +
        ''.join(['<meta name="citation_author" content="{}">\n'.format(escape(a))
            for a in info['author'].split('; ')]) +
        '</head>')
    if info['daylight-show-mdate'] and info['daylight-citation-meta'] == 't':
      # Use the full date modified (to the day) for the citation day.
        text = text.replace('</head>',
            '<meta name="citation_date" content="{}">\n'.format(date_modified) +
            '</head>')
    else:
      # Fill in other citation_ tags from the provided bibliographic data.
      # http://wiki.whatwg.org/wiki/MetaExtensions
      # http://www.monperrus.net/martin/accurate+bibliographic+metadata+and+google+scholar
        cm = json.loads(info['daylight-citation-meta'])
        text = text.replace('</head>',
            '{}</head>'.format('\n'.join(
                '<meta name="{}" content="{}">'.format(k, escape(v))
                for k, v in [
                    ('citation_date', '-'.join(cm['issued']['date-parts'][0])),
                    ('citation_journal_title', cm.get('container-title')),
                    ('citation_volume', cm.get('volume')),
                    ('citation_issue', cm.get('issue')),
                    ('citation_firstpage', cm.get('page') and cm['page'].split('–')[0]),
                    ('citation_lastpage', cm.get('page') and cm['page'].split('–')[1])]
                if v)))

# Remove '<meta name="author" …'.
text = re.sub(r'<meta\s+name="author"[^>]+>', '', text, 1)

if apa:
    # Add a class to the body.
    text = re.sub('<body>', '<body class="apa">', text, 1)

# Fix figure numbers, such that only figures with captions get
# numbers, and the numbers are in the order that figures are
# defined.
figures = []
def f(m):
    pre, idd = m.group(1), m.group(2)
    figures.append(idd)
    return '{}{}.'.format(pre, len(figures))
text = re.sub(r'(<figure id="([^"]+)">\s*<p><img[^>]+>\s*</p>\s*<figcaption><span class="figure-number">Figure )\d+\.', f, text)

# Change the text of table and figure references. For each
# reference, if the table or figure has a caption, change the
# link text to something like "Figure 3". Otherwise, change it to
# something like "fig--mona-lisa".
def f(m):
    idd, objnum = '{}--{}'.format(m.group(1), m.group(2)), m.group(3)
    if m.group(1) == 'tab':
        m2 = re.search('<table\s+id="{}"[^>]*>\s+<(\w+)'.format(re.escape(idd)), text)
        if m2.group(1) == 'caption':
            desc = 'Table {}'.format(objnum)
            cls = 'table-ref-pretty'
        else:
            desc = idd
            cls = 'table-ref-id'
    elif m.group(1) == 'fig':
        m2 = re.search('<figure id="{}">(.+?)</figure>'.format(re.escape(idd)), text, re.DOTALL)
        if '<figcaption>' in m2.group(1):
            desc = 'Figure {}'.format(figures.index(idd) + 1)
            cls = 'figure-ref-pretty'
        else:
            desc = idd
            cls = 'figure-ref-id'
    else:
        raise ValueError("Can't happen")
    return '<a class="{}" href="#{}">{}</a>'.format(cls, idd, desc)
text = re.sub(r'<a href="#(fig|tab)--([^"]+)">(\d+)</a>', f, text)

# Add linkable labels to <figure>s with 'id's.
if not (apa or slideshow):
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

if not apa or apa_onefile:
    # Add the subtitle.
    authors_html = escape(english_list(
        [undo_name_inversion(a) for a in info['author'].split('; ')]))
    if not slideshow and info['daylight-include-meta']:
        show_c = bool(date_created_str)
        show_m = info['daylight-show-mdate'] and (
            not date_created or date_modified != date_created)
        subtitle = '<p class="subtitle">{}<br>{}{}{}</p>'.format(
            authors_html,
            'Created {}'.format(date_created_str)
                if show_c else '',
            ' • '
                if show_c and show_m else '',
            'Last modified {}'.format(date_modified.strftime("%d %b %Y").lstrip('0'))
                if show_m else '')
    else:
        subtitle = ''
    if apa_onefile:
        text = re.sub('(<h2 id="abstract-header">)',
            '<h1>' + title_html + '</h1>' + subtitle + r'\1',
            text)
    else:
        text = re.sub('(<h1 class="title">.+?</h1>)',
            r'\1' + subtitle,
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
text = re.sub(r'<h(\d) id="(org[^"]+)">(.+?)</h\1>',
    f, text, flags = re.DOTALL)
text = re.sub(r'<a href="#(org[^"]+)"',
    lambda m: '<a href="#{}"'.format(sections.get(m.group(1), m.group(1))),
    text)

# Remove unnecessary hexadecimal IDs, which create noise in diffs
# of output documents.
text = re.sub(r'<div id="outline-container-[^"]+" (class="outline-[-0-9]+")>',
    r'<div \1>', text)
text = re.sub(r'<div (class="outline-text-[-0-9]+") id="text-[^"]+">',
    r'<div \1>', text)
text = re.sub(r'<a id="[^"]+"></a>References</h2>',
  'References</h2>', text, count = 1)
# But add an ID for the bibliography content.
text = re.sub(r'( id="bibliography".+?class="outline-text-\d+")',
    r'\1 id="text-bibliography"',
    text, count = 1, flags = re.DOTALL)

# Change table alignment classes to their old names.
text = re.sub(r'(<(?:td|th|col) [^>]*class=")org-(left|right)"',
    r'\1\2"', text)

if '<div id="footnotes">' in text:
    # Get rid of the redundant "Notes" header.
    text = re.sub(r'<div class="outline-2">\s*<h2 id="[^"]+">Notes</h2>\s*<div [^>]+>\s*</div>\s*</div>', '', text)
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
    text = re.sub(r'<footenotelabel ([^>]+)>(.*?)<a id="fnr\.(\d+)(?:\.\d+)?"([^>]+?) href="#fn.\d+">',
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
   # Move the figure captions to the end. In apa_onefile mode, the
   # figures go there, too. Otherwise, the figures are removed.
   figs = []
   def f(m):
       if apa_onefile:
           figs.append(m.group(0))
       else:
           figs.extend(re.findall('<figcaption>(.+?)</figcaption>',
               m.group(0), flags = re.DOTALL))
       return ''
   text = re.sub(r'<figure.+?</figure>', f, text, flags = re.DOTALL)
   if figs: text = text.replace('</body>',
       '<h2 id="figure-captions-header">{}</h2>{}</body>'.format(
           'Figures' if apa_onefile else 'Figure Captions',
           ''.join('\n\n' + ('' if apa_onefile else '<p class="moved-figcaption">') + s
               for s in figs)))

# Move the table of contents to just before the first headline.
# (Or, in APA or slideshow mode, or if it only has boilerplate
# sections, just delete it.)
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
section_names = set(re.findall('([^<>]+)</a></li>', contents))
if (contents and not (apa or slideshow) and
       not (section_names <= {'Notes', 'References'})):
   text = re.sub('<div class="outline-2">',
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
            year_created
                if year_created and not info['daylight-show-mdate']
                else '{}–{}'.format(year_created, year_modified)
                if year_created and year_created != year_modified
                else year_modified,
            authors_html)
    text = text.replace(rep,
        '<footer id="license-footer"><p>{}</p></footer>'.format(guts) +
        rep)

# Apply any post-processing block defined by the file.
if info['postproc']:
    lang, _, body = info['postproc'].partition('\n')
    if lang == 'python':
        exec(body)
        text = POSTPROC(text)
    elif lang == 'hy':
        import hy
        POSTPROC = hy.eval(hy.read_str("(fn [text]\n" + body + "\n)"))
        text = POSTPROC(text)
    else:
        raise ValueError("Unknown POSTPROC language " + repr(lang))

# Done.
print(text, end = '')

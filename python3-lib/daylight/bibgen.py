#!/usr/bin/python3

from sys import argv, stdin
from os import environ
from os.path import join, getmtime, exists
import re
from cgi import escape
import pickle, json
import quickbib, citematic_coins

abbreviate_subsequent_authors = 3

def bib_uri_to_local_file(uri):
    return join(environ['HOME'], '.daylight', 'bibliographies',
        re.sub('\A[a-z]+:/+', '', uri).replace('/', ':'))

# ------------------------------------------------------------

def encode_bibref(s):
    s = ''.join(c for c in s if c == '-' or c == ' ' or c.isalnum())
    s = re.sub(' +', '_', s)
    return escape("bibref--" + s)

def digest_citation(text):
    return (
        re.sub(r'`(.+?)`', r'[[m:i][\1]]',
        re.sub(r'(\[\[m:i\]\[[^\]]+)\]\], \[\[m:i\]\[', r'\1, ',
        re.sub(r'(doi:10\.[^ ]+[^ .])', lambda mo:
            '[[{}]]'.format(doi_escape(mo.group(1))),
        text))))

def doi_escape(text):
  # We have to escape square brackets (and then unescape them
  # back in Emacs Lisp) to work around a limitation of how Org-mode
  # parses links.
    return text.replace('[', '\ue006').replace(']', '\ue007')

class org_bib_formatter(object):

    preformat = str

    def Italic(s): return '[[m:i][{}]]'.format(s)
    Oblique = Italic
    Bold = str
    Light = str
    Underline = str
    def Superscript(s): return '^' + s
    def Subscript(s): return '_' + s
    SmallCaps = str

    class Bibliography(str):
        def __new__(cls, items):
            return super().__new__(cls, '\n\n'.join(map(str, items)))

# ------------------------------------------------------------

org = stdin.read()
apa = argv[1] != 'nil'
simplified_bibliography_path = argv[2] if len(argv) > 2 else None

m = re.search('\n#\+DAYLIGHT_BIBLIOGRAPHY: (.+)', org, re.IGNORECASE)
if not m:
    raise ValueError("No #+DAYLIGHT_BIBLIOGRAPHY specified")
bib_url = m.group(1)
bib_path = bib_uri_to_local_file(bib_url)
if not exists(bib_path):
    raise ValueError("Bilbliography {} not present (should be at {})".format(bib_url, bib_path))
bib_pickle_path = bib_path + '.pkl'
  # A pickled cache of the bibliography, which we create
  # and update.

if not exists(bib_pickle_path) or getmtime(bib_path) > getmtime(bib_pickle_path):
    import yaml
    with open(bib_path) as f: old_database = yaml.load(f)
    database = {}
    for item in old_database:
        key = item['KEY'].lower()
        database[key] = item['csl']
        database[key]['id'] = key
    with open(bib_pickle_path, "wb") as f: pickle.dump(database, f)
else:
    with open(bib_pickle_path, "rb") as f: database = pickle.load(f)

bib_lookup = lambda c: database[re.sub(',? &', ',', c.lower())]

# ------------------------------------------------------------

# If daylight_citation_meta has a citation in it, replace
# the link with a JSON dump of our bibliographical data for
# that reference.
org = re.sub(r'(\n#\+daylight_citation_meta: )\[\[bib:(.+?)\]\]',
    lambda mo: mo.group(1) + json.dumps(bib_lookup(mo.group(2))),
    org, count = 1, flags = re.IGNORECASE)

for start, link in re.findall(r'(..)(bibp?:\w\w\S*)', org, re.UNICODE):
    if start != '[[' and not re.match('\w', start[1], re.UNICODE):
        raise ValueError("Unbracketed link: " + link)

citations = re.findall(r'\[\[bibp?:(.+?)\]\]', org)

if simplified_bibliography_path:
    import json
    with open(simplified_bibliography_path, 'w') as o: json.dump(
        {c: bib_lookup(c) for c in set(citations)},
        o,
        sort_keys = True,
        indent = 2,
        separators = (',', ': '))
    exit()

# ------------------------------------------------------------

cites, ids, bibl = quickbib.bib(
    environ['APA_CSL_PATH'],
    [bib_lookup(c) for c in citations],
    formatter = org_bib_formatter,
    return_cites_and_keys = True,
    **(dict() if apa else dict(
        always_include_issue = True,
        include_isbn = True,
        url_after_doi = True,
        publisher_website = False)))

# Strip outer parentheses.
cites = [c[1:-1] for c in cites]

# Fill in the description parts of 'bib' and 'bibp' links.

cite_n = -1
def f(mo):
    if re.search('^# .*\Z', mo.string[:mo.start()], flags = re.MULTILINE):
      # This citation was in a comment, so skip it.
        return mo.group(0)
    global cite_n
    cite_n += 1
    cite = cites[cite_n]
    if (cite in cites[0 : cite_n] and
      # We've cited this before.
            cite.count(',') + 1 >= abbreviate_subsequent_authors):
        # Replace all but the first author with "et al."
        first_author = re.match('[^,]+', cite).group(0)
        date = re.search(', [^,]+\Z', cite).group(0)
        cite = '{} et al.{}'.format(first_author, date)
    if mo.group(3) and "'" in mo.group(3):
      # This citation ends with a possessive mark ("'" or "'s").
      # Move it to before the year.
        cite = re.sub(', [^,]+\Z',
            lambda m: mo.group(3).rstrip() + m.group(0),
            cite,
            count = 1)
    if mo.group(1) == 'p':
      # This citation is inside parentheses.
         if mo.group(3) == ' ':
           # Add a trailing comma.
            cite += ','
    else:
      # This citation is outside of parentheses.
        # Change "&" to "and".
        cite = cite.replace('&', 'and')
        # Put the date in parentheses.
        cite = re.sub(r', ([^,]+)\Z', r' (\1)', cite)
    return '[[bib{}:{}][{}]]{}'.format(
        mo.group(1),
        mo.group(2).lower(),
          # The lower() is needed because quickbib coerces IDs to lowercase.
        cite,
        ' ' if mo.group(3) else '')
org = re.sub(r"\[\[bib(p?):([^\]]+)\]\]( |' |'s )?", f, org)

# Make the bibliography

ref_org = ("\n* References\n:PROPERTIES:\n:CUSTOM_ID: bibliography\n:END:\n" +
    '\n\n'.join(
        '<<{}>> {} @@html:{}@@'.format(
                encode_bibref(key), digest_citation(c), citematic_coins.coins(database[key]))
            for key, c in zip(ids, bibl)) +
    '\n\n')

# If there's a section for footnotes, put ref_org before that.
# Otherwise, put it at the end of the document.

chunks = re.split(r'^(\* .+)', org, flags = re.MULTILINE)
if len(chunks) >= 2 and chunks[-2] == '* Notes':
    print(''.join(chunks[:-2]) + ref_org + chunks[-2] + chunks[-1])
else:
    print(org + ref_org)

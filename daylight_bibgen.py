#!/usr/bin/python3

from sys import stdin
from os import environ
from os.path import join, getmtime, exists
import re
from cgi import escape
import pickle
import quickbib

abbreviate_subsequent_authors = 3

def bib_uri_to_local_file(uri):
    return join(environ['HOME'], '.daylight', 'bibliographies',
        re.sub('\A[a-z]+:/+', '', uri).replace('/', ':'))

# ------------------------------------------------------------

def mergedicts(d1, d2):
    return dict(list(d1.items()) + list(d2.items()))

def encode_bibref(s):
    s = ''.join(c for c in s if c == '-' or c == ' ' or c.isalnum())
    s = re.sub(' +', '_', s)
    return escape("bibref--" + s)

def digest_citation(text):
    return (
        re.sub(r'`(.+?)`', r'¦\1¦',
        re.sub(r'(\[\[itn:[^\]]+)\]\], \[\[itn:', r'\1, ',
        re.sub(r'(doi:10\.[^ ]+[^ .])', r'[[\1]]',
        text))))

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

# ------------------------------------------------------------

citations = re.findall(r'\[\[bibp?:(.+?)\]\]', org)

if not exists(bib_pickle_path) or getmtime(bib_path) > getmtime(bib_pickle_path):
    import yaml
    with open(bib_path) as f: database = yaml.load(f)
    database = {c['KEY']: c['csl'] for c in database}
    with open(bib_pickle_path, "wb") as f: pickle.dump(database, f)
else:
    with open(bib_pickle_path, "rb") as f: database = pickle.load(f)

cites, ids, text = quickbib.bib(environ['APA_CSL_PATH'],
    [mergedicts(database[re.sub(',? &', ',', c)], {'id': c})
         for c in citations],
    formatter = org_bib_formatter,
    return_cites_and_keys = True,
    always_include_issue = True,
    include_isbn = True,
    url_after_doi = True,
    publisher_website = False)

# Strip outer parentheses.
cites = [c[1:-1] for c in cites]

# Fill in the description parts of 'bib' and 'bibp' links.

cite_n = -1
def f(mo):
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
    if mo.group(1) != 'p':
      # This citation is outside of parentheses.
        # Change "&" to "and".
        cite = cite.replace('&', 'and')
        # Put the date in parentheses.
        cite = re.sub(r', ([^,]+)\Z', r' (\1)', cite)
    return '[[bib{}:{}][{}]]'.format(mo.group(1), mo.group(2).lower(), cite)
      # The lower() is needed because quickbib coerces IDs to lowercase.
org = re.sub(r'\[\[bib(p?):(.+?)\]\]', f, org)

# Make the bibliography

ref_org = ("\n* References\n:PROPERTIES:\n:CUSTOM_ID: bibliography\n:END:\n" +
    '\n\n'.join(
        #'#+HTML: <div id="{}"></div>\n{}'
        '<<{}>> {}'.format(encode_bibref(key), digest_citation(c))
            for key, c in zip(ids, text.split('\n\n'))) +
    '\n\n')

# If there's a section for footnotes, put ref_org before that.
# Otherwise, put it at the end of the document.

chunks = re.split(r'^(\* .+)', org, flags = re.MULTILINE)
if len(chunks) >= 2 and chunks[-2] == '* Notes':
    print(''.join(chunks[:-2]) + ref_org + chunks[-2] + chunks[-1])
else:
    print(org + ref_org)
#!/usr/bin/python3

import sys, re, argparse, subprocess
from xml.sax.saxutils import unescape

operators = ('logit', 'ilogit', 'invlogit', 'Bern', 'Unif', 'Normal')
latex_chars = {
    '∞': r'\infty', '~': r'\sim',
    '«': r'\left(', '»': r'\right)'}
delims = [
   dict(begin = r'\$', end = r'\$', block = False),
   dict(begin = r'\\\s*\(', end = r'\\\s*\)\Z', block = False),
   dict(begin = r'\\\s*\[', end = r'\\\s*\]\Z', block = True)]

# ---------------------------------------------------------------

operator_regex = re.compile(
    r'\\({})\b'.format('|'.join(operators)))
char_regex = re.compile(
    '[{}]'.format(''.join(latex_chars.keys())))

def digest_latex(s):
    return char_regex.sub(lambda mo: latex_chars[mo.group(0)],
        operator_regex.sub(r'\\operatorname{\1}',
        re.sub(r'\\t\{(.+?)\}', r'_\\text{\1}', s)))

def to_mathml(s, block = None, delimited = False, xmlns = False):
    s = s.strip()
    if delimited:
        for d in delims:
            if not (re.match(d['begin'], s) and re.search(d['end'] + r'\Z', s)):
               continue
            s = re.sub(r'\A' + d['begin'], '', s, count = 1)
            s = re.sub(d['end'] + r'\Z', '', s, count = 1)
            if block is None:
                block = d['block']
            break
        else:
            raise ValueError('No delimiter found')
    if block is None:
        block = False
    s = subprocess.Popen(
           ["blahtexml", "--mathml"],
           stdin = subprocess.PIPE,
           stdout = subprocess.PIPE).communicate(
        bytes(digest_latex(unescape(s)), 'UTF-8'))[0].decode('UTF-8')
    s = re.sub(r'\A\s*<blahtex>\s*<mathml>\s*<markup>\s*', '', s)
    s = re.sub(r'\s*</markup>\s*</mathml>\s*</blahtex>\s*\Z', '', s)
    return '<math{}{}>{}</math>'.format(
        ' xmlns="http://www.w3.org/1998/Math/MathML"' if xmlns else '',
        ' display="block"' if block else '',
        s)

# ---------------------------------------------------------------

if __name__ == '__main__':
    import argparse
    p = argparse.ArgumentParser()
    p.add_argument('input_file', nargs = '?')
    p.add_argument('output_file', nargs = '?')
    #p.add_argument('--purge-delims', action = 'store_true', default = True)
    #p.add_argument('--xlmns', action = 'store_true', default = True)
    p = p.parse_args()

    if p.input_file:
       with open(p.input_file) as f: inp = f.read()
    else:
       inp = sys.stdin.read()

    output = to_mathml(inp, delimited = True, xmlns = True, block = False)

    print(output, file = sys.stderr)
    if p.output_file:
       with open(p.output_file, 'w') as f: print(output, file = f)
    else:
       print(output)

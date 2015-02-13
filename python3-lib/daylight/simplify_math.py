#!/usr/bin/python3

import sys, re
class Jump(Exception): pass

s = sys.stdin.read()

gex = re.compile('|'.join([
    '\n# .+',
    r'\n#\+begin_src (?:.|\n)+?\n#\+end_src',
    r'(?<!\\)~.+?(?!\\)~',
    r'\$(\w)\$(?!\w)',
    r'\$(\S(?:[^$\n]|\\$)*?[^\\ ])\$(?!\w)',
    r'\\\((.+?)\\\)']))

def f(m):
   if not any(m.groups()):
       return m.group(0)
   s = [x for x in m.groups() if x][0]
   try:
       s = re.sub(r'([_^])([^{])', r'\1{\2}', s)
       def g(m):
          # We raise Jump when we hit a command we can't fake
          # (meaning that we can't simplify this expression).
          x = m.group(0)
          command, arg = m.group(1), m.group(2)
          if command:
              if command == 'text':
                # Ordinary text
                  return arg
              if command == 't':
                # Text subscript
                  return '_{%s}' % arg
              else:
                  raise Jump
          elif x == '\\$':
              return '$'
          elif x == '\\\\':
              return '\\'
          elif len(x) > 1:
              return x
          elif x in '«»':
              raise Jump
          elif x == '-':
              return '−'
          elif x.isalpha():
              return '[[var:' + x + ']]'
          else:
              return x
       return re.sub(r'\\(\w+)(?:\{([^}]*)\})?|\\\$|\\\\|.', g, s)
   except Jump:
       return m.group(0)

print(gex.sub(f, s))

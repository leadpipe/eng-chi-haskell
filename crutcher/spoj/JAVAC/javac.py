#!/usr/bin/env python2.5 -i

import re
import sys

javaIdRe = re.compile(r'([a-z][a-zA-Z]*')
cIdRe = re.compile(r'[a-z][a-z_]*')

def convert(id):
  j = javaIdRe.match(id)
  c = cIdRe.match(id)

  if j:
    print j

  elif c:
    print c

  else:
    raise Exception("Error!")


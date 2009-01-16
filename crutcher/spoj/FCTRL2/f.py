import sys

lines = sys.stdin.readlines()
t = int(lines[0])
ns = map(int, lines[1:t+1])

max_n = max(ns)

f = 1
k = 0
facs = [f]
while k < max_n:
  k = k + 1
  f = f * k
  facs.append(f)

for i in ns:
  print facs[i]



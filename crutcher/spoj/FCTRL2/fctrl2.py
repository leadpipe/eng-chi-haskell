import sys

facs_array = [1]

def fac(i):
  try:
    return facs_array[i]
  except IndexError:
    f = facs_array[-1]
    k = len(facs_array) - 1
    while k < i:
      k = k + 1
      f = f * k
      facs_array.append(f)
    return f

lines = sys.stdin.readlines()
for i in map(int, lines[1:int(lines[0]) + 1]):
  print fac(i)


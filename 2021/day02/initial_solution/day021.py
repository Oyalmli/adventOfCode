import sys
from functools import reduce 

def solve(sub, l):
  hor, dpt, aim = sub
  dir, sn = l
  n = int(sn)
  return (
    (dir == "forward")*[hor + n, dpt + n*aim, aim  ] + 
    (dir == "down")   *[hor    , dpt        , aim+n] + 
    (dir == "up")     *[hor    , dpt        , aim-n] )

inp = [line.split() for line in sys.stdin.readlines()]
hor, dpt, _ = reduce(solve, inp, [0,0,0])
print(hor * dpt)
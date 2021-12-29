import sys
from collections import defaultdict

hTb = {
    '0': "0000",
    '1': "0001",
    '2': "0010",
    '3': "0011",
    '4': "0100",
    '5': "0101",
    '6': "0110",
    '7': "0111",
    '8': "1000",
    '9': "1001",
    'A': "1010",
    'B': "1011",
    'C': "1100",
    'D': "1101",
    'E': "1110",
    'F': "1111",
}

class Node:
    def __init__(self, v,t, nodes):
        self.v = v
        self.t = t
        self.nodes = int(nodes,2) if t == 4 else nodes

    def __str__(self) -> str:
        return f"({self.v}, {self.t}, {self.nodes})"

def pNum(acc, str):
    i = str[0]
    curr = str[1:5]
    rest = str [5:]
    if i == '0':
        return acc + curr, rest
    return pNum(acc + curr, rest)

def p(rest):
    res = []
    while rest:
        if all(map(lambda c: c=='0', rest)) or len(rest) <= 6: break
        v = int(rest[:3], 2)
        t = int(rest[3:6], 2)
        rest = rest[6:]
        if t == 4:
            chunk, rest = pNum("", rest)
        else:
            chunk, rest = pChunk(rest)
        res.append((int(chunk, 2) if t == 4 else chunk))
    return res, rest

def pChunk(str):
    i = str[0]
    chunks = []
    if i == '0':
        l = int(str[1: 16],2)
        c, rs = p(str[16:16+l])
    else:
        l = int(str[1: 12],2)
        rs = str[12:]
        for _ in range(l):
            c, rs = p(rs)
            chunks.append(c)
        c = chunks
    #print(c,rs)
    return c, rs

def main():
    inp = sys.stdin.readline().strip()
    #print(inp)
    rest = ''.join(map(lambda c: hTb[c], inp))
    print(rest)
    #print(p(rest))
    

if __name__ == "__main__":
    main()
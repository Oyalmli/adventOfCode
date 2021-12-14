import sys
from collections import Counter, defaultdict

def makeRuleMap(lines):
    res = defaultdict(str)
    for line in lines:
        (a,b),_,c = line.split()
        res[(a,b)] = c
    return res

def countChars(hist):
    res = defaultdict(int)
    for (_,b), v in hist.items(): 
        res[b] += v
    res = [v for (_,v) in res.items()]
    return max(res) - min(res)

def solve(n, ruleMap, pHist):
    for _ in range(n):
        npHist = pHist.copy()
        for (a,b), v in pHist.items():
            c = ruleMap[(a,b)]
            npHist[(a,b)] -= v
            npHist[(a,c)] += v
            npHist[(c,b)] += v
        pHist = npHist.copy()
    return countChars(pHist)

def main():
    inp = [line.strip() for line in sys.stdin.readlines()]
    pHist = Counter(zip(inp[0], inp[0][1:]))
    ruleMap = makeRuleMap(inp[2:])
    
    print("Part 1:",solve(10, ruleMap, pHist))
    print("Part 2:",solve(40, ruleMap, pHist))

if __name__ == "__main__":
    main()
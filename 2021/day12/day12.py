import sys
from collections import defaultdict
  
class Graph:
  
    def __init__(self, vertices, sizeArr):
        self.V = vertices
        self.sizeArr = sizeArr
        self.graph = defaultdict(list)

    def addEdge(self, u, v):
        self.graph[u].append(v)
        self.graph[v].append(u)
  
    def countPaths(self, u, d, visited, day):
        s=0
        if (self.sizeArr[u] == 0): 
            visited[u] += 1
        if u == d: 
            s+=1
        else:
            s += sum([self.countPaths(i, d, visited, day) for i in self.graph[u] 
                if visited[i] < day and count2(visited) < 2])
        visited[u] -= 1
        return s
  
    def printAllPaths(self, s, d, day):
        visited = [0]*(self.V)
        visited[s] += 3
        return self.countPaths(s, d, visited, day)
  
def count2(ls):
    cnt = 0
    for n in ls:
        if n == 2:
            cnt+=1
    return cnt

locDict = defaultdict(int)
sizeArr = []
paths = []
cnt = 0
for line in sys.stdin.readlines():
    a,b = line.strip().split("-")
    if a in locDict: a = locDict[a]
    else:
        locDict[a] = cnt
        sizeArr.append(2 if a in ["start","end"] else 1 if a[0].isupper() else 0)
        a = cnt
        cnt += 1
        
    if b in locDict: b = locDict[b]
    else:
        locDict[b] = cnt
        sizeArr.append(2 if b in ["start","end"] else 1 if b[0].isupper() else 0)
        b = cnt
        cnt += 1
    paths.append((a,b))

g = Graph(len(locDict),sizeArr)
for a,b in paths:
    g.addEdge(a,b)

print("Part 1:", g.printAllPaths(locDict["start"], locDict["end"], 1))
print("Part 2:", g.printAllPaths(locDict["start"], locDict["end"], 2))
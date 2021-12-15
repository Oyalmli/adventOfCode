import sys
import heapq as hq

INT_MAX = sys.maxsize
deltas = [(0,-1),(1,0),(0,1),(-1,0)]

def inGrid(x, y, grid):
    return 0 <= x < len(grid[0]) and 0 <= y < len(grid)

def minCost(cost, m, n):
    dp = [[INT_MAX for _ in range(n+1)] for _ in range(m+1)]
    visited = [[False for _ in range(n+1)] for _ in range(m+1)]
    dp[0][0] = 0
    pq = []
    hq.heappush(pq,(cost[0][0], 0, 0))
     
    while pq:
        (_,x,y) = hq.heappop(pq)
        if visited[x][y]: continue
        visited[x][y] = True
        for (dx,dy) in deltas:
            next_x, next_y = x + dx, y + dy
            if inGrid(next_x, next_y, cost) and not visited[next_x][next_y]:
                dp[next_x][next_y] = min(dp[next_x][next_y], dp[x][y] + cost[next_x][next_y])
                hq.heappush(pq,(dp[next_x][next_y], next_x, next_y))
    return dp[m][n]

def printGrid(grid):
    for row in grid:
        for cell in row:
            print(f"{cell}", end="")
        print()

def transformGrid(grid):
    h = len(grid)
    w = len(grid[0])
    megaGrid = [[0 for _ in range(w*5)] for _ in range(h*5)]
    for i in range(h):
        for j in range(w):
            for x in range(5):
                for y in range(5):
                    megaGrid[h*x+i][w*y+j] = 1+((grid[i][j]-1+y+x) % 9)
    return megaGrid

def main():
    #Part 1
    grid = [[int(x) for x in row.strip()] for row in sys.stdin.readlines()]
    print("Part 1:", minCost(grid, len(grid[0])-1, len(grid)-1))
    
    #Part 2
    grid = transformGrid(grid)
    print("Part 2:", minCost(grid, len(grid[0])-1, len(grid)-1))

if __name__ == "__main__":
    main()

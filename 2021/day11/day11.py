import sys

def addOne(grid):
    return list(map(lambda row: list(map(lambda c: c+1,row)),grid))

deltas = [(-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1)]

def inGrid(grid, point):
    y,x = point
    return 0 <= y < len(grid) and 0 <= x < len(grid[0])

def printGrid(grid):
    for row in grid:
        for col in row:
            print(col, end="")
        print()

def flatten(t):
    return [item for sublist in t for item in sublist]

def main():
    grid = [[int(c) for c in line.strip()] for line in sys.stdin.readlines()]
    idx = 0
    cnt = 0
    while True:
        if idx == 100:
            print("Part 1:", cnt)
        idx+=1
        grid = addOne(grid)
        q = []
        blinked = [[False for _ in range(10)] for _ in range(10)]
        for y, row in enumerate(grid):
            for x, col in enumerate(row):
                if col > 9:
                    cnt+=1
                    blinked[y][x] = True
                    q.extend([(y+dy, x+dx) for (dy,dx) in deltas if inGrid (grid,(y+dy, x+dx))])
        while q:
            y,x = q.pop()
            grid[y][x] += 1
            if grid[y][x] > 9 and not blinked[y][x]:
                cnt+=1
                blinked[y][x] = True
                q.extend([(y+dy, x+dx) for (dy,dx) in deltas if inGrid (grid,(y+dy, x+dx))])
            
        for y, row in enumerate(grid):
            for x, col in enumerate(row):
                if col > 9: grid[y][x] = 0

        if all(flatten(blinked)):
            break
    print("Part 2:", idx)


if __name__ == "__main__":
    main()
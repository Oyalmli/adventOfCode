import sys

def readPoints(lines):
    points = []
    for idx, line in enumerate(lines):
        if line == '': break
        a,b = map(int, line.split(","))
        points.append((a,b))
    return points, lines[idx+1:]

def readCommands(lines):
    commands = []
    for line in lines:
        _, _, command = line.split() 
        d, pos = command.split("=")
        commands.append((d, int(pos)))
    return commands

def foldX(point, at):
    x,y = point
    return (at - abs(at - x), y)

def foldY(point, at):
    x,y = point
    return (x, at - abs(at - y))


def printPaper(points):
    w,h = wh(points)

    grid = [[' ' for _ in range(w+1)] for _ in range(h+1)]
    for x,y in points:
        grid[y][x] = u"\u2588"
    
    print('\n'.join([''.join(line) for line in grid]))


def wh(points):
    maxX = 0
    maxY = 0
    for x,y in points:
        if x > maxX: maxX = x
        if y > maxY: maxY = y
    return maxX, maxY


def main():
    inp = [line.strip() for line in sys.stdin.readlines()]
    points, restInp = readPoints(inp)
    points = set(points)
    commands = readCommands(restInp)
    showP1 = True
    for direction, pos in commands:
        if direction == "y":
            points = set(map(lambda point: foldY(point, pos), points))
        elif direction == "x":
            points = set(map(lambda point: foldX(point, pos), points))
        if showP1:
            showP1 = False
            print("Part 1:", len(points), "\n")
    
    print("Part 2:\n")
    printPaper(points)
    

if __name__ == "__main__":
    main()
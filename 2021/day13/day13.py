import sys

def readPoints(lines):
    points = []
    for idx, line in enumerate(lines):
        if line == '':
            break
        a, b = map(int, line.split(","))
        points.append((a, b))
    return points, lines[idx+1:]

def readCommands(lines):
    commands = []
    for line in lines:
        _, _, command = line.split()
        d, pos = command.split("=")
        commands.append((d, int(pos)))
    return commands

def foldX(point, at):
    x, y = point
    return (at - abs(at - x), y)

def foldY(point, at):
    x, y = point
    return (x, at - abs(at - y))

def printPaper(points):
    w, h = wh(points)
    grid = [[' ' for _ in range(w+1)] for _ in range(h+1)]
    for x, y in points:
        grid[y][x] = "█"
    print('\n'.join([''.join(line) for line in grid]))

def wh(points):
    maxX, _ = max(points, key=lambda t: t[0])
    _, maxY = max(points, key=lambda t: t[1])
    return maxX, maxY

def doStep(direction, pos, points):
    return set(map(lambda point: foldY(point, pos) if direction == "y" else foldX(point, pos), points))

def main():
    inp = [line.strip() for line in sys.stdin.readlines()]
    points, restInp = readPoints(inp)
    points = set(points)
    commands = readCommands(restInp)

    # Part 1
    direction, pos = commands[0]
    print("Part 1:", len(doStep(direction, pos, points)), "\n")
    
    # Part 2
    for direction, pos in commands:
        points = doStep(direction, pos, points)
    print("Part 2:")
    printPaper(points)


if __name__ == "__main__":
    main()

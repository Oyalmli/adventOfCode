NORTH = [0,1]
SOUTH = [0,-1]
EAST = [1,0]
WEST = [-1,0]


def rotate_left(dir, n):
    if n == 0: return dir
    if dir == NORTH:
        return rotate_left(WEST, n-1)
    elif dir == WEST:
        return rotate_left(SOUTH, n-1)
    elif dir == SOUTH:
        return rotate_left(EAST, n-1)
    else:
        return rotate_left(NORTH, n-1)

def rotate_right(dir, n):
    if n == 0: return dir
    if dir == NORTH:
        return rotate_right(EAST, n-1)
    elif dir == WEST:
        return rotate_right(NORTH, n-1)
    elif dir == SOUTH:
        return rotate_right(WEST, n-1)
    else:
        return rotate_right(SOUTH, n-1)
    

def getInput(path):
    res = [line.strip() for line in open(path)]
    res = [[line[0], int(line[1:])] for line in res]
    return res

def main():
    facing = EAST
    pos = [0,0]
    input = getInput("../input.txt")
    for dir, unit in input:
        print(pos, unit, dir[0], facing)
        if dir == 'F':
            pos[0] += unit * facing[0]
            pos[1] += unit * facing[1]
        elif dir == 'N':
            pos[0] += unit * NORTH[0]
            pos[1] += unit * NORTH[1]
        elif dir == 'S':
            pos[0] += unit * SOUTH[0]
            pos[1] += unit * SOUTH[1]
        elif dir == 'E':
            pos[0] += unit * EAST[0]
            pos[1] += unit * EAST[1]
        elif dir == 'W':
            pos[0] += unit * WEST[0]
            pos[1] += unit * WEST[1]
        elif dir == 'L':
            facing = rotate_left(facing, unit//90)
        else:
            facing = rotate_right(facing, unit//90)
    print(abs(pos[0]) + abs(pos[1]))

if __name__ == '__main__':
    main()

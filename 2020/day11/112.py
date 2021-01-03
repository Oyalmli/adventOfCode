def main():
    board = makeBoard("../input.txt")
    height = len(board)
    width = len(board[0])
    changed = False
    while(not changed):
        board, c = update(board, height, width)
        printBoard(board)
        changed = not c
    print(cnt(board))


def update(grid,height,width):
    newGrid = [x[:] for x in grid]
    changed = False
    for i in range(height):
        for j in range(width):
            if  grid[i][j] == '.':
                continue
            neighbours = sum(map(
                lambda tup: check(j, i, grid, height, width, tup[0], tup[1]),
                    [(0,1),(0,-1),(1,0),(-1,0),(1,1),(-1,1),(1,-1),(-1,-1)]))
            if grid[i][j] == 'L':
                if neighbours == 0:
                    changed = True
                    newGrid[i][j] = '#'
            elif grid[i][j] == '#':
                if neighbours >= 5:
                    changed = True
                    newGrid[i][j] = 'L'
 
    grid[:] = newGrid[:]
    return grid, changed

def inside(x,y,height,width):
    return 0 <= x <= (width-1) and 0 <=y <=(height-1)

def check(x,y,board, W, H, dx, dy):
    posx=x+dx
    posy=y+dy
    while(inside(posx,posy,W,H)):
        if board[posy][posx] == 'L':
            return 0
        if board[posy][posx] == '#':
            return 1
        posx+=dx
        posy+=dy
    return 0

def printBoard(board):
    for x in board:
        for y in x:
            print(y, end='')
        print()

def makeBoard(path):
    return [[c for c in line.strip()] for line in open(path)]

def cnt(board):
    return sum(list([y == '#' for row in board for y in row]))

if __name__ == '__main__':
    main()

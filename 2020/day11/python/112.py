import numpy as np

def update(grid,M,N):
    newGrid = [x[:] for x in grid]
    changed = False
    for i in range(M):
        for j in range(N):
            if  grid[i][j] == '.':
                continue
            neighbours = [
                check_up(j,i,grid,M,N),
                check_down(j,i,grid,M,N),
                check_left(j,i,grid,M,N),
                check_right(j,i,grid,M,N),
                check_up_left(j,i,grid,M,N),
                check_up_right(j,i,grid,M,N),
                check_down_left(j,i,grid,M,N),
                check_down_right(j,i,grid,M,N),
                ]
            neighbours = sum(neighbours)
            #print(grid[i][j], i,j, neighbours)
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


def check_right(x,y,board,M,N):
    for i in range(x+1,N):
        if board[y][i] == 'L':
            return 0
        if board[y][i] == '#':
            return 1
    return 0

def check_left(x,y,board,M,N):
    for i in range(x-1,-1, -1):
        if board[y][i] == 'L':
            return 0
        if board[y][i] == '#':
            return 1
    return 0

def check_down(x,y,board,M,N):
    #print(f"x{x} y{y}, c{board[y][x]}, m{M}")
    f = 0
    for i in range(y+1,M):
        #print(f"y{y}, x{i}")
        if board[i][x] == 'L':
            break
        if board[i][x] == '#':
            f = 1
            break
    return f

def check_up(x,y,board,M,N):
    #print(f"x{x} y{y}, c{board[y][x]}, m{M}")
    f = 0
    yd=y-1
    while(yd>=0):
        #print(f"y{y}, x{i}")
        if board[yd][x] == 'L':
            break
        if board[yd][x] == '#':
            f = 1
            break
        yd-=1
    return f

def check_up_right(x,y,board,M,N):
    #print(f"x{x} y{y}, c{board[y][x]}, m{M}")
    f = 0
    yd=y-1
    xd=x+1
    while(yd>=0 and xd<N):
        #print(f"y{y}, x{i}")
        if board[yd][xd] == 'L':
            break
        if board[yd][xd] == '#':
            f = 1
            break
        yd-=1
        xd+=1
    return f
    
def check_down_right(x,y,board,M,N):
    #print(f"x{x} y{y}, c{board[y][x]}, m{M}")
    f = 0
    yd=y+1
    xd=x+1
    while(yd<M and xd<N):
        #print(f"y{y}, x{i}")
        if board[yd][xd] == 'L':
            break
        if board[yd][xd] == '#':
            f = 1
            break
        yd+=1
        xd+=1
    return f

def check_down_left(x,y,board,M,N):
    #print(f"x{x} y{y}, c{board[y][x]}, m{M}")
    f = 0
    xd=x-1
    yd=y+1
    while(yd<M and xd>=0):
        #print(f"y{y}, x{i}")
        if board[yd][xd] == 'L':
            break
        if board[yd][xd] == '#':
            f = 1
            break
        yd+=1
        xd-=1
    return f

def check_up_left(x,y,board,M,N):
    #print(f"x{x} y{y}, c{board[y][x]}, m{M}")
    f = 0
    xd=x-1
    yd=y-1
    while(yd>=0 and xd>=0):
        #print(f"y{y}, x{i}")
        if board[yd][xd] == 'L':
            break
        if board[yd][xd] == '#':
            f = 1
            break
        yd-=1
        xd-=1
    return f

def makeBoard(path):
    return [[c for c in line.strip()] for line in open(path)]


def printBoard(board):
    for x in board:
        for y in x:
            print(y, end='')
        print()


def cnt(board):
    count = 0
    for x in board:
        for y in x:
            if y == '#':
                count += 1
    return count


def main():
    board = makeBoard("../input.txt")
    dim = (len(board), len(board[0]))

    changed = False
    printBoard(board)
    while(not changed):
        board, c = update(board, dim[0], dim[1])
        changed = not c
    print(cnt(board))
    print(dim)

if __name__ == '__main__':
    main()

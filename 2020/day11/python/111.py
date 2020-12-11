import numpy as np

def update(grid,M,N):
    newGrid = [x[:] for x in grid]
    changed = False
    for i in range(M):
        for j in range(N):
            
            minRow=0
            if i>1: minRow=i-1
            maxRow=M-1
            if i<M-2: maxRow=i+1
            minCol=0
            if j>1: minCol=j-1
            maxCol=N-1
            if j<N-2: maxCol=j+1
            
            neighbours = 0-1 if grid[i][j] == "#" else 0
            for nrow in range(minRow,maxRow+1):
                for ncol in range(minCol,maxCol+1):
                    if grid[nrow][ncol] == '#':
                        neighbours += 1

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
    update(board, dim[0], dim[1])

    while(not changed):
        board, c = update(board, dim[0], dim[1])
        changed = not c
        #print(changed)
        printBoard(board)
        print('')
        
    print(cnt(board))
    print(dim)

if __name__ == '__main__':
    main()

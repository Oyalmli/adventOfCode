main = interact $ show . map parse . lines 



parse (x:xs) = (x, (read::String->Int) xs)
{-
def r_left(wp_pos, n):
    if n == 0: return wp_pos
    return r_left([-wp_pos[1], wp_pos[0]], n-1)

def r_right(wp_pos, n):
    if n == 0: return wp_pos
    return r_right([wp_pos[1], -wp_pos[0]], n-1)

def getInput(path):
    res = [line.strip() for line in open(path)]
    return [[line[0], int(line[1:])] for line in res]

def dist(pos):
    return abs(pos[0]) + abs(pos[1])

def main():
    boat_pos = [0,0]
    wp_pos = [10,1]
    input = getInput("../input.txt")
    for dir, amount in input:
        print(boat_pos, wp_pos, dist(boat_pos))
        if dir == 'F':
            boat_pos[0] += amount * wp_pos[0]
            boat_pos[1] += amount * wp_pos[1]
        elif dir == 'N':
            wp_pos[1] += amount
        elif dir == 'S':
            wp_pos[1] -= amount
        elif dir == 'E':
            wp_pos[0] += amount
        elif dir == 'W':
            wp_pos[0] -= amount
        elif dir == 'L':
            wp_pos = r_left(wp_pos, amount//90)
        else:
            wp_pos = r_right(wp_pos, amount//90)
    print(dist(boat_pos))

if __name__ == '__main__':
    main()
-}
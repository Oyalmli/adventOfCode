import sys
import json

def traverse(n, pair):
    l, r = pair
    print(type(l), type(r))
    if (type(l) == int):
        print(n, l)
    else:
        traverse(n+1, l)
    if (type(r) == int):
        print(n, r)
    else:
        traverse(n+1, r)

def main():
    inp = [json.loads(line.strip()) for line in sys.stdin.readlines()]
    print(*inp)
    whole = []
    for line in inp:
        traverse(0, line)


if __name__ == "__main__":
    main()
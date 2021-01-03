import json
from functools import reduce


def parse(line):
    words = line.split()
    name = "".join(words[:2])
    elems = split_bags(words[4:])
    return (name, elems)


def divide_chunks(l, n):
    for i in range(0, len(l), n):
        yield l[i:i + n]


def split_bags(word_list):
    bags = [(bag[0], bag[1] + bag[2]) if bag[0] != "no" else (0, "empty") for bag in divide_chunks(word_list, 4)]
    return bags


def pretty_print_dict(di):
    print(json.dumps(di, indent=4))


def search(n, name, di):
    if name == "shinygold":
        return True
    if name == "empty":
        return False
    return any(map(lambda tup: search(tup[0], tup[1], di), di[name]))


def main():
    bags = []
    for line in open("input.txt"):
        bags.append(parse(line))
    di = dict(bags)
    res = []
    for name in di:
        res.append(list(map(lambda tup: search(tup[0], tup[1], di), di[name])))
    print(di)
    print(sum(list(map(any, res))))


if _name_ == "_main_":
    main()
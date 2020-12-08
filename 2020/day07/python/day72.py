def parse(line):
    words = line.split()
    name = "".join(words[:2])
    elems = split_bags(words[4:])
    return (name, elems)


def divide_chunks(l, n):
    for i in range(0, len(l), n):
        yield l[i:i + n]


def split_bags(word_list):
    return [(bag[0], bag[1] + bag[2])
            if bag[0] != "no"
            else (0, "empty")
            for bag in divide_chunks(word_list, 4)]


def search(n, name, di):
    if name == "empty": return 0
    return n + sum(map(lambda tup: search(int(tup[0]), tup[1], di), di[name])) * n


def main():
    di = dict( [parse(line) for line in open("input.txt")])
    di["empty"] = (0, "")
    res = sum(map(lambda tup: search(int(tup[0]), tup[1], di), di["shinygold"]))
    print(res)


if _name_ == "_main_":
    main()
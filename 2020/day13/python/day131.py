def main():
    input = [line.strip() for line in open("../input.txt")]
    target = int(input[0])
    nums = [int(x) for x in input[1].split(',') if x != 'x']
    print (target, nums)
    
    deltas = []
    for num in nums:
        min = target
        for x in range(0,target+num,num):
            if 0 <= x- target < min:
                min = x- target
        deltas.append((min,(min * num)))

    deltas.sort(key=lambda x:x[0])
    print(deltas[0])

if __name__ == '__main__':
    main()
import time

startTime = time.time()
input = [int(line) for line in open("../input.txt")]
input.sort()
input = [0] + input + [input[-1]+3]

between = [input[i+1]-input[i] for i in range(len(input)-1)]

total = 1
count = -1
for n in between:
    if n == 1:
        count += 1
    elif count > 0:
        total *= 1 + (count * (count + 1) // 2)
        count = -1
    else:
        count = -1

endTime = time.time()
print(total, f"{(endTime-startTime)*1000000:0.0f}μs")
import sys

def gaussSum(n,a,l):
    return (n*(a+l)//2)

def doStep(x_vel,y_vel,x_ran,y_ran):
    x_pos = 0
    y_pos = 0
    max_y = 0
    wasInside = False
    while x_pos < x_ran[1] and y_pos >= y_ran[0]:
        x_pos += x_vel
        y_pos += y_vel
        if y_pos > max_y:
            max_y = y_pos
        if x_ran[0] <= x_pos <= x_ran[1] and y_ran[0] <= y_pos <= y_ran[1]:
            wasInside = True
            break
        
        if x_vel > 0:
            x_vel -=1
        y_vel -= 1

    return max_y, wasInside


def main():
    inp = sys.stdin.readline().split()
    x = inp[2]
    y = inp[3]
    x_ran = [int(n) for n in x[2:-1].split("..")]
    y_ran = [int(n) for n in y[2:].split("..")]

    possibleSteps = set()
    for x in range(15,216): #0 216
        for y in range(-132,10000): #-132 10000
            max_y, inside = doStep(x,y,x_ran,y_ran)
            if inside:
                possibleSteps.add((x,y))
    
    print(len(possibleSteps))


if __name__ == "__main__":
    main()

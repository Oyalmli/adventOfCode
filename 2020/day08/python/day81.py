ops = [line.strip() for line in open("../input.txt")]
ops = [[False, line[:3], line[3:]] for line in ops]
#print(ops)

pc = 0
acc = 0
while(True):
    curr_op = ops[pc]
    
    visited, op_name, val = curr_op
    if visited : break
    ops[pc][0] = True
    pc += 1
    if op_name == "nop": continue
    if op_name == "acc": acc += int(val)
    if op_name == "jmp": 
        pc += int(val)-1
        continue
    
print(acc)
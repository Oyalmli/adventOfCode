def main():
    opss = make_all_ops_combinations()
    for ops in opss:
        run_program(ops)

def run_program(ops):
    pc = 0
    acc = 0
    while(True):
        if pc >= len(ops):
            print("Finished!", acc)
            break
        if pc < 0: break
        visited, op_name, val =ops[pc]
        if visited : break
        
        ops[pc][0] = True
        pc += 1
        if op_name == "nop": continue
        if op_name == "acc": acc += int(val)
        if op_name == "jmp": 
            pc += int(val)-1
            continue
    return acc

def make_all_ops_combinations():
    ops = [line.strip() for line in open("../input.txt")]
    ops = [[False, line[:3], line[3:]] for line in ops]
    opss = [ops]
    for idx in range(len(ops)):
        new_ops = [x[:] for x in ops]
        curr_op = ops[idx]
        _, op_name, _ = curr_op
        if op_name == "acc": continue
        if op_name == "jmp": new_ops[idx][1] = "nop"
        if op_name == "nop": new_ops[idx][1] = "jmp"
        opss.append(new_ops)
    return opss

if __name__ == "__main__":
    main()
    

def endOfSubPkt(subPkts):
    typeId = int(subPkts[3:6], 2)
    end = 6 # header length
    if typeId == 4:
        subPkt = subPkts[6:]
        for i in range(0, len(subPkt), 5):
            if subPkt[i] == '0':
                end += 5
                return end
            else:
                end += 5
    else:
        lenTypeId = subPkts[6]
        end += 1
        if lenTypeId == '0':
            end += 15
            lenSubPkts = int(subPkts[7:end], 2)
            end += lenSubPkts
            return end
        else:
            end += 11
            noSubPkts = int(subPkts[7:end], 2)
            for i in range(0, noSubPkts):
                end += endOfSubPkt(subPkts[end:])
            return end

    return -1

def openPacket(packet):
    result = 0
    typeId = int(packet[3:6], 2)
    res = []

    if typeId == 4: # packet is literal value
        pos = 6
        cont = '1'
        res = ''
        while cont == '1':
            res += packet[pos+1:pos+5]
            cont = packet[pos]
            pos += 5
        return [int(res, 2)]
    else: # for now: all other packets
        lenTypeId = packet[6]
        if lenTypeId == '0':
            lenSubPkts = int(packet[7:22], 2)
            pos = 22
            subPkts = packet[pos:pos + lenSubPkts]
            while len(subPkts) > 0 and int(subPkts, 2) != 0:
                end = endOfSubPkt(subPkts)
                res.extend(openPacket(subPkts[:end]))
                subPkts = subPkts[end:]
        else:
            noSubPkts = int(packet[7:18], 2)
            pos = 18
            subPkts = packet[pos:]
            for i in range(0, noSubPkts):
                end = endOfSubPkt(subPkts)
                res.extend(openPacket(subPkts[:end]))
                subPkts = subPkts[end:]
                if len(subPkts) == 0:
                    break
            
    if typeId == 0:
        return [sum(res)]
    elif typeId == 1:
        product = 1
        for x in res:
            product = product * x
        return [product]
    elif typeId == 2:
        return [min(res)]
    elif typeId == 3:
        return [max(res)]
    elif typeId == 5:
        return [1] if res[0] > res[1] else [0]
    elif typeId == 6:
        return [1] if res[0] < res[1] else [0]
    elif typeId == 7:
        return [1] if res[0] == res[1] else [0]
    return res
    
def aoc16(filename):
    inStr = ''
    with open(filename, 'r') as f:
        for line in f.readlines():
            inStr += line.strip()

    binNo = ''
    for x in inStr:
        binNo += bin(int(x, 16))[2:].zfill(4)
    result = openPacket(binNo)
    print(result)

if __name__ == '__main__':
    aoc16('day16.txt')

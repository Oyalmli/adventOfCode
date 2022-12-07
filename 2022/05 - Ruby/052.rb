require 'enumerator'
lines = ARGF.read.split("\n")

arr = [
    "NWFRZSMD", 
    "SGQPW", 
    "CJNFQVRW",
    "LDGCPZF",
    "SPT",
    "LRWFDH",
    "CDNZ",
    "QJSVFRNW",
    "VWZGSMR"
]

lines[10..].each { |line|
    n,x,y = line.scan(/\d+/).map { |d| d.to_i }
    x-=1; y-=1
    
    arr[y] = arr[x][0..(n-1)] + arr[y]
    arr[x] = arr[x][n..]
}

arr.each { |c| print c[0] }
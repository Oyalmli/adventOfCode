(lambda i:print(min(map(lambda x:sum(map(lambda a:(abs(a-x)*(abs(a-x)+1))//2,i)),range(max(i))))))([*map(int,input().split(","))])
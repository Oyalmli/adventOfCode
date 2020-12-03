import re;print(sum([(s[int(n)-1]==x)^(s[int(m)-1]==x)for n,m,(x,_),s in[re.split("[\- ]",l)for l in open("x")]]))

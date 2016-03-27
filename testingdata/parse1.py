files = ["tomcat3.3" ,"tomcat4.1","tomcat5" ,"firefox2" ,"firefox3" ,"firefox4" ,"libre3" ,"libre4" ,"freebsd2" ,"freebsd3" ,"freebsd4" ,"eclipse1" ,"eclipse2.0", "eclipse2.1"]

def parse(name):
    f = open(name+"-result.csv","r")
    g = f.readlines()
    a = [name]
    for t in g[1:]:
        l = t.split(' ')
        if l[1]!="gpd":
            a.append(l[4])
    return a
    f.close()

f = open("rrse.csv","w")
for i in files:
    a = parse(i)
    f.write(",".join(a))
    f.write('\n')
f.close()
    
    

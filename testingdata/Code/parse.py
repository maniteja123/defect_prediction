o = open("tomcat3.3-1.csv","w")
v = ['"1.0', '"1.5.0.x', '"10', '"11', '"12', '"13', '"14', '"15', '"16', '"17', '"18', '"19', '"2.0', '"20', '"21', '"22', '"23', '"24', '"25', '"26', '"27', '"28', '"29', '"3.0', '"3.5', '"3.6', '"30', '"31', '"32', '"33', '"34', '"35', '"36', '"37', '"38', '"39', '"4.0', '"40', '"41', '"42', '"43', '"44', '"5', '"6', '"7', '"8', '"9', '"Trunk"', '"unspecified"']
vv = ['"15 Branch"\n', '"22 Branch"\n', '"2.0 Branch"\n', '"33 Branch"\n', '"35 Branch"\n', '"37 Branch"\n', '"3.6 Branch"\n', '"29 Branch"\n', '"31 Branch"\n', '"1.0 Branch"\n', '"12 Branch"\n', '"21 Branch"\n', '"8 Branch"\n', '"unspecified"\n', '"17 Branch"\n', '"38 Branch"\n', '"40 Branch"\n', '"3.5 Branch"\n', '"27 Branch"\n', '"44 Branch"\n', '"43 Branch"\n', '"Trunk"\n', '"18 Branch"\n', '"19 Branch"\n', '"36 Branch"\n', '"23 Branch"\n', '"11 Branch"\n', '"5 Branch"\n', '"32 Branch"\n', '"20 Branch"\n', '"26 Branch"\n', '"24 Branch"\n', '"42 Branch"\n', '"28 Branch"\n', '"9 Branch"\n', '"30 Branch"\n', '"13 Branch"\n', '"41 Branch"\n', '"16 Branch"\n', '"6 Branch"\n', '"Trunk"', '"39 Branch"\n', '"34 Branch"\n', '"14 Branch"\n', '"3.0 Branch"\n', '"7 Branch"\n', '"25 Branch"\n', '"4.0 Branch"\n', '"10 Branch"\n', '"1.5.0.x Branch"\n']
with open("tomcat 3.3.csv","r") as f:
    g = f.readlines()
    a = set()
    t = g[0].split()[
    o.write(g[0])
    for t in g[1:]:
        l = t.split(',')
        print l
        #a.add(l[-1])
        #if l[-1][1:3] == '5.':
        #    o.write(t)
    print a
o.close()

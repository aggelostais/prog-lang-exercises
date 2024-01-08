import sys
import numpy
from collections import deque

d=dict([('A',0),('C',1),('G',2),('U',3)]) #dictionary of acgu matrix

def complement(list_1):
    temp=list(list_1)
    for i in range(0, len(temp)):
        if temp[i]=='A':
            temp[i]='U'
        elif temp[i]=='U':
            temp[i]='A'
        elif temp[i]=='C': 
            temp[i]='G'
        else: temp[i]='C'
    return temp

class state:
    def __init__(self, first, second,acgu,moves):
        self.first = tuple(first)
        self.second = tuple(second)
        self.acgu = tuple(acgu)
        self.moves = tuple(moves)

# Parafei oles tis nomimes katastaseis pou einai prosvasimes apo auti
    def accessible(self):
        
        # C-move
        if ((len(self.moves)==1 and self.moves[-1]!='c') # complement shouldn't be first move
            or (len(self.moves)>=2 and (self.moves[-1]!='c') and (self.moves[-1]=='p' or self.moves[-2]=='p'))):
            yield state(complement(self.first),self.second,self.acgu,self.moves+('c',))
        
        # P-move
        if (self.second==() # In the begining we can do p move
        or (self.first[-1]==self.second[-1]) # letter is the same with last one
        or (self.first[-1]=='A' and self.acgu[0]==False) # letter has not been used
        or (self.first[-1]=='C' and self.acgu[1]==False) # ->>-
        or (self.first[-1]=='G' and self.acgu[2]==False)
        or (self.first[-1]=='U' and self.acgu[3]==False)):
            yield state(self.first[0:-1] , self.second+(self.first[-1],) , self.acgu[0:d[self.first[-1]]]+(True,)+self.acgu[(d[self.first[-1]]+1):] ,self.moves+('p',))
        
        # R-move
        if (len(self.second)>=2 #second queue must have at least 2 elements 
        and (self.moves[-1]!='r') and (self.moves[-1]=='p' or self.moves[-2]=='p')):
            yield state(self.first,(self.second[-1],)+self.second[1:-1]+(self.second[0],),self.acgu,self.moves+('r',))

    # Typonei mia katastasi
    #def __str__(self):
    #    return "First Queue: {}, Second Queue: {}, Moves: {}".format(
    #    "".join(self.first), "".join(self.second),"".join(self.moves)
    #)

    def __str__(self):
        return "".join(self.moves)

    # Euresi lisis
    def success(self):
        return not self.first #diladi i proti lista exei adeiasi

    # Isotita dyo katastaseon
    def __eq__(self, other):
        return isinstance(other, state) and self.first == other.first and self.second==other.second

    def __hash__(self):
        return hash(self.first)


def solve(self):
    Q = deque([self])
    seen={self:None}
    while Q:
        s = Q.popleft()
        for t in s.accessible():
            if t.success():
                seen[t]=s
                return print(t)
            if t not in seen:
                Q.append(t)
                seen[t]=s
            
   

#in1_a=state(('G','U','A','C','A'),(),(False,False,False,False),())

# Test gia prosvasimotitita katastaseon
#for s in in1_a.accessible():
#    print(s)
#    for t in s.accessible():
#       print(" ", t)
#       for l in t.accessible():
#           print("  ",l)

#solve(in1_a)

#Read file, put every RNA line into a list and call solver
f = open(sys.argv[1], "r")
N=int(f.readline())
for i in range(N):
    first_queue=tuple(f.readline().rstrip('\n')) 
    #rstrip removes ' ' from the start and end of the string
    solve(state(first_queue,(),(False,False,False,False),()))
    

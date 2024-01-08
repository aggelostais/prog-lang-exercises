import sys
import numpy

sys.setrecursionlimit(10**6)
d=dict([('A',0),('C',1),('G',2),('U',3)]) #dictionary of acgu_matrix

#makes the complement change to a list
def complement(list_1):
    for i in range(0, len(list_1)):
        if list_1[i]=='A':
            list_1[i]='U'
        elif list_1[i]=='U':
            list_1[i]='A'
        elif list_1[i]=='C': 
            list_1[i]='G'
        else: list_1[i]='C'



# main solver function
def solver(first_queue,second_queue,result,acgu_array,solution_limit): 
    #first_queue: RNA list, second_queue: vaccine, result: moves
    #acgu_array: array to store whether that char was used
    #solution_limit: the maximum solution length permitted currently

    #print("Solver called for first_queue:",''.join(first_queue),",second queue",''.join(second_queue),"and result",''.join(result))
    if solution_limit==0: return [] 

    #p move conditions without solution limit
    if (second_queue==[] # In the begging we can do p move
     or (first_queue[-1]==second_queue[-1]) # letter is the same with last one
     or (first_queue[-1]=='A' and acgu_array[0]==False) # letter has not been used
     or (first_queue[-1]=='C' and acgu_array[1]==False) # ->>-
     or(first_queue[-1]=='G' and acgu_array[2]==False)
     or (first_queue[-1]=='U' and acgu_array[3]==False)):
        
        # p_move copy structures
        p_1st=list(first_queue)
        p_2nd=list(second_queue)
        p_res=list(result)
        p_acgu=numpy.copy(acgu_array)

        # p move tasks
        letter=p_1st.pop()
        p_2nd.append(letter) #put element from the first queue to the second
        p_res.append('p')    #put p in result
        p_acgu[d[letter]]=True #change the letter to used
        
        if(p_1st==[]): # Vaccine was found, 1st queue is empty 
            #print("Vaccine was found and is",''.join(p_res),
            #"of length",len(p_res),".",solution_limit-1,"moves remaining.")
            return p_res # if p is acceptable to finish vaccine it's optimal

        #else:  # Vaccine wasn't found yet,1st queue is not empty
            #print("P move was permitted and the intermediary result is",''.join(p_res),
            #"of length",len(p_res),".",solution_limit-1,"moves remaining.")
           
        P_move=True
            
    else: 
        p_sol=[] # p move can't be made
        P_move=False


    #complement condition without solution limit
    if (result==[] or 
       (len(result)==1 and result[-1]!='c') or 
       (len(result)>=2 and (result[-1]!='c') and (result[-1]=='p' or result[-2]=='p'))):
        
        #complement move tasks
        c_1st=list(first_queue)
        c_res=list(result)
        complement(c_1st)
        c_res.append('c')

        #print("C move was permitted and the intermediary result is ",''.join(c_res),
        #    " of length",len(c_res),".",solution_limit-1," moves remaining.")
        
        C_move=True
    
    else: 
        c_sol=[] #complement move can't be made
        C_move=False


    #reverse condition
    if (len(second_queue)>=2 #second queue must have at least 2 elements 
       and (result[-1]!='r') and (result[-1]=='p' or result[-2]=='p')):
       
        #reverse tasks
        r_2nd=list(second_queue)
        r_res=list(result)
        first=r_2nd.pop(0)
        last=r_2nd.pop()
        r_2nd.insert(0,last)
        r_2nd.append(first)
        r_res.append('r')

        #print("R move was permitted and the intermediary result is ",''.join(r_res),
        #    " of length",len(r_res),".",solution_limit-1," moves remaining.")
        
        R_move=True

    else: 
        r_sol=[] #reverse move can't be made
        R_move=False

    p_sol=[]
    c_sol=[]
    r_sol=[]

    if(P_move==True): p_sol=solver(p_1st,p_2nd,p_res,p_acgu,solution_limit-1)
    if(C_move==True and solution_limit>1): c_sol=solver(c_1st,second_queue,c_res,acgu_array,solution_limit-1) #get recursive result after complement
    if(R_move==True and solution_limit>1): r_sol=solver(first_queue,r_2nd,r_res,acgu_array,solution_limit-1) #get recursive result after reserve

    #Solution within the solution limit was found
    if (len(c_sol)!=0 or len(r_sol)!=0 or len(p_sol)!=0):
        min_sol_len=min(n for n in [len(p_sol),len(c_sol),len(r_sol)] if n>0)
        if (min_sol_len==len(c_sol)):
            return c_sol
        elif (min_sol_len==len(p_sol)):
            return p_sol 
        else:
            return r_sol
        
        # Solution within the solution limit wasn't found
    else: return []


def master_solver(first_queue):
    i=0
    while True:
        sol=solver(first_queue,[],[],numpy.zeros(4,bool),len(first_queue)+i)
        if sol!=[]: 
            return sol
        else:
            i=i+1

    

        


#Read file, put every RNA line into a list and call solver
f = open(sys.argv[1], "r")
N=int(f.readline())
for i in range(N):
    first_queue=list(f.readline().rstrip('\n')) 
    #rstrip removes ' ' from the start and end of the string
    #print(first_queue)
    print(''.join(master_solver(first_queue)))
    
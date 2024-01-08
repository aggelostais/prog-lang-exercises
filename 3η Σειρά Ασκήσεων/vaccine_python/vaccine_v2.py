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
    
    if(len(first_queue)>solution_limit): return [False,False,[]]

    # P MOVE Conditions
    if (second_queue==[] # In the begining we can do p move
     or (first_queue[-1]==second_queue[-1]) # letter is the same with last one
     or (first_queue[-1]=='A' and acgu_array[0]==False) # letter has not been used
     or (first_queue[-1]=='C' and acgu_array[1]==False) # ->>-
     or(first_queue[-1]=='G' and acgu_array[2]==False)
     or (first_queue[-1]=='U' and acgu_array[3]==False)):
        
        # p_move copy structures
        p_1st=list(first_queue)
        p_2nd=list(second_queue)
        p_res=list(result) #intermediate result
        p_acgu=numpy.copy(acgu_array)

        # p move tasks
        letter=p_1st.pop()     #remove element from first queue
        p_2nd.append(letter)   #put element to the second queue
        p_res.append('p')      #put p in result
        p_acgu[d[letter]]=True #change the letter to used
        
        if(p_1st==[]): # Vaccine was found, 1st queue is empty 
            
            #print("Finished. Result:",''.join(p_res),
            #"of length",len(p_res),".",solution_limit-1,"moves remaining.")
            
            return [True,True,p_res] 
            #[Finished in that step,Result]
            # if p is acceptable to finish vaccine it's optimal

        #else:  # Vaccine wasn't found yet,1st queue is not empty
            #print("P move possible. Intermediate result:",''.join(p_res),
           # "of length",len(p_res),".",solution_limit-1,"moves remaining.")
           
        P_move=True
            
    else: # P move isn't permitted in this state
        P_move=False
        p_1st=[]
        p_2nd=[]
        p_res=[]
        p_acgu=[]
        p_sol=[] # There isn't a final solution from that move



    # C MOVE Conditions
    if ((len(result)==1 and result[-1]!='c') # complement shouldn't be first move
        or (len(result)>=2 and (result[-1]!='c') and (result[-1]=='p' or result[-2]=='p'))):
        
        #complement move tasks
        c_1st=list(first_queue) #first queue after the complement 
        c_res=list(result)      #moves result with added c
        complement(c_1st)
        c_res.append('c')

        #print("C move possible. Intermediary result:",''.join(c_res),
        #   " of length",len(c_res),".",solution_limit-1," moves remaining.")
        
        C_move=True
    
    else: # C move isn't permitted
        C_move=False
        c_1st=[]
        c_res=[]
        c_sol=[] #There isn't a final solution from that move
        

    # R MOVE Conditions
    if (len(second_queue)>=2 #second queue must have at least 2 elements 
       and (result[-1]!='r') and (result[-1]=='p' or result[-2]=='p')):
       
        #reverse tasks
        r_2nd=list(second_queue) #second queue after the reserve move
        r_res=list(result)       #moves result with added r
        first=r_2nd.pop(0)
        last=r_2nd.pop()
        r_2nd.insert(0,last)
        r_2nd.append(first)
        r_res.append('r')

        #print("R move possible. Intermediary result:",''.join(r_res),
        #   " of length",len(r_res),".",solution_limit-1," moves remaining.")
        
        R_move=True

    else: # R move isn't permitted
        r_sol=[] #reverse move can't be made
        R_move=False
        r_2nd=[]
        r_res=[]

    p_sol=[] # There isn't a solution cause of solution limit expiration
    c_sol=[] # ->>-
    r_sol=[]

    if(P_move==True): 
        
        #print("Calling Solver for P-move with 1:",''.join(p_1st),"2:",''.join(p_2nd),"Intermediate result:",''.join(p_res),
        #"and solution limit:",solution_limit-1)
        
        P_rec=solver(p_1st,p_2nd,p_res,p_acgu,solution_limit-1) # Run solver recursively   
        if(P_rec[0]==True or P_rec[1]==True): # Solution within the solution limit found
            p_sol=P_rec[2]      
        else: p_sol=[]                        # Solution within the solution limit not found
    if(C_move==True): #and solution_limit-1>1):  
        
        #print("Calling Solver for C-move with 1:",''.join(c_1st),"2:",''.join(second_queue),"Intermediate result:",''.join(c_res),
        #"and solution limit:",solution_limit-1)
        
        C_rec=solver(c_1st,second_queue,c_res,acgu_array,solution_limit-1)
        if(C_rec[0]==True or C_rec[1]==True):
            c_sol=C_rec[2]      
        else: c_sol=[]
    if(R_move==True): # and solution_limit-1>1): 
        
        #print("Calling Solver for R-move with 1:",''.join(first_queue),"2:",''.join(r_2nd),"Intermediate result:",''.join(r_res),
        #"and solution limit:",solution_limit-1)
        
        R_rec=solver(first_queue,r_2nd,r_res,acgu_array,solution_limit-1) 
        if(R_rec[0]==True or R_rec[1]==True):
            r_sol=R_rec[2]      
        else: r_sol=[]

    #Solution within the solution limit was found
    #print(p_sol)
    #print(c_sol)
    #print(r_sol)
    if (len(p_sol)!=0 or len(c_sol)!=0 or len(r_sol)!=0):
        min_sol_len=min(n for n in [len(p_sol),len(c_sol),len(r_sol)] if n>0)
        if (min_sol_len==len(c_sol)):
            return [False,True,c_sol]
            #[Finished in that step,Found solution within solution limit
        elif (min_sol_len==len(p_sol)):
            return [False,True,p_sol]
        else:
            return [False,True,r_sol]
        
        # Solution within the solution limit wasn't found
    else: 
        return [False,False,P_move,C_move,R_move,p_1st,p_2nd,p_res,p_acgu,c_1st,second_queue,c_res,acgu_array,first_queue,r_2nd,r_res,acgu_array]
        # moves: True or False if they can/can't be made regardless of solution limit #
        

def master_solver(first_queue):

        init_limit=len(first_queue)+1
        
        sol=solver(first_queue,[],[],numpy.zeros(4,bool),init_limit) #First call of solver
        i=1
        while True:
            if (sol[0]==True or sol[1]==True): return sol[2] # finished within the solution limit
            else:                                            # didn't finish within the solution limit
                    p_sol=[]
                    c_sol=[]
                    r_sol=[]
                    if(sol[2]==False and sol[3]==False and sol[4]==False): # No move is permitted, there isn't a solution
                        #print("No move can be made. Exiting...")
                        return [] 
                    if(sol[3]==True): # C_move is permitted
                        new_sol=solver(sol[9],sol[10],sol[11],sol[12],init_limit*i)
                        if(new_sol[0]==True or new_sol[1]==True): 
                            c_sol=new_sol[2]
                    if(sol[2]==True): # P_move is permitted
                        new_sol=solver(sol[5],sol[6],sol[7],sol[8],init_limit*i) # Trying to find P_move solution within new solution limit
                        if(new_sol[0]==True or new_sol[1]==True):                # P_move solution with new solution limit found
                            p_sol=new_sol[2]
                    if(sol[4]==True): # R_move can be made
                        new_sol=solver(sol[13],sol[14],sol[15],sol[16],init_limit*i)
                        if(new_sol[0]==True or new_sol[1]==True): 
                            r_sol=new_sol[2]
                    if (len(p_sol)!=0 or len(c_sol)!=0 or len(r_sol)!=0):
                        min_sol_len=min(n for n in [len(p_sol),len(c_sol),len(r_sol)] if n>0)
                        if (min_sol_len==len(c_sol)):
                            return c_sol
                        elif (min_sol_len==len(p_sol)):
                            return p_sol
                        else:
                            return r_sol
                    else:
                        sol=new_sol
                        #print("\n\nIncreasing solution limit to",init_limit*(i+1),"\n\n")
                        i=i*i
                  

#Read file, put every RNA line into a list and call solver
f = open(sys.argv[1], "r")
N=int(f.readline())
for i in range(N):
    first_queue=list(f.readline().rstrip('\n')) 
    #rstrip removes ' ' from the start and end of the string
    #print(first_queue)
    print(''.join(master_solver(first_queue)))
    

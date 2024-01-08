# Python program to  
# demonstrate stack implementation 
# using list 
stack = [] 
  
# append() function to push 
# element in the stack 
stack.append('a') 
stack.append('b') 
stack.append('c') 
  
print('Initial stack') 
print(stack) 

print(stack[-1])
print(stack[0])
  
# pop() fucntion to pop 
# element from stack in  
# LIFO order 
print('\nElements poped from stack:') 
print(stack.pop()) 
print(stack.pop()) 
print(stack.pop()) 
  
print('\nStack after elements are poped:') 
print(stack) 

# Split string into characters 
def split(word): 
    return list(word) 
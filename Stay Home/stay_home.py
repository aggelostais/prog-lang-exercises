import copy
import queue
import numpy
import sys

sys.setrecursionlimit(10**6)

def virus_flood(graph_in, q, visited, airports, airp_found, N, M):
    curr = q.get()
    x = curr[0]
    y = curr[1]
    time = curr[2]
    # print("popping (", x, y, ")")
    if(type(graph_in[x][y]) is int and graph_in[x][y] < time):
        # print("#1doin nothing on node", x, ",", y)
        return
    if(graph_in[x][y] != 'X'):
        # print("visiting (", x, y, ")")

        if(x > 0 and x < N-1 and y > 0  and y < M-1):
            # print("#1")
            if(visited[x+1][y] < 2 and (type(graph_in[x+1][y]) is str or (type(graph_in[x+1][y]) is int and graph_in[x+1][y] > time+2))):
                q.put([x+1, y, time+2])
                visited[x+1][y] += 1
            if(visited[x][y-1] < 2 and (type(graph_in[x][y-1]) is str or (type(graph_in[x][y-1]) is int and graph_in[x][y-1] > time+2))):
                q.put([x, y-1, time+2])
                visited[x][y-1] += 1
            if(visited[x][y+1] < 2 and (type(graph_in[x][y+1]) is str or (type(graph_in[x][y+1]) is int and graph_in[x][y+1] > time+2))):
                q.put([x, y+1, time+2])
                visited[x][y+1] += 1
            if(visited[x-1][y] < 2 and (type(graph_in[x-1][y]) is str or (type(graph_in[x-1][y]) is int and graph_in[x-1][y] > time+2))):
                q.put([x-1, y, time+2])
                visited[x-1][y] += 1
        elif(x == 0 and y > 0  and y < M-1):
            # print("#2")
            if(visited[x+1][y] < 2 and (type(graph_in[x+1][y]) is str or (type(graph_in[x+1][y]) is int and graph_in[x+1][y] > time+2))):
                q.put([x+1, y, time+2])
                visited[x+1][y] += 1
            if(visited[x][y-1] < 2 and (type(graph_in[x][y-1]) is str or (type(graph_in[x][y-1]) is int and graph_in[x][y-1] > time+2))):
                q.put([x, y-1, time+2])
                visited[x][y-1] += 1
            if(visited[x][y+1] < 2 and (type(graph_in[x][y+1]) is str or (type(graph_in[x][y+1]) is int and graph_in[x][y+1] > time+2))):
                q.put([x, y+1, time+2])
                visited[x][y+1] += 1
        elif(x == N-1 and y > 0  and y < M-1):
            # print("#3")
            if(visited[x][y-1] < 2 and (type(graph_in[x][y-1]) is str or (type(graph_in[x][y-1]) is int and graph_in[x][y-1] > time+2))):
                q.put([x, y-1, time+2])
                visited[x][y-1] += 1
            if(visited[x][y+1] < 2 and (type(graph_in[x][y+1]) is str or (type(graph_in[x][y+1]) is int and graph_in[x][y+1] > time+2))):
                q.put([x, y+1, time+2])
                visited[x][y+1] += 1
            if(visited[x-1][y] < 2 and (type(graph_in[x-1][y]) is str or (type(graph_in[x-1][y]) is int and graph_in[x-1][y] > time+2))):
                q.put([x-1, y, time+2])
                visited[x-1][y] += 1
        elif(x > 0 and x < N-1 and y == 0):
            # print("#4")
            if(visited[x+1][y] < 2 and (type(graph_in[x+1][y]) is str or (type(graph_in[x+1][y]) is int and graph_in[x+1][y] > time+2))):
                q.put([x+1, y, time+2])
                visited[x+1][y] += 1
            if(visited[x][y+1] < 2 and (type(graph_in[x][y+1]) is str or (type(graph_in[x][y+1]) is int and graph_in[x][y+1] > time+2))):
                q.put([x, y+1, time+2])
                visited[x][y+1] += 1
            if(visited[x-1][y] < 2 and (type(graph_in[x-1][y]) is str or (type(graph_in[x-1][y]) is int and graph_in[x-1][y] > time+2))):
                q.put([x-1, y, time+2])
                visited[x-1][y] += 1
        elif(x > 0 and x < N-1 and y == M-1):
            # print("#5")
            if(visited[x+1][y] < 2 and (type(graph_in[x+1][y]) is str or (type(graph_in[x+1][y]) is int and graph_in[x+1][y] > time+2))):
                q.put([x+1, y, time+2])
                visited[x+1][y] += 1
            if(visited[x][y-1] < 2 and (type(graph_in[x][y-1]) is str or (type(graph_in[x][y-1]) is int and graph_in[x][y-1] > time+2))):
                q.put([x, y-1, time+2])
                visited[x][y-1] += 1
            if(visited[x-1][y] < 2 and (type(graph_in[x-1][y]) is str or (type(graph_in[x-1][y]) is int and graph_in[x-1][y] > time+2))):
                q.put([x-1, y, time+2])
                visited[x-1][y] += 1
        elif(x == 0 and y == 0):
            # print("#6")
            if(visited[x+1][y] < 2 and (type(graph_in[x+1][y]) is str or (type(graph_in[x+1][y]) is int and graph_in[x+1][y] > time+2))):
                q.put([x+1, y, time+2])
                visited[x+1][y] += 1
            if(visited[x][y+1] < 2 and (type(graph_in[x][y+1]) is str or (type(graph_in[x][y+1]) is int and graph_in[x][y+1] > time+2))):
                q.put([x, y+1, time+2])
                visited[x][y+1] += 1
        elif(x == N-1 and y == M-1):
            # print("#7")
            if(visited[x][y-1] < 2 and (type(graph_in[x][y-1]) is str or (type(graph_in[x][y-1]) is int and graph_in[x][y-1] > time+2))):
                q.put([x, y-1, time+2])
                visited[x][y-1] += 1
            if(visited[x-1][y] < 2 and (type(graph_in[x-1][y]) is str or (type(graph_in[x-1][y]) is int and graph_in[x-1][y] > time+2))):
                q.put([x-1, y, time+2])
                visited[x-1][y] += 1
        elif(x == 0 and y == M-1):
            if(visited[x+1][y] < 2 and (type(graph_in[x+1][y]) is str or (type(graph_in[x+1][y]) is int and graph_in[x+1][y] > time+2))):
                q.put([x+1, y, time+2])
                visited[x+1][y] += 1
            if(visited[x][y-1] < 2 and (type(graph_in[x][y-1]) is str or (type(graph_in[x][y-1]) is int and graph_in[x][y-1] > time+2))):
                q.put([x, y-1, time+2])
                visited[x][y-1] += 1
        elif(x == N-1 and y == 0):
            if(visited[x][y+1] < 2 and (type(graph_in[x][y+1]) is str or (type(graph_in[x][y+1]) is int and graph_in[x][y+1] > time+2))):
                q.put([x, y+1, time+2])
                visited[x][y+1] += 1
            if(visited[x-1][y] < 2 and (type(graph_in[x-1][y]) is str or (type(graph_in[x-1][y]) is int and graph_in[x-1][y] > time+2))):
                q.put([x-1, y, time+2])
                visited[x-1][y] += 1

        if(graph_in[x][y] == 'A' and not airp_found[0]):
            # first airport found
            airp_found[0] = True
            airports.remove([x,y])
            visited[x][y] += 1
            while airports :
                air = airports.pop(0)
                air.append(time+5)
                q.put(air)
                visited[air[0]][air[1]] += 1

        graph_in[x][y] = time

def sot_flood(sot_graph, virus_graph, q, visit, parents, N, M):
    curr = q.get()
    x = curr[0]
    y = curr[1]
    time = curr[2]
    prev_x = curr[3]
    prev_y = curr[4]
    # print("popping (", x, y, ")")
    if(type(sot_graph[x][y]) is int and sot_graph[x][y] < time):
        # print("#sot:doin nothing on node", x, ",", y)
        return

    #if virus got there faster
    if(type(virus_graph[x][y]) is int and virus_graph[x][y] <= time):
        sot_graph[x][y] = 'X'
        visit[x][y] = 1
        return

    if(sot_graph[x][y] != 'X'):
        # print("visiting (", x, y, ")")
        if(x > 0 and x < N-1 and y > 0  and y < M-1):
            # print("#1")
            if(not visit[x+1,y]):
                visit[x+1,y] = 1
                q.put([x+1, y, time+1, x, y])
            if(not visit[x,y-1]):
                visit[x,y-1] = 1
                q.put([x, y-1, time+1, x, y])
            if(not visit[x,y+1]):
                visit[x,y+1] = 1
                q.put([x, y+1, time+1, x, y])
            if(not visit[x-1,y]):
                visit[x-1,y] = 1
                q.put([x-1, y, time+1, x, y])
        elif(x == 0 and y > 0  and y < M-1):
            # print("#2")
            if(not visit[x+1,y]):
                visit[x+1,y] = 1
                q.put([x+1, y, time+1, x, y])
            if(not visit[x,y-1]):
                visit[x,y-1] = 1
                q.put([x, y-1, time+1, x, y])
            if(not visit[x,y+1]):
                visit[x,y+1] = 1
                q.put([x, y+1, time+1, x, y])
        elif(x == N-1 and y > 0  and y < M-1):
            # print("#3")
            if(not visit[x,y-1]):
                visit[x,y-1] = 1
                q.put([x, y-1, time+1, x, y])
            if(not visit[x,y+1]):
                visit[x,y+1] = 1
                q.put([x, y+1, time+1, x, y])
            if(not visit[x-1,y]):
                visit[x-1,y] = 1
                q.put([x-1, y, time+1, x, y])
        elif(x > 0 and x < N-1 and y == 0):
            # print("#4")
            if(not visit[x+1,y]):
                visit[x+1,y] = 1
                q.put([x+1, y, time+1, x, y])
            if(not visit[x,y+1]):
                visit[x,y+1] = 1
                q.put([x, y+1, time+1, x, y])
            if(not visit[x-1,y]):
                visit[x-1,y] = 1
                q.put([x-1, y, time+1, x, y])
        elif(x > 0 and x < N-1 and y == M-1):
            # print("#5")
            if(not visit[x+1,y]):
                visit[x+1,y] = 1
                q.put([x+1, y, time+1, x, y])
            if(not visit[x,y-1]):
                visit[x,y-1] = 1
                q.put([x, y-1, time+1, x, y])
            if(not visit[x-1,y]):
                visit[x-1,y] = 1
                q.put([x-1, y, time+1, x, y])
        elif(x == 0 and y == 0):
            # print("#6")
            if(not visit[x+1,y]):
                visit[x+1,y] = 1
                q.put([x+1, y, time+1, x, y])
            if(not visit[x,y+1]):
                visit[x,y+1] = 1
                q.put([x, y+1, time+1, x, y])
        elif(x == N-1 and y == M-1):
            # print("#7")
            if(not visit[x,y-1]):
                visit[x,y-1] = 1
                q.put([x, y-1, time+1, x, y])
            if(not visit[x-1,y]):
                visit[x-1,y] = 1
                q.put([x-1, y, time+1, x, y])
        elif(x == 0 and y == M-1):
            if(not visit[x+1,y]):
                visit[x+1,y] = 1
                q.put([x+1, y, time+1, x, y])
            if(not visit[x,y-1]):
                visit[x,y-1] = 1
                q.put([x, y-1, time+1, x, y])
        elif(x == N-1 and y == 0):
            if(not visit[x,y+1]):
                visit[x,y+1] = 1
                q.put([x, y+1, time+1, x, y])
            if(not visit[x-1,y]):
                visit[x-1,y] = 1
                q.put([x-1, y, time+1, x, y])

        sot_graph[x][y] = time
        parents[x][y][0] = prev_x
        parents[x][y][1] = prev_y

def par_path(sot_graph, parents, x, y):
    if(sot_graph[x][y] == 0):
        return []

    prev_x = parents[x][y][0]
    prev_y = parents[x][y][1]
    res = par_path(sot_graph, parents, prev_x, prev_y)

    if(prev_x < x):
        res.append('D')
    elif(x < prev_x):
        res.append('U')
    elif(prev_y < y):
        res.append('R')
    elif(y < prev_y):
        res.append('L')

    return res

# we are going to read a graph of size N x M
# and use 2d and 3d lists for representation

if len(sys.argv) != 2:
    print("improper call!")
    sys.exit()

f = open(sys.argv[1], "r")
N = 0
M = 0
i = 0
j = 0
S = [0,0]
W = [0,0]
T = [0,0]
A = []
graph = [[]]
while 1 :
    char = f.read(1)
    if not char:
        break
    else:
        if char == '\n':
            graph.append([])
            i += 1
            j = 0
        elif char == 'S':
            S = [i, j]
            graph[i].append('S')
            j += 1
        elif char == 'T':
            T = [i,j]
            graph[i].append('T')
            j += 1
        elif char == 'W':
            W = [i,j]
            graph[i].append('W')
            j += 1
        elif char == 'A':
            A.append([i,j])
            graph[i].append('A')
            j += 1
        else:
            graph[i].append(char)
            j += 1
# print("done reading!")
del graph[-1]
f.close()

N = i
M = len(graph[0])

virus_graph = copy.deepcopy(graph)
visited = numpy.zeros([N, M], dtype = int)
qu = queue.Queue()
qu.put([W[0], W[1], 0, -1, -1])
found = [False]
while not qu.empty():
    virus_flood(virus_graph, qu, visited, A, found, N, M)


parents = numpy.zeros([N, M, 2], dtype = int)
sot_graph = copy.deepcopy(graph)
visited = numpy.zeros([N, M], dtype = int)
qu = queue.Queue()
qu.put([S[0], S[1], 0, -1, -1])
while (not qu.empty()):
    sot_flood(sot_graph, virus_graph, qu, visited, parents, N, M)


if( type(sot_graph[T[0]][T[1]]) is int ):
    #possible
    print(sot_graph[T[0]][T[1]])
    p = par_path(sot_graph, parents, T[0], T[1])
    print(*p, sep = "")
else:
    print("IMPOSSIBLE")
        
import java.io.*;
import java.util.*;
public class StayHome{
    int N, M;
    int[] S;
    int[] T ;
    int[] W ;
    Vector<int[]> A;
    Vector< Vector<Integer> > graph;

    public StayHome(){} //empty constructor

    public void readGraph(String filename) throws Exception
    {
        File input_file = new File(filename);
        S = new int[2];
        T = new int[2];
        W = new int[2];
        A = new Vector<int[]>();
        BufferedReader br = new BufferedReader(new FileReader(input_file));
        int read;
        graph = new Vector< Vector<Integer> >();
        Vector<Integer> row = new Vector<Integer>();
        int x = 0, y = 0;
        while ((read = br.read()) != -1)
        {
            char c = (char)read;
            if(c == '\n'){
                y++;
                x = 0;
                graph.add(row);
                row = new Vector<Integer>();
            }
            else{
                switch(read){
                    case 46:
                        read = -1;  // dot
                        break;
                    case 65:
                        read = -2; // airport
                        int[] tmp = new int[2];
                        tmp[0] = y;
                        tmp[1] = x;
                        A.add(tmp);
                        break;
                    case 83:
                        read = -10; // S
                        S[0] = y;
                        S[1] = x;
                        break;
                    case 84:
                        read = -42; // T
                        T[0] = y;
                        T[1] = x;
                        break;
                    case 87:
                        read = -5; // W
                        W[0] = y;
                        W[1] = x;
                        break;
                    case 88:
                        read = -7; // X
                        break;
                    default:
                        read = 0;
                }
                row.add(read);
                x++;
            }
        }
        br.close();
    }

    public void virus_flood(Vector<Vector<Integer>> virus_graph,
    LinkedList<Integer[]> queue, Integer[][] visited, boolean[] airp_found)
    {
        Integer[] curr = queue.poll();
        int x = curr[0];
        int y = curr[1];
        int time = curr[2];

        if(virus_graph.get(x).get(y) > -1 && virus_graph.get(x).get(y) < time)
            // Do nothing
            return;
        if(virus_graph.get(x).get(y) != -7) // -7 --> X
        {
            if(x > 0 && x < N-1 && y > 0 && y < M-1){
                if(visited[x+1][y] < 2 && ( virus_graph.get(x+1).get(y) < 0 || virus_graph.get(x+1).get(y) > time+2)){
                    Integer[] q_item = {x+1, y, time+2};
                    queue.add(q_item);
                    visited[x+1][y] += 1;
                }
                if(visited[x][y-1] < 2 && (virus_graph.get(x).get(y-1) < 0 || virus_graph.get(x).get(y-1) > time+2)){
                    Integer[] q_item = {x, y-1, time+2};
                    queue.add(q_item);
                    visited[x][y-1] += 1;
                }
                if(visited[x][y+1] < 2 && (virus_graph.get(x).get(y+1) < 0 || virus_graph.get(x).get(y+1) > time+2)){
                    Integer[] q_item = {x, y+1, time+2};
                    queue.add(q_item);
                    visited[x][y+1] += 1;
                }
                if(visited[x-1][y] < 2 && (virus_graph.get(x-1).get(y) < 0 || virus_graph.get(x-1).get(y) > time+2)){
                    Integer[] q_item = {x-1, y, time+2};
                    queue.add(q_item);
                    visited[x-1][y] += 1;
                }
            }
            else if(x == 0 && y > 0  && y < M-1){
                if(visited[x+1][y] < 2 && ( virus_graph.get(x+1).get(y) < 0 || virus_graph.get(x+1).get(y) > time+2)){
                    Integer[] q_item = {x+1, y, time+2};
                    queue.add(q_item);
                    visited[x+1][y] += 1;
                }
                if(visited[x][y-1] < 2 && (virus_graph.get(x).get(y-1) < 0 || virus_graph.get(x).get(y-1) > time+2)){
                    Integer[] q_item = {x, y-1, time+2};
                    queue.add(q_item);
                    visited[x][y-1] += 1;
                }
                if(visited[x][y+1] < 2 && (virus_graph.get(x).get(y+1) < 0 || virus_graph.get(x).get(y+1) > time+2)){
                    Integer[] q_item = {x, y+1, time+2};
                    queue.add(q_item);
                    visited[x][y+1] += 1;
                }
            }
            else if(x == N-1 && y > 0  && y < M-1){
                if(visited[x][y-1] < 2 && (virus_graph.get(x).get(y-1) < 0 || virus_graph.get(x).get(y-1) > time+2)){
                    Integer[] q_item = {x, y-1, time+2};
                    queue.add(q_item);
                    visited[x][y-1] += 1;
                }
                if(visited[x][y+1] < 2 && (virus_graph.get(x).get(y+1) < 0 || virus_graph.get(x).get(y+1) > time+2)){
                    Integer[] q_item = {x, y+1, time+2};
                    queue.add(q_item);
                    visited[x][y+1] += 1;
                }
                if(visited[x-1][y] < 2 && (virus_graph.get(x-1).get(y) < 0 || virus_graph.get(x-1).get(y) > time+2)){
                    Integer[] q_item = {x-1, y, time+2};
                    queue.add(q_item);
                    visited[x-1][y] += 1;
                }
            }
            else if(x > 0 && x < N-1 && y == 0){
                if(visited[x+1][y] < 2 && ( virus_graph.get(x+1).get(y) < 0 || virus_graph.get(x+1).get(y) > time+2)){
                    Integer[] q_item = {x+1, y, time+2};
                    queue.add(q_item);
                    visited[x+1][y] += 1;
                }
                if(visited[x][y+1] < 2 && (virus_graph.get(x).get(y+1) < 0 || virus_graph.get(x).get(y+1) > time+2)){
                    Integer[] q_item = {x, y+1, time+2};
                    queue.add(q_item);
                    visited[x][y+1] += 1;
                }
                if(visited[x-1][y] < 2 && (virus_graph.get(x-1).get(y) < 0 || virus_graph.get(x-1).get(y) > time+2)){
                    Integer[] q_item = {x-1, y, time+2};
                    queue.add(q_item);
                    visited[x-1][y] += 1;
                }
            }
            else if(x > 0 && x < N-1 && y == M-1){
                if(visited[x+1][y] < 2 && ( virus_graph.get(x+1).get(y) < 0 || virus_graph.get(x+1).get(y) > time+2)){
                    Integer[] q_item = {x+1, y, time+2};
                    queue.add(q_item);
                    visited[x+1][y] += 1;
                }
                if(visited[x][y-1] < 2 && (virus_graph.get(x).get(y-1) < 0 || virus_graph.get(x).get(y-1) > time+2)){
                    Integer[] q_item = {x, y-1, time+2};
                    queue.add(q_item);
                    visited[x][y-1] += 1;
                }
                if(visited[x-1][y] < 2 && (virus_graph.get(x-1).get(y) < 0 || virus_graph.get(x-1).get(y) > time+2)){
                    Integer[] q_item = {x-1, y, time+2};
                    queue.add(q_item);
                    visited[x-1][y] += 1;
                }
            }
            else if(x == 0 && y == 0){
                if(visited[x+1][y] < 2 && ( virus_graph.get(x+1).get(y) < 0 || virus_graph.get(x+1).get(y) > time+2)){
                    Integer[] q_item = {x+1, y, time+2};
                    queue.add(q_item);
                    visited[x+1][y] += 1;
                }
                if(visited[x][y+1] < 2 && (virus_graph.get(x).get(y+1) < 0 || virus_graph.get(x).get(y+1) > time+2)){
                    Integer[] q_item = {x, y+1, time+2};
                    queue.add(q_item);
                    visited[x][y+1] += 1;
                }
            }
            else if(x == N-1 && y == M-1){
                if(visited[x][y-1] < 2 && (virus_graph.get(x).get(y-1) < 0 || virus_graph.get(x).get(y-1) > time+2)){
                    Integer[] q_item = {x, y-1, time+2};
                    queue.add(q_item);
                    visited[x][y-1] += 1;
                }
                if(visited[x-1][y] < 2 && (virus_graph.get(x-1).get(y) < 0 || virus_graph.get(x-1).get(y) > time+2)){
                    Integer[] q_item = {x-1, y, time+2};
                    queue.add(q_item);
                    visited[x-1][y] += 1;
                }
            }
            else if(x == 0 && y == M-1){
                if(visited[x+1][y] < 2 && ( virus_graph.get(x+1).get(y) < 0 || virus_graph.get(x+1).get(y) > time+2)){
                    Integer[] q_item = {x+1, y, time+2};
                    queue.add(q_item);
                    visited[x+1][y] += 1;
                }
                if(visited[x][y-1] < 2 && (virus_graph.get(x).get(y-1) < 0 || virus_graph.get(x).get(y-1) > time+2)){
                    Integer[] q_item = {x, y-1, time+2};
                    queue.add(q_item);
                    visited[x][y-1] += 1;
                }
            }
            else if(x == N-1 && y == 0){
                if(visited[x][y+1] < 2 && (virus_graph.get(x).get(y+1) < 0 || virus_graph.get(x).get(y+1) > time+2)){
                    Integer[] q_item = {x, y+1, time+2};
                    queue.add(q_item);
                    visited[x][y+1] += 1;
                }
                if(visited[x-1][y] < 2 && (virus_graph.get(x-1).get(y) < 0 || virus_graph.get(x-1).get(y) > time+2)){
                    Integer[] q_item = {x-1, y, time+2};
                    queue.add(q_item);
                    visited[x-1][y] += 1;
                }
            }

            // I need to override Vector.equals() for A.remove to work
            if(virus_graph.get(x).get(y) == -2 && !airp_found[0]){ //-2 --> A
                // First airport found
                airp_found[0] = true;
                // Next lines are to remove airport safely
                int[] target = new int[2];
                for(int[] item : A)
                    if(item[0] == x && item[1] == y)
                        target = item;
                A.removeElement(target);
                visited[x][y] += 1;
                while(A.size() > 0){
                    int[] element = A.remove(0); //remove first item
                    Integer[] to_queue = {element[0], element[1], time+5};
                    queue.add(to_queue);
                    visited[element[0]][element[1]] += 1;
                }
            }
            // virus_graph[x][y] = time;
            virus_graph.get(x).setElementAt(time, y);
        }
    }

    public void sot_flood(Vector<Vector<Integer>> sot_graph,
    Vector<Vector<Integer>> virus_graph, LinkedList<Integer[]> queue,
    Integer[][] visited, Integer[][][] parents)
    {
        Integer[] curr = queue.poll();
        int x = curr[0];
        int y = curr[1];
        int time = curr[2];
        int prev_x = curr[3];
        int prev_y = curr[4];

        if(sot_graph.get(x).get(y) > -1 && sot_graph.get(x).get(y) < time)
            // Do nothing
            return;
        // if virus got there faster
        if(virus_graph.get(x).get(y) != -7 && virus_graph.get(x).get(y) <= time){
            sot_graph.get(x).setElementAt(-7, y);
            visited[x][y] = 1;
            return;
        }

        if(sot_graph.get(x).get(y) != -7){ //-7 --> X
            if(x > 0 && x < N-1 && y > 0 && y < M-1){
                if(visited[x+1][y] == 0){
                    visited[x+1][y] = 1;
                    Integer[] to_queue = {x+1, y, time+1, x, y};
                    queue.add(to_queue);
                }
                if(visited[x][y-1] == 0){
                    visited[x][y-1] = 1;
                    Integer[] to_queue = {x, y-1, time+1, x, y};
                    queue.add(to_queue);
                }
                if(visited[x][y+1] == 0){
                    visited[x][y+1] = 1;
                    Integer[] to_queue = {x, y+1, time+1, x, y};
                    queue.add(to_queue);
                }
                if(visited[x-1][y] == 0){
                    visited[x-1][y] = 1;
                    Integer[] to_queue = {x-1, y, time+1, x, y};
                    queue.add(to_queue);
                }
            }
            else if(x == 0 && y > 0  && y < M-1){
                if(visited[x+1][y] == 0){
                    visited[x+1][y] = 1;
                    Integer[] to_queue = {x+1, y, time+1, x, y};
                    queue.add(to_queue);
                }
                if(visited[x][y-1] == 0){
                    visited[x][y-1] = 1;
                    Integer[] to_queue = {x, y-1, time+1, x, y};
                    queue.add(to_queue);
                }
                if(visited[x][y+1] == 0){
                    visited[x][y+1] = 1;
                    Integer[] to_queue = {x, y+1, time+1, x, y};
                    queue.add(to_queue);
                }
            }
            else if(x == N-1 && y > 0  && y < M-1){
                if(visited[x][y-1] == 0){
                    visited[x][y-1] = 1;
                    Integer[] to_queue = {x, y-1, time+1, x, y};
                    queue.add(to_queue);
                }
                if(visited[x][y+1] == 0){
                    visited[x][y+1] = 1;
                    Integer[] to_queue = {x, y+1, time+1, x, y};
                    queue.add(to_queue);
                }
                if(visited[x-1][y] == 0){
                    visited[x-1][y] = 1;
                    Integer[] to_queue = {x-1, y, time+1, x, y};
                    queue.add(to_queue);
                }
            }
            else if(x > 0 && x < N-1 && y == 0){
                if(visited[x+1][y] == 0){
                    visited[x+1][y] = 1;
                    Integer[] to_queue = {x+1, y, time+1, x, y};
                    queue.add(to_queue);
                }
                if(visited[x][y+1] == 0){
                    visited[x][y+1] = 1;
                    Integer[] to_queue = {x, y+1, time+1, x, y};
                    queue.add(to_queue);
                }
                if(visited[x-1][y] == 0){
                    visited[x-1][y] = 1;
                    Integer[] to_queue = {x-1, y, time+1, x, y};
                    queue.add(to_queue);
                }
            }
            else if(x > 0 && x < N-1 && y == M-1){
                if(visited[x+1][y] == 0){
                    visited[x+1][y] = 1;
                    Integer[] to_queue = {x+1, y, time+1, x, y};
                    queue.add(to_queue);
                }
                if(visited[x][y-1] == 0){
                    visited[x][y-1] = 1;
                    Integer[] to_queue = {x, y-1, time+1, x, y};
                    queue.add(to_queue);
                }
                if(visited[x-1][y] == 0){
                    visited[x-1][y] = 1;
                    Integer[] to_queue = {x-1, y, time+1, x, y};
                    queue.add(to_queue);
                }
            }
            else if(x == 0 && y == 0){
                if(visited[x+1][y] == 0){
                    visited[x+1][y] = 1;
                    Integer[] to_queue = {x+1, y, time+1, x, y};
                    queue.add(to_queue);
                }
                if(visited[x][y+1] == 0){
                    visited[x][y+1] = 1;
                    Integer[] to_queue = {x, y+1, time+1, x, y};
                    queue.add(to_queue);
                }
            }
            else if(x == N-1 && y == M-1){
                if(visited[x][y-1] == 0){
                    visited[x][y-1] = 1;
                    Integer[] to_queue = {x, y-1, time+1, x, y};
                    queue.add(to_queue);
                }
                if(visited[x-1][y] == 0){
                    visited[x-1][y] = 1;
                    Integer[] to_queue = {x-1, y, time+1, x, y};
                    queue.add(to_queue);
                }
            }
            else if(x == 0 && y == M-1){
                if(visited[x+1][y] == 0){
                    visited[x+1][y] = 1;
                    Integer[] to_queue = {x+1, y, time+1, x, y};
                    queue.add(to_queue);
                }
                if(visited[x][y-1] == 0){
                    visited[x][y-1] = 1;
                    Integer[] to_queue = {x, y-1, time+1, x, y};
                    queue.add(to_queue);
                }
            }
            else if(x == N-1 && y == 0){
                if(visited[x][y+1] == 0){
                    visited[x][y+1] = 1;
                    Integer[] to_queue = {x, y+1, time+1, x, y};
                    queue.add(to_queue);
                }
                if(visited[x-1][y] == 0){
                    visited[x-1][y] = 1;
                    Integer[] to_queue = {x-1, y, time+1, x, y};
                    queue.add(to_queue);
                }
            }

            sot_graph.get(x).setElementAt(time, y);
            parents[x][y][0] = prev_x;
            parents[x][y][1] = prev_y;
        }
    }

    public LinkedList<Character> par_path
    (Vector<Vector<Integer>> sot_graph, Integer[][][] parents,
    int x, int y){
        LinkedList<Character> res;
        if(sot_graph.get(x).get(y) == 0){
            res = new LinkedList<Character>();
            return res;
        }

        int prev_x = parents[x][y][0];
        int prev_y = parents[x][y][1];
        res = par_path(sot_graph, parents, prev_x, prev_y);

        if(prev_x < x)
            res.addLast('D');
        else if(x < prev_x)
            res.addLast('U');
        else if(prev_y < y)
            res.addLast('R');
        else if(y < prev_y)
            res.addLast('L');

        return res;
    }

    public static void main(String[] args) throws Exception
    {
        if(args.length == 0){
            System.out.println("invalid usage: please give one input file");
            return;
        }

        StayHome mainObj = new StayHome();

        mainObj.readGraph(args[0]);
        mainObj.N = mainObj.graph.size();
        mainObj.M = mainObj.graph.get(0).size();

        //get the virus_graph deepcopy
        Vector< Vector<Integer> > virus_graph = new Vector< Vector<Integer> >();
        for(Vector<Integer> r : mainObj.graph){
            Vector<Integer> r_clone = new Vector<Integer>();
            for(int i : r){
                r_clone.add(i);
            }
            virus_graph.add(r_clone);
        }

        //visited is initialized to all 0
        Integer[][] visited = new Integer[mainObj.N][mainObj.M];
        for(int i = 0; i < mainObj.N; i++)
            for(int j = 0; j < mainObj.M; j++)
                visited[i][j] = 0;

        LinkedList<Integer[]> queue = new LinkedList<Integer[]>();
        Integer[] q_item = {mainObj.W[0],mainObj.W[1],0};
        queue.add(q_item);
        boolean[] found = new boolean[1];
        found[0] = false;
        while(queue.size() > 0)
            mainObj.virus_flood(virus_graph, queue, visited, found);

        Integer[][][] parents = new Integer[mainObj.N][mainObj.M][2];
        //get the sot_graph deepcopy
        Vector< Vector<Integer> > sot_graph = new Vector< Vector<Integer> >();
        for(Vector<Integer> r : mainObj.graph){
            Vector<Integer> r_clone = new Vector<Integer>();
            for(int i : r){
                r_clone.add(i);
            }
            sot_graph.add(r_clone);
        }

        queue = new LinkedList<Integer[]>();
        Integer[] new_q_item = {mainObj.S[0],mainObj.S[1],0,-1,-1};
        queue.add(new_q_item);

        // Visited is initialized to 0
        for(int i = 0; i < mainObj.N; i++)
            for(int j = 0; j < mainObj.M; j++)
                visited[i][j] = 0;

        while(queue.size() > 0)
            mainObj.sot_flood(sot_graph, virus_graph, queue, visited, parents);
        
        if(sot_graph.get(mainObj.T[0]).get(mainObj.T[1]) >= 0){
            // possible
            System.out.println(sot_graph.get(mainObj.T[0]).get(mainObj.T[1]));
            for(char c : mainObj.par_path(sot_graph, parents, mainObj.T[0], mainObj.T[1]))
                System.out.print(c);
            System.out.println();
        }
        else
            System.out.println("IMPOSSIBLE");
    }

}
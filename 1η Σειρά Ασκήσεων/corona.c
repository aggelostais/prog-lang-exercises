#include <stdlib.h>
#include <stdio.h>

void fill_cycle(int node, int * before, int * cycle_list)
{
        if(cycle_list[node] == 1) return;
        cycle_list[node] = 1;
        fill_cycle( before[node], before, cycle_list);
}

void dfs(int node, int ** graph, int size, int * visited, int * parent, int * before, int * cycle_list, int * count)
{
        visited[node] = 1;
        for(int i = 1; i <= size; i++)
        {
                if(graph[node][i] == 1 && visited[i] == 0)
                {
                        parent[i] = node;
                        before[i] = node;
                        dfs(i, graph, size, visited, parent, before, cycle_list, count);
                }
                else if(graph[node][i] == 1 && visited[i] == 1 && parent[node] != i)
                {//cycle found
                        before[i] = node;
                        (*count) += 1;
                        fill_cycle(i, before, cycle_list);

                }
        }
        visited[node] = 2;
}


//find_cycles returns the number of total cycles contained in a graph
//and in case the graph contains exactly one cycle,
//it fills array cycle with the nodes of the cycle
int find_cycles(int ** graph, int size, int * cycle_list)
{
        // parent[i] = j -> node i was visited right after node j
        int * parent = (int *)malloc( (size +1) * sizeof(int));
        int * before = (int *)malloc( (size +1) * sizeof(int));
        int * visited = (int *)malloc( (size +1) * sizeof(int));
		for(int i = 1; i <= size; i++)
        {
            parent[i] = 0;
            before[i] = 0;
		    visited[i] = 0;
        }
        //visited[i] = 0 -> not yet visited
        //visited[i] = 1 -> visited, but not closed
        //visited[i] = 2 -> visited and closed
        int count = 0;
        dfs(1, graph, size, visited, parent, 
		    before, cycle_list, &count);
        free(before);
        free(parent);
        free(visited);

        return count;
}

//Xrisimopoieitai gia DFS kai epistrofi arithmou paidion kath dentrou
//Ksekinaei apo mia riza,pairnei 1-d pinaka midenikon, grapho kai megethos grafou 
int tree_of_root(int root, int * visited, int ** graph, int size) 
{
    visited[root] = 1; //Episkepsi ekastote rizas
    int res = 1;       //arxikos metritis megethos graphou 1
    for(int i = 1; i <= size; i++) 
       {
        if( graph[root][i] == 1 && !visited[i])//Gia kathe paidi tis rizas (1h synthiki) 
		//pou den exo episkefthe (2i synthiki)
        res += tree_of_root(i, visited, graph, size); 
       }
    return res;
}

//arr should contain the cycle
//s should be the size of graph
//G is the graph
void get_trees(int ** graph, int *cycle_list, int size, int * trees)
{
        int * visited = (int *)malloc( (size + 1) * sizeof(int));

        for(int i = 1; i <= size; i++) visited[i] = cycle_list[i];

        for(int i = 1; i <= size; i++)
        {
                if(cycle_list[i] == 1)
		            {
                        trees[ tree_of_root(i, visited, graph, size) ] += 1;
                }

        }

        free(visited);
}


int corona(int ** graph, int size)
{
	int * cycle_list = (int *)malloc( (size +1) * sizeof(int));
	for(int i = 1; i <= size; i++)
		cycle_list[i] = 0;

	int total_cycles = find_cycles(graph, size, cycle_list);

	if(total_cycles != 1)
	{
		printf("NO CORONA\n");
		return 0;
	}

	int trees_counter = 0;
	for(int i = 1; i <= size; i++)
	{
		if(cycle_list[i] == 1) trees_counter++;
	}


	printf("CORONA %d\n", trees_counter);

  /*
  trees[i] periexei to plithos twn dentrwn megethous i
  */
	int * trees = (int *)malloc( (size +1) * sizeof(int));
	for(int i = 0; i <= size; i++)
		trees[i] = 0;

	get_trees(graph, cycle_list, size, trees);

	for(int i = 1; i <= size; i++)
  {
  		int amount = trees[i];
      if(amount > 0)
  		{
  			for(int j = 0; j < amount; j++)
  				printf("%d ", i);
  		}
  }

	printf("\n");

	free(trees);
	free(cycle_list);
}


void print_graph(int ** graph, int size)
{
	for(int i = 1; i <= size; i++)
	{
		for(int j = 1; j <= size; j++)
		{
			printf(" %d ", graph[i][j]);
		}
		printf("\n");
	}
}

int is_connected(int ** G, int size) //Pairnei to grafo kai to megethos tou
{
	int * visited = (int *)malloc( (size + 1) * sizeof(int)); //Ftiaxnei 1-d pinaka
	for(int i = 1; i <= size; i++) visited[i] = 0; //Arxikopoiisi pinaka me 0
	tree_of_root(1, visited, G, size); //Kalei ti tree_of_root pou tha kanei th diasxisi
	//ksekinontas apo to komvo 1 kanei DFS kai syblironontos to pinaka visited
	for(int i = 1; i <= size; i++) //An opoiosdipote komvos exei 0 epestrepse 0 (no corona)
		if(visited[i] == 0 ) return 0;
	free(visited);
	return 1;
}

int main(int argc, char ** argv) //Diavazei, friaxnei pinaka, efoson einai synektikos kalei corona
{
	if( argc != 2 )
        {
            printf("please give one input file\n");
            return 0;
        }
    FILE * file;  //typos dieythinsi se arxeio 
    file = fopen(argv[1], "r"); //anoixe to arxeio file pou dinetai os orisma 1 mono gia anagnosi
    if(file == NULL)
        {
            printf("error! opening the file failed\n");
            exit(1);
        }
	int amount_of_graphs;
	fscanf(file, "%d", &amount_of_graphs); //diabase sto arxeio mexri na breis akeraio
	for(int times = 0; times < amount_of_graphs; times++) //gia kathe grafo
	{
		int n, e; //komboi,akmes
		fscanf(file, "%d", &n);
		fscanf(file, "%d", &e);
		/*dynamically allocating n*n graph table
		 *graph[i][j] = 1 if there is an edge between nodes i and j
		 *otherwise
		 *graph[i][j] = 0
		 */
		int ** graph = (int **)malloc( (n+1) * sizeof(int *));
		for(int i = 1; i <= n; i++)
			graph[i] = (int *)malloc( (n+1) * sizeof(int));

		//filling graph with zeros
		for(int i = 1; i <= n; i++)
			for(int j = 1; j <= n; j++)
				graph[i][j] = 0;

		//reading edges
		for(int edges = 0; edges < e; edges++)
		{
			int i, j;
			fscanf(file, "%d", &i);
			fscanf(file, "%d", &j);
			graph[i][j] = 1;
			graph[j][i] = 1;
		}
		if( is_connected(graph, n) ) corona(graph, n); //An einai synektikos ektelese ton corona
		//allios typose no corona
		else printf("NO CORONA\n");
		//end
		for(int i = 1; i <= n; i++) //apeleutherosi xorou
			free(graph[i]);
		free(graph);
	}
	fclose(file);
	return 0;
}

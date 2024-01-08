(*Tha prepei h dfs call na gyrizei to count*)
fun find_cycles(graph,size,cycle_list)=
 let
    val parent=Array.array(size+1,0) 
    val before=Array.array(size+1,0)
    val visited=Array.array(size+1,0)
    val count=0;
  in dfs_call(1,graph,size,cycle_list,visited,
                 parent,before,count)
  end; 

fun dfs (node,graph,size,cycle_list,visited,
        parent,before,cycle list,count) =
       Array.update(visited,node, 1) 
       while i<1 do 
       ( i=i+1
         if(Array2.sub(graph,node,i)=1 andalso Array.sub(visited,i)
            andalso Array.sub(parent,i)<>i) 
          Array.update(before,i,node);
          count=count+1
          fill_cycle(i,before,cycle_list) )

fun tree_of_root(root, visited, graph, size) =
  let
      val res=1
      val i=1
      fun for_tree_of_root (root, visited, graph,size,i,res)=
         if(Array2.sub(graph,root,i)=1 
            andalso Array.sub(visited,i)=0 andalso i<size)
          res=res+for_tree_of_root(i,visited,graph,size,i+1);
         else ();

          )
           
    in
     Array.update(visited, root, 1)
     while i<size do 
     (i=i+1; 
      if(Array2.sub(graph,root,i)=1 andalso Array.sub(visited,i)=0)
      res=res+for_tree_of_root(root,visited,graph,size,i))
    end;

fun is_connected(graph, size) =
  let
      val visited = Array.array(size + 1, 0)
      val trees = tree_of_root(1, visited, graph, size)
      (*number of trees is not used here, we just need the side effect*)

      fun zero_exist(arr, i) =
        if(i = Array.length(arr)) then false
        else
          if(Array.sub(arr, i) = 0) then true
          else zero_exist(arr, i + 1)

    in
      not(zero_exist(visited, 1))
    end
    ;

fun print_graph(graph, size) =
  let
      fun print_next(graph, size, i, j) =
        if (j = size + 1) then ()
        else
          (
            print(Int.toString( Array2.sub(graph, i, j) ) );
            print " ";
            print_next(graph, size, i, j + 1)
            )

      fun print_row(graph, size, 0) = ()
        | print_row(graph, size, i) =
          if (i = size + 1) then ()
          else
          (
            print_next(graph, size, i, 1);
            print "\n"
            )

      fun for_rows(graph, size, i) =
          if(i = size + 1) then ()
          else
            (
              print_row(graph, size, i);
              for_rows(graph, size, i + 1)
              )
    in
      for_rows(graph, size, 1)
    end
    ;

fun get_int stream =
  Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) stream)
  ;

fun coronagraph(infile : string) = (*Pairnei os orisma to onoma tou arxeiou*)
        let
          val fstream = TextIO.openIn infile  (*fstream= "metabliti" arxeio*)
          (* TextIO.openIn infile open the file named name for input.
          If name is a relative pathname, the file opened depends on the current working directory.*)
          (* val amount_of_graphs = get_int fstream*)
          fun fill_edges (0, graph) = graph (*Synartisi epanalipsis pou eisagei tis akmes*)
            | fill_edges (i, graph) =       (*i o arithmos akmon*)
            (
             let
                 val node_i = get_int fstream (*Painrei to proto akeraio tis akmis*)
                 val node_j = get_int fstream
               in
                 Array2.update (graph, node_i, node_j, 1); (*Bazei to komvo sto pinaka*)
                 Array2.update (graph, node_j, node_i, 1);
                 fill_edges (i-1, graph)
               end
             )

          val N = get_int fstream
          val M = get_int fstream
          val graph = fill_edges(M, Array2.array(N + 1, N + 1, 0))
        in
          is_connected(graph,n)
          print_graph(graph, N)
        end;

coronagraph "input.txt";

cd C:\Users\aggel\Documents\Σχολή\6ο Εξάμηνο\(Λ) Γλώσσες Προγραμματισμού Ι\Σειρές Ασκήσεων\1η Σειρά
sml <corona_adjlist.sml 


(*De tha xreiastei, definition of the adjacecy list type*)
(*type vertex = int*)
(*type graph = (vertex * vertex list) list*)


val a=Array.fromList[[4,5,7,9],[5],[6],[1,5,6],[1,2,4],[3,4],[1],[9],[8,5,1,3]]; 
val b=Array.fromList[[4,5,7,9],[5],[6],[1,5,6],[1,2,4],[3,4],[1],[9],[1,8]];
val c=Array.fromList[[],[],[],[]];
corona(5,[[2,3],[1,3],[1,2,3,4],[3,5],[3,4]],[2,2,4,2,2],0); 
corona(5,[[2,3],[1,3],[1,2],[5],[4]],[2,2,2,1,1],2);
corona (6,[[2,3,4],[1,3,5],[1,2,6],[1],[2],[3]],[3,3,3,1,1,1],3); 

(*Metatrepei ton int array list se int list list*)
(*n arithmos komvon,teliki int array list,[]*)
fun convert(1,array_list,list_list)=
    Array.sub(array_list,0)::list_list
    |convert(n,array_list,list_list)=
    convert(n-1,array_list,Array.sub(array_list,n-1)::list_list);

 (*Diorthosi: Na mis psaxnei se ola, na pigainei stoxeymena*)
(*Diagraphei to komvo bazontas sth thesi tou mia keni lista, n komvous synolika exoume,i metritis*) (*N*) 
(*Elegxithike gia 0 i 1*) 
fun  delete_node_graph(n,komvos,graph)=
     let 
     fun delete_loop(i,n,komvos,graph)=
     if(i<n) then
     (Array.update(graph,i-1,List.filter(fn x => x<>komvos) (Array.sub(graph,i-1))); delete_loop(i+1,n,komvos,graph))
     else 
      (Array.update(graph,i-1,List.filter(fn x => x<>komvos) (Array.sub(graph,i-1))); graph)
      in 
       (Array.update(graph,komvos-1,[]); delete_loop(1,n,komvos,graph))
       end;
   

fun get_int stream =
  Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) stream);

fun coronagraph(infile : string) = (*Pairnei os orisma to onoma tou arxeiou*)
        let
          val fstream = TextIO.openIn infile  (*fstream= "metabliti" arxeio*)
          (* TextIO.openIn infile open the file named name for input.
          If name is a relative pathname, the file opened depends on the current working directory.*)
          (* val amount_of_graphs = get_int fstream*)
          fun fill_edges (0, graph) = graph (*Synartisi epanalipsis pou eisagei tis akmes*)
            | fill_edges (m, graph) =       (*m o arithmos akmon*)
            (
             let
                 val node_i = get_int fstream (*Painrei to proto akeraio tis akmis*)
                 val node_j = get_int fstream
               in
                 fill_edges (m-1,insert_edge(node_j,node_i,insert_edge(node_i,node_j,graph)))
               end
             )

          val N = get_int fstream (*Arithmos komvon*)
          val M = get_int fstream (*Arithmos akmon*)
          val graph = sort_graph(M,fill_edges(M,create_graph(N))) (*H arxiki klisi ths fill_edges
            tha parei orisma keni int list list megethous N*)
          val llinital=list_length(N,graph)

        in
          corona(N,graph,llinitial,count_ones(llinital))
          print_graph(graph, N)
        end;
 coronagraph "input.txt";       


(*fun delete_node_listslength (1,n,komvos,lists_length)=lists_length
   |delete_node_listslength (i,n,komvos,head::tail)=
      if (i=(n-komvos+1)) then (delete_node_listslength(i-1,n,komvos,tail))
      else  
*) 

(*fun cycle_node_tree(current_node,graph,cycle_list,helping_list)= *)

(mini_help_list(9,9,create_list(9),[4,1,1,3,3,2,1,1,2],[[4,5,7,9],[5],[6],[1,5,6],[1,2,4],[3,4],[1],[9],[1,8]]);
mini_help_list(9,9,[2,0,0,1,2,2,0,0,2],[3,0,0,3,2,1,0,0,1],[[4,5,9],[],[],[1,5,6],[1,4],[4],[],[],[1]]);
[4,0,0,3,2,0,0,0,0] 
mini_help_list(5,5,create_list(5),[2,2,2,1,1],[[2,3],[1,3],[1,2],[5],[4]]); 
        
val graph1=[[4,5,7,9],[5],[6],[1,5,6],[1,2,4],[3,4],[1],[9],[1,8]];
val graph2=[[2,5],[1,3],[2,4],[3,5],[1,4]];
val graph3=[[4,5],[4,6],[5,6],[1,2],[1,3],[2,3]];
val graph4=[[2,3],[1,3],[1,2],[5,6],[4,6],[4,5]];
val graph5=[[2,3],[1,3],[1,2]]; 
is_connected(1,graph1,create_list_for_connected(1,9),create_list_g(9,0),give_list(1,1,graph1)); 

val graph1=[[4,5,7,9],[5],[6],[1,5,6],[1,2,4],[3,4],[1],[9],[1,8]];
val graph2=[[4,5],[4,6],[5,6],[1,2],[1,3],[2,3]];
val graph3=[[2,3],[1,3],[1,2,3,4],[3,5],[3,4]];
val graph4=[[2,3],[1,3],[1,2],[5],[4]];
val graph5=[[2,3,4],[1,3],[1,2,5],[1],[3],[]];
val graph6=[[2,3],[1,3],[1,2],[]];
val graph7=[[2,3],[1,3],[1,2],[5,6],[4,6],[4,5]];
val graph8=[[5,8],[8],[4,9],[3,6,7,9],[1],[4,7],[4,6],[1,2],[3,4]];

corona2(9,graph1,graph1,[4,1,1,3,3,2,1,1,2],4,create_list(9)); (*ekfonisi aksisis [2,3,4]*) 
corona2(6,graph2,graph2,[2,2,2,2,2,2],0,create_list(6)); (*Paradeigma 2 trigonon []*)
corona2(5,graph3,graph3,[2,2,4,2,2],0,create_list(5));  (*Paradeigma 3 [] *)
corona2(5,graph4,graph4,[2,2,2,1,1],2,create_list(5)); (*[]*) 
corona2(6,graph5,graph5,[3,2,3,1,1,0],2,create_list(6)); (*[]*)
corona2(4,graph6,graph6,[2,2,2,0],0,create_list(4)); (*[]*)
corona2(6,graph7,graph7,[2,2,2,2,2,2],0,create_list(6)); (*[]*) 
corona2(9,graph8,graph8,lists_length(9,graph8),count_ones(lists_length(9,graph8)),create_list(9));

val b=Array.fromList[[4,5,7,9],[5],[6],[1,5,6],[1,2,4],[3,4],[1],[9],[1,8]];
val c=Array.fromList[1,1,1,1,1,1,1,1,1];
replace_value(2,c,b);


mini_help_array(9,9,Array.array(9,1),Array.fromList[4,1,1,3,3,2,1,1,2],Array.fromList[[4,5,7,9],[5],[6],[1,5,6],[1,2,4],[3,4],[1],[9],[1,8]]);
mini_help_array(9,9,Array.fromList[2,0,0,1,2,2,0,0,2],Array.fromList[3,0,0,3,2,1,0,0,1],Array.fromList[[4,5,9],[],[],[1,5,6],[1,4],[4],[],[],[1]]);
[4,0,0,3,2,0,0,0,0] 
mini_help_list(5,5,create_list(5),[2,2,2,1,1],[[2,3],[1,3],[1,2],[5],[4]]); 


is_connected(1,b,array_for_connected(9),Array.array(9,0),Array.sub(b,0))
[|2,2,2,1,1,1,1,2,1|]
val initial_graph=[[4,5,7,9],[5],[6],[1,5,6],[1,2,4],[3,4],[1],[9],[1,8]];
is_connected(1,initial_graph,
create_list_for_connected(1,9),create_list_g(9,0),give_list(1,1,initial_graph));


val graph1=Array.fromList[[4,5,7,9],[5],[6],[1,5,6],[1,2,4],[3,4],[1],[9],[1,8]]; (*ekfonisi aksisis [2,3,4]*) 
val graph2=Array.fromList[[4,5],[4,6],[5,6],[1,2],[1,3],[2,3]]; (*Megalos kyklos corona graph [1,1,..*)
val graph3=Array.fromList[[2,3],[1,3],[1,2,3,4],[3,5],[3,4]]; (*Paradeigma 2 trigonon []*)
val graph4=Array.fromList[[2,3],[1,3],[1,2],[5],[4]]; (*[]*) 
val graph5=Array.fromList[[2,3,4],[1,3],[1,2,5],[1],[3],[]]; (*[]*)
val graph6=Array.fromList[[2,3],[1,3],[1,2],[]]; (*[]*)
val graph7=Array.fromList[[2,3],[1,3],[1,2],[5,6],[4,6],[4,5]]; (*[]*) 
val graph8=Array.fromList[[5,8],[8],[4,9],[3,6,7,9],[1],[4,7],[4,6],[1,2],[3,4]];

corona(9,graph1,graph1,Array.fromList[4,1,1,3,3,2,1,1,2],4,Array.array(9,1)); 
corona(6,graph2,graph2,Array.fromList[2,2,2,2,2,2],0,Array.array(6,1)); 
corona(5,graph3,graph3,Array.fromList[2,2,4,2,2],0,Array.array(5,1));  
corona(5,graph4,graph4,Array.fromList[2,2,2,1,1],2,Array.array(5,1); 
corona(6,graph5,graph5,Array.fromList[3,2,3,1,1,0],2,Array.array(6,1)); 
corona(4,graph6,graph6,Array.fromList[2,2,2,0],0,Array.array(4,1)); 
corona(6,graph7,graph7,Array.fromList[2,2,2,2,2,2],0,Array.array(6,1)); 
corona(9,graph8,graph8,array_length(9,graph8),count_ones(array_length(9,graph8)),Array.array(9,1));

(print_list (arrayToList connected_array); connected_array)
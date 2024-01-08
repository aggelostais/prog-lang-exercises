fun count_ones [] = 0 (*An parei keni lista epistrefei 0*) (*N*)
  | count_ones (head::tail) = 
        if head = 1 then 1 + count_ones tail
        else count_ones tail;

fun count_zeros[]=0
|count_zeros (head::tail)=
    if head=0 then 1+count_zeros tail
    else count_zeros tail;

fun only_2s []=true
    |only_2s(head::tail)=
    if head=2 then (only_2s tail)
    else (false);

(*Taksinomisi liston entos grafou*) (*N*)
(*Elexthike gia 0 i 1*)
fun sort_graph(0,graph)=graph 
|   sort_graph(n,head::tail)=
 let
    fun sort_list (list)= 
        ListMergeSort.sort (fn (s, t) => s > t) list
 in
  sort_list(head)::sort_graph(n-1,tail)
 end;

(*Briskei to athroisma ton stoixeion tis listas mexri auti na teleiosi
komvoi,lista.De ginetai elegxos*)
fun  sum_of_list(1,head::[])= head 
    |sum_of_list(n,head::tail)=head+sum_of_list(n-1,tail);

(*Synartisi dimiourgias kenis int list list*) (*N*)
fun create_graph(0)=[]
|create_graph(n)=[]::create_graph(n-1);

(*Dimiourgeia listas 1 n stoixeion*)
fun create_list(1)=[1]
|create_list(n)=1::create_list(n-1);

(*Dimiourgia listas timis value, n stoixeion*)
fun create_list_g(1,value)=[value]
|create_list_g(n,value)=value::create_list_g(n-1,value);

fun create_list_for_connected(1,n)=
    if (n=1) then [1] else 1::create_list_for_connected(2,n)
   |create_list_for_connected(i,n)=
   if(i=n) then [0] else 0::create_list_for_connected(i+1,n);

fun final_corona_list(0::tail)=final_corona_list(tail)
    |final_corona_list(list)=list;

(*Eisagei ataskinomita ta stoixeia sth lista kai tin epistrefei*) (*N*)
(*Orismata i,j,mideniki lista*)
(*Elegxthike gia 0 i 1*)
fun insert_edge(1,j,head::tail)= 
   (j::head)::tail
  | insert_edge(i,j,head::tail)= 
   head::insert_edge(i-1,j,tail);

(*Diagraphei to komvo bazontas sth thesi tou mia keni lista, n komvous synolika exoume,i metritis*) (*N*) 
(*Elegxithike gia 0 i 1*)
fun  delete_node_graph(0,n,komvos,graph)=
      if (0=(n-komvos+1)) then [[]]     
      else (graph)
    |delete_node_graph(i,n,komvos,head::tail)=
        if (i=(n-komvos+1)) then []::(delete_node_graph(i-1,n,komvos,tail))
        else
           (List.filter (fn x => x<>komvos) head)::(delete_node_graph(i-1,n,komvos,tail));

(*Ftiaxnei ton neo grapho meta apo 1 gyro daigrafon komvon (*N*)
i metritis,n arithmos komvon,graph,lists_length 
sto opoio tha basistei kai ton opoio diatrexei(den prerei na allazei kata th diarkeia kai den to allazei*)
(*Isos xreiazetai 0 anti gia 1*)
fun mini_corona (1,n,graph,[1])=delete_node_graph(n,n,n,graph)
 |mini_corona (1,n,graph,lists_length)=graph
 |mini_corona (i,n,graph,1::tail)=
    mini_corona(i-1,n,delete_node_graph(n,n,n-i+1,graph),tail)
 |mini_corona(i,n,graph,head::tail)= mini_corona(i-1,n,graph,tail);

(*Pairnei ton arithmo ton komvon kai grapho kai epistrefei ena int list me ton arithmo geitonon kathe komvou*) (*N*)
(*Exei elegthi gia 0 i 1*)
fun  lists_length (1,[graph])=[List.length graph]
    |lists_length (n,head::tail)=
        (List.length head)::lists_length(n-1,tail);

(*Pairnei arithmo komvon kai to grapho kai epistrefei ton arithmo ton kenon liston*) (*N*)

fun empty_lists(1,[[]])=1
    |empty_lists(1,tail)=0
    |empty_lists(n,[]::tail)= 1+empty_lists(n-1,tail)
    |empty_lists(n,head::tail)=empty_lists(n-1,tail);

(*Ftiaxnei meta th teliki epeksergasia pinaka komvon pou symmetexoun syo kyklo. (*N*)
 Tha yparxei 0 sto pinaka an no-corona.
Pairnei metriti i=komvoi, komvoi,to teliko lists_length meta thn epeksergasia, epistrefei int list me to kyklo
An den yparxei keni lista*)
fun  corona_cycle(1,n,[0])=[]
      |corona_cycle(1,n,[2])=[n]
      |corona_cycle(1,n,last_element)=[0]
      |corona_cycle(i,n,0::tail)=corona_cycle(i-1,n,tail)
      |corona_cycle(i,n,2::tail)=(n-i+1)::corona_cycle(i-1,n,tail)
      |corona_cycle(i,n,lists_length)=[0];

(*Pairnei to pinaka tis corona_cycle kai 
epistrefei keni lista an no-corona lista komvon pou symmetexoun an corona*) (*N*)
fun is_corona([],list,counter)=[]
    |is_corona([0],list,counter)=[]
    |is_corona([a],list,counter)=
      if (counter+1)>=3 then list else []
    |is_corona(0::tail,list,counter)=[]
    |is_corona(head::tail,list,counter)=is_corona(tail,list,counter+1);


(*Eisagontas metriti i=1,ena komvo diagrafis kai to grafo epistrefei to komvo me ton opoio syndeotan*)
fun give_capitalist_node(i,komvos,last::[])= List.nth(last,0)
  |give_capitalist_node(i,komvos,head::tail)=
    if(i=komvos) then List.nth(head,0)  
    else 
        (give_capitalist_node(i+1,komvos,tail));

(*metritis i,komvos pou tha diagrafei,help list pairnei apo to komvo diagrafis th timi tou*) (*N*)
fun take_deleted_value(i,komvos,head::[])=head
    |take_deleted_value(i,komvos,head::tail)=
    if(i=komvos) then head
    else 
        take_deleted_value(i+1,komvos,tail);

(*Xrisimopoieitai gia th topothetisi 0 sto bohthitiko pinaka        (*N*)
kai thn prostthesi sto ofeloumeno komvo, epistrefei to pinaka meta thn allagi*)
(*metritis i=1,timi antikatastasis value,komvos antikatastasis,lista*) 
fun replace_value(i,0,komvos,head::[])=[0]
   |replace_value(i,0,komvos,head::tail)=
    if(i=komvos) then 0::tail
        else head::replace_value(i+1,0,komvos,tail)
   |replace_value(i,value,komvos,head::[])=[value+head]
   |replace_value(i,value,komvos,head::tail)=
        if(i=komvos) then (head+value)::tail
        else head::replace_value(i+1,value,komvos,tail);

(*fun cycle_node_tree(current_node,graph,cycle_list,helping_list)= *)

(*Epistrefei ton aneomeno pinaka help list meta apo 1 gyro diagrafon
metriris i*) 
(*metritis i=komvoi,n=komvoi,help_list,lists_length,grafos*) (*N*)
 fun mini_help_list (1,n,help_list,[1],graph)=
      replace_value(1,take_deleted_value(1,n,help_list),give_capitalist_node(1,n,graph),replace_value(1,0,n,help_list))
 |mini_help_list (1,n,help_list,last_element,graph)=help_list
 |mini_help_list(i,n,help_list,1::tail,graph)=
    mini_help_list(i-1,n,
    replace_value(1,take_deleted_value(1,n-i+1,help_list),give_capitalist_node(1,n-i+1,graph),replace_value(1,0,n-i+1,help_list)),
    tail,graph)
 |mini_help_list(i,n,help_list,head::tail,graph)= mini_help_list(i-1,n,help_list,tail,graph);


(*Pairnei os orisma ton arxiko grapho,arxiko list_length*)
(*n arithmos komvon,assoi= arithmos asson tou lists_length*)
fun corona(n,graph,listslength,0)=
    is_corona(corona_cycle(n,n,listslength),corona_cycle(n,n,listslength),0)
   |corona(n,graph,listslength,assoi)=
      (let 
          val new_graph=mini_corona(n,n,graph,listslength)
          val new_listslength=lists_length(n,new_graph)
      in
        if (empty_lists(n,new_graph)=assoi) then  (*diorthosi*)
              corona(n,new_graph,new_listslength,count_ones(new_listslength))
        else [] 
      end);

(*Dinei th lista enos komvou, metritis i=1, komvos stoxos, grafos*) (*N*)
fun give_list(i,komvos,last::[])= last
  |give_list(i,komvos,head::tail)=
    if(i=komvos) then head
    else 
        (give_list(i+1,komvos,tail));

fun replace_list(i,value,komvos,head::[])=[value]
   |replace_list(i,value,komvos,head::tail)=
    if(i=komvos) then value::tail
        else head::replace_list(i+1,value,komvos,tail);

(* give_list(1,komvos,graph) *)
(*Tsekarei synektikotita: komvos episkepsis, telikos epeksergasmenos grafos,lista synektikotitas kai lista geitonon kathe komvou (head einai to paidi se kathe ektelesi)*)
fun is_connected(1,graph,connected_list,parent_list,head::[])=
  let     
          val child_value=List.nth(connected_list,head-1) (*Katastasi komvou head sti connected_list*)
          val child_connected_list=replace_list(1,1,head,connected_list)
          val child_parent_list=replace_list(1,1,head,parent_list) (*Nea parent list pou tha parei to paidi*)
          val adj_child_list=give_list(1,head,graph)
      in
        if(child_value=0) then is_connected(head,graph,child_connected_list,child_parent_list,adj_child_list)
        else replace_list(1,2,1,connected_list)
      end
  |is_connected(komvos,graph,connected_list,parent_list,head::[])= (*An exoume ftasei se ena komvo pou exei mono to gonio 
                                                                      sto kosmo i gia auto to komvo teleiosan ta paidia tou*)
     let val parent=List.nth(parent_list,komvos-1) (*Gonios tou trexontos komvou*)
         val child_value=List.nth(connected_list,head-1) (*Katastasi komvou head sti connected_list*)
         val parent_connected_list=replace_list(1,2,komvos,connected_list) (*Oristikopoiimeno connected list gia paidi pou epistrefei se gonio*)
         val adj_parent_list=give_list(1,parent,graph) (*Lista geitonon tou goniou*)
         val child_connected_list=replace_list(1,1,head,connected_list)
         val child_parent_list=replace_list(1,komvos,head,parent_list) (*Nea parent list pou tha parei to paidi*)
         val adj_child_list=give_list(1,head,graph)
     in
       if(child_value=0) then is_connected(head,graph,child_connected_list,child_parent_list,adj_child_list)
       else (is_connected(parent,graph,parent_connected_list,parent_list,adj_parent_list))
     end
    |is_connected(komvos,graph,connected_list,parent_list,head::tail)=
      let val parent=List.nth(parent_list,komvos-1) (*Gonios tou trexontos komvou*)
         val child_value=List.nth(connected_list,head-1) (*Katastasi komvou head sti connected_list*)
         val parent_connected_list=replace_list(1,2,komvos,connected_list) (*Oristikopoiimeno connected list gia paidi pou epistrefei se gonio*)
         val adj_parent_list=give_list(1,parent,graph) (*Lista geitonon tou goniou*)
         val child_connected_list=replace_list(1,1,head,connected_list)
         val child_parent_list=replace_list(1,komvos,head,parent_list) (*Nea parent list pou tha parei to paidi*)
         val adj_child_list=give_list(1,head,graph)
     in
        if(child_value=0) then is_connected(head,graph,child_connected_list,child_parent_list,adj_child_list)
       else is_connected(komvos,graph,connected_list,parent_list,tail)
      end;

(*Tsekarei an einai corona i oxi kai epistrefei ta plithi ton komvon sta dentra se ayksousa seira*)
(*An no-corona keni lista*)
(*n=arithmos komvon,grafos,grafos,lists_length(grafos),count_ones(lists_length(grafos)),create_list(arithmos komvon))*)
fun corona2(n,graph,initial_graph,listslength,0,help_list)=
     if ((n=sum_of_list(n,help_list)) andalso 
         (empty_lists(n,graph)=count_zeros(help_list)) andalso
         (if(help_list=create_list(n)) then (only_2s listslength) else true) 
        (* andalso count_zeros(is_connected(1,initial_graph,create_list_for_connected(1,n),create_list_g(n,0),give_list(1,1,initial_graph)))=0*)
        ) 
         then final_corona_list(ListMergeSort.sort (fn (s, t) => s > t) help_list)
      else []
   |corona2(n,graph,initial_graph,listslength,assoi,help_list)=
      (let 
          val new_graph=mini_corona(n,n,graph,listslength)
          val new_listslength=lists_length(n,new_graph)
          val new_helplist=mini_help_list(n,n,help_list,listslength,graph)
      in
          if (empty_lists(n,new_graph)=count_zeros(new_helplist)) then
              corona2(n,new_graph,initial_graph,new_listslength,count_ones(new_listslength),new_helplist)
          else []
      end);

(*
* from
* https://stackoverflow.com/questions/29809722/reading-an-integer-file-to-an-integer-list-in-sml
* *)
fun get_int stream =
  Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) stream);

(*Ektyponei ta stoixeia listas me ena keno anamesa kai sto telos allagi grammis*)
fun print_list [] = print "" 
  | print_list l =
        let
                fun help_print_list ([], ishead) = () 
                  | help_print_list ((h::t), true) =
                         (
                         print( Int.toString(h) );
                         help_print_list(t, false)
                          )
                  | help_print_list ((h::t), false) =
                         (
                         print " ";
                         print( Int.toString(h) );
                         help_print_list(t, false)
                         )
        in 
        (
         help_print_list ( l, true );
         print "\n"
         )
        end;



fun print_graph ([])= 
  (print("NO CORONA"); print ("\n"))
|print_graph (list)=
  (print("CORONA ") ; 
   print(Int.toString(List.length list));
   print ("\n");
   print_list(list));

fun coronograph(infile : string) = (*Pairnei os orisma to onoma tou arxeiou*)
        let
          val fstream = TextIO.openIn infile  (*fstream= "metabliti" arxeio*)
          (* TextIO.openIn infile open the file named name for input.
          If name is a relative pathname, the file opened depends on the current working directory.*)
          val number_of_graphs = get_int fstream
          fun for 0 = ()
            | for i =
            (let 
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
                tha parei orisma keni int list list megethous N*)(*To proto orisma mallon eprepe na einai N*)
              val llinitial=lists_length(N,graph)
              val corona_corona=corona2(N,graph,graph,llinitial,count_ones(llinitial),create_list(N))
            in 
              (print_graph(corona_corona); 
               for (i-1))
            end
           )
        in
          for(number_of_graphs)
        end;
coronograph("graphs.txt");
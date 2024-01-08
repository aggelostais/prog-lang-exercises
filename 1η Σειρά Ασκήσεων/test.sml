(*int list->int*)
fun count_ones [] = 0 (*An parei keni lista epistrefei 0*) (*N*)
  | count_ones (head::tail) = 
        if head = 1 then 1 + count_ones tail
        else count_ones tail;

(*int list->int*)
fun count_zeros[]=0
    |count_zeros (head::tail)=
      if head=0 then 1+count_zeros tail
      else count_zeros tail;

(*int list->bool, blepei an h lista exei mono 2ria*)
fun only_2s []=true
    |only_2s(head::tail)=
    if head=2 then (only_2s tail)
    else (false);

(*Taksinomisi liston entos grafou (int list array)*) 
(* arithmos komvon,grafos*)
(*Elexthike gia 0 i 1*)
fun sort_graph(0,graph)=graph 
|   sort_graph(n,graph)=
 let
    fun sort_list (list)= 
        ListMergeSort.sort (fn (s, t) => s > t) list
 in
  (Array.update(graph,n-1,sort_list(Array.sub(graph,n-1)));
  sort_graph(n-1,graph))
 end;

(*Briskei to athroisma ton stoixeion tis listas mexri auti na teleiosi
komvoi,lista.De ginetai elegxos*)
fun  sum_of_list(1,head::[])= head 
    |sum_of_list(n,head::tail)=head+sum_of_list(n-1,tail);

(*Dimiourgeia listas n asson*)
fun create_list(1)=[1]
|create_list(n)=1::create_list(n-1);

(*Dimiourgia kenou int list array, orisma megethos*)
fun create_list_array(n)=
    Array.array(n,[]);
 
(*Eisagogi akmis se int list array kai stous 2 komvous*)
(*epistrefei to int list array*)
fun insert_edge2(i,j,array_list)=
   (Array.update(array_list,i-1,j::Array.sub(array_list,i-1));
    Array.update(array_list,j-1,i::Array.sub(array_list,j-1));
    array_list);

(*Dimiourgia listas timis value, n stoixeion*)
fun create_list_g(1,value)=[value]
|create_list_g(n,value)=value::create_list_g(n-1,value);

(*Dimiourgia listas 1,0,0,.. gia thn connected, n synolikos arithmos stoixeion*)
fun create_list_for_connected(1,n)=
    if (n=1) then [1] else 1::create_list_for_connected(2,n)
   |create_list_for_connected(i,n)=
   if(i=n) then [0] else 0::create_list_for_connected(i+1,n);

(*Diagrafei ena komvo apo to grafo,epistrefontas auton.
Prosoxi: Allazi to grafo pou pernei os orisma, an klithi
 ksana o grafos pou dothike os orisma tha einai allagmenos
komvos diagrafis,grafos*)
fun delete_node2(komvos,graph)=
  let 
   val list=Array.sub(graph,komvos-1)
   fun  delete_loop(komvos,[],graph)=(Array.update(graph,komvos-1,[]); graph)
       |delete_loop(komvos,head::tail,graph)=
       (Array.update(graph,head-1,List.filter(fn x => x<>komvos) (Array.sub(graph,head-1))); delete_loop(komvos,tail,graph))
    in 
    delete_loop(komvos,list,graph)
    end;
  
(*Pairnei ton arithmo ton komvon kai grapho kai epistrefei ena int list me ton arithmo geitonon kathe komvou*) (*N*)
(*Arithmos komvon,graph*)
fun list_length(n,graph)=
let 
  fun  lists_length (1,graph)=[List.length (Array.sub(graph,0))]
      |lists_length (n,graph)=
          ((List.length (Array.sub(graph,n-1)))::lists_length(n-1,graph))
in List.rev (lists_length(n,graph))
end;

(*Pairnei arithmo komvon kai to grapho kai epistrefei ton arithmo ton kenon liston*) 
(*Prosoxi:De douleyei an o pinakas exei oles tis listes kenes*)
fun empty_lists(1,graph)=
      if(Array.sub(graph,0)=[]) then 1
      else 0
    |empty_lists(n,graph)= 
    if(Array.sub(graph,n-1)=[]) then (1+empty_lists(n-1,graph))
    else empty_lists(n-1,graph)

(*Otan enas komvos prokeitai na diagrafei ston algorithmo menei me ena komvo sth lista tou*)
(*Epistrefei auto to komvo, komvos diagrafis,grafos*)
fun eunooumenos_komvos(komvos,graph)=
 let val a=Array.sub(graph,komvos-1)
 in (List.nth(a,0))
 end;

(*metritis i,komvos pou tha diagrafei,
help list pairnei apo to komvo diagrafis th timi tou*) (*N*)
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


(*Epistrefei ton aneomeno pinaka help list meta apo 1 gyro diagrafon
metriris i*) 
(*metritis i=komvoi,n=komvoi,help_list,lists_length,grafos*) (*N*)
 fun mini_help_list (1,n,help_list,[1],graph)=
      replace_value(1,take_deleted_value(1,n,help_list),eunooumenos_komvos(1,n,graph),replace_value(1,0,n,help_list))
 |mini_help_list (1,n,help_list,last_element,graph)=help_list
 |mini_help_list(i,n,help_list,1::tail,graph)=
    mini_help_list(i-1,n,
    replace_value(1,take_deleted_value(1,n-i+1,help_list),eunooumenos_komvos(1,n-i+1,graph),replace_value(1,0,n-i+1,help_list)),
    tail,graph)
 |mini_help_list(i,n,help_list,head::tail,graph)= mini_help_list(i-1,n,help_list,tail,graph);


(*Dinei th lista enos komvou, metritis i=1, komvos stoxos, grafos*) (*N*)
fun give_list(i,komvos,last::[])= last
  |give_list(i,komvos,head::tail)=
    if(i=komvos) then head
    else 
        (give_list(i+1,komvos,tail));

(*Allazei th timi tou komvou mias listas se value, i metritis*)
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
         andalso count_zeros(is_connected(1,initial_graph,create_list_for_connected(1,n),create_list_g(n,0),give_list(1,1,initial_graph)))=0
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

(* from https://stackoverflow.com/questions/29809722/reading-an-integer-file-to-an-integer-list-in-sml *)
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
             print(Int.toString(h));
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
                        fill_edges(m-1,insert_edge2(node_i,node_j,graph))
                    end
                    )
              val N = get_int fstream (*Arithmos komvon*)
              val M = get_int fstream (*Arithmos akmon*)
              val graph = fill_edges(M,create_list_array(N))
              val graph2=sort_graph(N,convert(N,graph,[]))(*H arxiki klisi ths fill_edges
                tha parei orisma keni int list list megethous N*)
              val llinitial=lists_length(N,graph2) 
              val corona_corona=corona2(N,graph2,graph2,llinitial,count_ones(llinitial),create_list(N)) 
            in 
             (
               print_graph(corona_corona); 
               for (i-1)
             )
            end
           )
        in
          for(number_of_graphs)
        end;
coronograph("graphs.txt");
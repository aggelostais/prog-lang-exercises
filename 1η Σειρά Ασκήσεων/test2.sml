(*Metraei posa stoixeia timis value exoume se pinaka 1D*)
(* diastasi pinaka,timi,pinakas*)
fun count_values(1,value,array)=
    if(Array.sub(array,0)=value) then 1 else 0
    |count_values(n,value,array)=
        if(Array.sub(array,n-1)=value) 
            then 1+count_values(n-1,value,array)
        else count_values(n-1,value,array);

(*Bool an exei mono value o pinakas*)
(* diastasi pinaka,timi,pinakas*)
fun only_value(n,value,array)=
    if(count_values(n,value,array)=n) then true
    else false;

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

 (*Briskei to athroisma ton stoixeion tou pinaka*)
 (*arithmos stoixeion,pinakas*)
 fun sum_of_array(1,array)= Array.sub(array,0)
    |sum_of_array(n,array)= 
     Array.sub(array,n-1)+sum_of_array(n-1,array);

(*Dimiourgia pinaka n timis value Array.array(n,value)*)

(*Dimiourgia kenou int list array, orisma megethos*)
fun create_list_array(n)=
    Array.array(n,[]);

(*Eisagogi akmis se int list array kai stous 2 komvous*)
(*epistrefei to int list array*)
fun insert_edge2(i,j,array_list)=
   (Array.update(array_list,i-1,j::Array.sub(array_list,i-1));
    Array.update(array_list,j-1,i::Array.sub(array_list,j-1));
    array_list);

(*Dimiourgia pinaka gia connected opou to 1 einai sthn thesi  
 n synolikos arithmos stoixeion*)
fun array_for_connected(n,thesi)=
    let val arr=Array.array(n,0)
    in (Array.update(arr,thesi-1,1); arr)
    end;

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
fun array_length(n,graph)=
let 
 val arr=Array.array(n,0)
  fun  main_job(1,arr,graph)=
       (Array.update(arr,0,List.length(Array.sub(graph,0))); arr)
      |main_job(n,arr,graph)=
        (Array.update(arr,n-1,List.length(Array.sub(graph,n-1)));
        main_job(n-1,arr,graph))
in main_job(n,arr,graph)
end;  

(*Pairnei arithmo komvon kai to grapho kai epistrefei ton arithmo ton kenon liston*) 
(*Prosoxi:De douleyei an o pinakas exei oles tis listes kenes*)
fun empty_lists(1,graph)=
      if(Array.sub(graph,0)=[]) then 1
      else 0
    |empty_lists(n,graph)= 
    if(Array.sub(graph,n-1)=[]) then (1+empty_lists(n-1,graph))
    else empty_lists(n-1,graph);

(*take_deleted_value=Array.sub(help_array,komvos)*)

(*Epeksergasia help_array: topothetisi 0 sto komvo pou diagrafetai  
 eyresi kai prostthesi ths aksias tou sto ofeloumeno komvo, 
epistrefei help_array meta thn allagi*)
(*timi antikatastasis value,komvos antikatastasis,lista*) 

(*eunooumenos_komvos(komvos,graph): Otan enas komvos prokeitai 
na diagrafei ston algorithmo menei me ena komvo sth lista tou*)
(*Epistrefei auto to komvo, komvos diagrafis,grafos*)

fun replace_value(komvos,help_array,graph)=
let 
    fun eunooumenos_komvos(komvos,graph)=
       ( let val a=Array.sub(graph,komvos-1)
        in (List.nth(a,0))
        end
       )
   val eunooumenos=eunooumenos_komvos(komvos,graph)
   val value=Array.sub(help_array,komvos-1)
in 
    (Array.update(help_array,eunooumenos-1,
     Array.sub(help_array,eunooumenos-1)+value);
     Array.update(help_array,komvos-1,0); help_array)
end;

(*Epistrefei ton aneomeno pinaka help_array meta apo 1 gyro diagrafon*)
(*Briskei ola ta 1 sto array_length kai ta eisagei os orisma sto replace value*)
(* metritis=komvoi,komvoi,help_array,array length,grafos*)
fun  mini_help_array(1,n,help_array,array_length,graph)=
  if (Array.sub(array_length,0)=1)
        then replace_value(1,help_array,graph)
        else help_array
    |mini_help_array(i,n,help_array,array_length,graph)=
        if (Array.sub(array_length,i-1)=1)
        then mini_help_array(i-1,n,replace_value(i,help_array,graph),array_length,graph)
        else mini_help_array(i-1,n,help_array,array_length,graph);

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

fun arrayToList arr = Array.foldr (op ::) [] arr;

fun final_corona_list(0::tail)=final_corona_list(tail)
    |final_corona_list(list)=list;

(*Dinei th lista enos komvou*)
(*(val give_list=Array.sub(graph,komvos)*)

(*Allazei th timi se ena pinaka se value*)
(* Array.update(array,komvos,value)*)

(*Tsekarei synektikotita: komvos episkepsis, arxikos grafos,
lista synektikotitas kai lista geitonon kathe komvou 
(head einai to paidi se kathe ektelesi)*)
(* 1,graph,array_for_connected(n),Array.array(n,0),Array.sub(graph,0)*)

fun is_connected(1,graph,connected_array,parent_array,[])= connected_array
    |is_connected(1,graph,connected_array,parent_array,head::[])=
   let 
       val child_value=Array.sub(connected_array,head-1)
   in 
       if(child_value=0) 
       then 
           (Array.update(connected_array,head-1,1);
           Array.update(parent_array,head-1,1); 
           is_connected(head,graph,connected_array,parent_array,Array.sub(graph,head-1)))
       else (Array.update(connected_array,0,2); connected_array)
   end
|is_connected(komvos,graph,connected_array,parent_array,head::[])=
    let 
        val child_value=Array.sub(connected_array,head-1)
    in 
       if(child_value=0) then 
            (
            Array.update(connected_array,head-1,1);
            Array.update(parent_array,head-1,komvos);
            is_connected(head,graph,connected_array,parent_array,Array.sub(graph,head-1))
            )
       else 
            (
            Array.update(connected_array,komvos-1,2);
            is_connected(Array.sub(parent_array,komvos-1),
            graph,connected_array,parent_array,Array.sub(graph,(Array.sub(parent_array,komvos-1))-1))
            )
    end
|is_connected(komvos,graph,connected_array,parent_array,head::tail)=
      let 
        val child_value=Array.sub(connected_array,head-1) (*Pare to torino paidi*)
    in 
       if(child_value=0) then  (*An den exoume episkeftei to paidi*)
            (Array.update(connected_array,head-1,1);
             Array.update(parent_array,head-1,komvos);
            is_connected(head,graph,connected_array,parent_array,Array.sub(graph,head-1))
            )
       else  
        is_connected(komvos,graph,connected_array,parent_array,tail) (*An exoume episkeftei/oloklirosei to trexon paidi
        proxora sto epomeno paidi*)
    end;

(*val graph4 = Array.fromList [[2,3],[1,3],[1,2],[5],[4]];
is_connected(1,graph4,array_for_connected(5),Array.array(5,0),Array.sub(graph4,0));
*)

(*Theloume na mhn allazei to grafo*)
(*Efarmozei 1 gyro graigrafon ston grafo*)
(* kovmoi,komvoi,grafos,array_length*)
fun mini_corona (1,n,graph,array_length)=
    if ((Array.sub(array_length,n-1))=1) then delete_node2(n,graph)
    else graph
 |mini_corona (i,n,graph,array_length)=
    if ((Array.sub(array_length,n-i))=1) then mini_corona(i-1,n,delete_node2(n-i+1,graph),array_length)
    else mini_corona(i-1,n,graph,array_length);

(*Tsekarei an einai corona i oxi kai epistrefei ta plithi ton komvon sta dentra se ayksousa seira*)
(*An no-corona keni lista*)
(*n=arithmos komvon,grafos,grafos,lists_length(grafos),count_ones(lists_length(grafos)),create_list(arithmos komvon))*)
 (*andalso count_values(n,0,is_connected(1,initial_graph,array_for_connected(n),Array.array(n,0),Array.sub(initial_graph,0)))=0 *)

fun corona(n,graph,initial_graph,arrayslength,0,help_array)=
     if (n=sum_of_array(n,help_array)) andalso 
         (empty_lists(n,graph)=count_values(n,0,help_array)) andalso 
         (if(only_value(n,1,help_array)) then (only_value(n,2,arrayslength)) else true) 
         then 
         final_corona_list(ListMergeSort.sort (fn (s, t) => s > t) (arrayToList(help_array)))
      else  [] 
   |corona(n,graph,initial_graph,arrayslength,assoi,help_array)=
      (let 
          val new_helparray= mini_help_array(n,n,help_array,arrayslength,graph) (*help_array*)
          val new_graph=mini_corona(n,n,graph,arrayslength) (*graph;*)
      in
          if (empty_lists(n,new_graph)=count_values(n,0,new_helparray)) then
              corona(n,new_graph,initial_graph,(array_length(n,new_graph)),count_values(n,1,(array_length(n,new_graph))),new_helparray)
          else  []
      end);
   
(* from https://stackoverflow.com/questions/29809722/reading-an-integer-file-to-an-integer-list-in-sml *)
fun get_int stream =
  Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) stream);


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
              val graph2=sort_graph(N,graph)
              (*H arxiki klisi ths fill_edges
                tha parei orisma keni int list list megethous N*)
              val llinitial=array_length(N,graph2) 
              val corona_corona=
              (if(count_values(N,0,is_connected(1,graph2,array_for_connected(N,1),Array.array(N,0),Array.sub(graph2,0)))=0)
                  then corona(N,graph2,graph2,llinitial,count_values(N,1,llinitial),Array.array(N,1)) else []) 
                 (* corona(N,graph2,graph2,llinitial,count_values(N,1,llinitial),Array.array(N,1)) *)
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
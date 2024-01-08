cd C:\Users\aggel\Documents\Σχολή\6ο Εξάμηνο\(Λ) Γλώσσες Προγραμματισμού Ι\Σειρές Ασκήσεων\2η Σειρά Ασκήσεων\Stay Home
timecmd sml <stay_home.sml 

sml <stay_home.sml "stayhome.in17.txt"


(*
fun readlist (infile : string) =  
  let
      val fstream = TextIO.openIn infile 
      fun loop indata = 
          case TextIO.inputLine indata of 
              SOME line => line :: loop indata 
            | NONE      => []
      val result = loop fstream
  in 
     TextIO.closeIn ins;
     result
  end;
*)
(*
(*Agnoei to \n*)
  fun readlist file =
let
    fun next_String input = (TextIO.inputAll input) 
    val stream = TextIO.openIn file
    val a = next_String stream (*string*)
in
    print(a);
    explode(a) (*To metatrepei se char list*)
end;

fun string_to_map (text)=
*)
 (*
fun readlist (infile : string) =  
  let
      val fstream = TextIO.openIn infile 
        fun loop indata = 
          case TextIO.inputLine indata of 
              SOME line => explode(indata)
            | NONE      => []
    in 
    print(loop fstream)
    end;

readlist("stayhome.in1.txt");*)


(*from https://stackoverflow.com/questions/3918288/turning-a-string-into-a-char-list-list-using-sml *)
open Char
open String
open List

fun linelist file =
    let val instr = TextIO.openIn file
        val str   = TextIO.inputAll instr
    in tokens isSpace str
       before
       TextIO.closeIn instr
    end;

fun getsudo file   = map explode (linelist file);

fun  main args = 
   getsudo "stayhome.in1.txt";

val map=linelist("stayhome.in1.txt");


(*Tha klithei converter(map,map_x,map_y,map_string,([],0,0,0,0))*)
fun  converter(array,x_length,y,[],(airport_list,S_x,S_y,T_x,T_y))= 
    (airport_list,S_x,S_y,T_x,T_y)
    |converter(array,x_length,y,head::tail,(airport_list,S_x,S_y,T_x,T_y))=
    let 
      val line=explode(head) (*lista symboseiron mias grammis*)
      fun  what_char(array,1,x_length,y,[],(airport_list,S_x,S_y,T_x,T_y))=
            (airport_list,S_x,S_y,T_x,T_y)
          |what_char(array,counter,x_length,y,head::tail,(airport_list,S_x,S_y,T_x,T_y))=
          if(head=String.sub("S",0)) then 
            ( Array2.update(array,x_length-counter,y-1,~1); 
              what_char(array,counter-1,x_length,y,tail,(airport_list,x_length-counter,y,T_x,T_y)) )
          else if(head=String.sub("T",0)) then 
            ( Array2.update(array,x_length-counter,y-1,~2);
              what_char(array,counter-1,x_length,y,tail,(airport_list,S_x,S_y,x_length-counter,y)) )
          else if(head=String.sub("X",0)) then 
             ( Array2.update(array,x_length-counter,y-1,~3);
              what_char(array,counter-1,x_length,y,tail,(airport_list,S_x,S_y,T_x,T_y)) )
          else if(head=String.sub("A",0)) then 
              ( Array2.update(array,x_length-counter,y-1,~4);
              what_char(array,counter-1,x_length,y,tail,([x_length-counter,y]::airport_list,S_x,S_y,T_x,T_y)) )
          else 
              what_char(array,counter-1,x_length,y,tail,(airport_list,S_x,S_y,T_x,T_y)) 
        val tuple=what_char(array,x_length,x_length,y,line,(airport_list,S_x,S_y,T_x,T_y))
    in
      converter(array,x_length,y+1,tail,tuple)
    end;


(*val map_x=List.length(explode(List.hd(map_string))); (*diastasi x*)
val map_y=List.length map_string;     (*diastasi y*) *)
(*val map=Array2.array(map_x,map_y,0); (*Arxikos kenos xartis*) *)

(*
fun stay_home(infile : string) = (*Pairnei os orisma to onoma tou arxeiou*)
        let
          val fstream = TextIO.openIn infile  (*fstream= "metabliti" arxeio*)
          (* TextIO.openIn infile open the file named name for input.
          If name is a relative pathname, the file opened depends on the current working directory.*)
          val line=TextIO.inputLine fstream
          val line_list=explode(line)
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
                        fill_edges(m~1,insert_edge2(node_i,node_j,graph))
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
               for (i~1)
             )
            end
           )
        in
          for(number_of_graphs)
        end;
coronograph("graphs.txt");

*)

(*Efarmozei to flood fill tou iou ston pinaka virus_time*)
(*Kaleitai os flood_fill_virus((W_grammi,W_stili,0),Q,map,map_grammes-1,map_stiles-1,virus_times,airport_list);*)
fun flood_fill_virus((W_grammi,W_stili,counter),Q,map,map_grammes,map_stiles,virus_times,airport_list)= 
   if (Queue.isEmpty(Q) andalso counter<>0) then ()
   else
    (
      let
         fun conditions((grammi,stili,counter),Q,map,map_grammes,map_stiles,virus_times)=   
         (*stili,grammi,counter: pou tha ginei eggrafi*)
          if(grammi>=0 andalso (grammi <= map_grammes)  andalso stili>=0 andalso (stili <= map_stiles)  andalso
            (Array2.sub(map,grammi,stili) <> String.sub("X",0)) andalso
            (Array2.sub(map,grammi,stili)<> String.sub("W",0))  andalso
            ((Array2.sub(virus_times,grammi,stili))=0  orelse (Array2.sub(virus_times,grammi,stili)>counter))
            )
          then
            ( (*print(Int.toString(grammi+1)^" "^Int.toString(stili+1)^" "^Int.toString(counter)^" added to queue."^"\n");*)
             Queue.enqueue(Q,(grammi,stili,counter)); Array2.update(virus_times,grammi,stili,counter)
            )
          else ((*print(Int.toString(grammi+1)^" "^Int.toString(stili+1)^" "^Int.toString(counter)^" not added to queue."^"\n")*))
        (*val next_element= Queue.dequeue(Q); *)
        fun conditions_airports(counter,Q,map,map_grammes,map_stiles,virus_times,[])= () (*Prepei na dothei to counter eggrafis gia ta aerodromia*)
          |conditions_airports(counter,Q,map,map_grammes,map_stiles,virus_times,(grammi,stili)::tail)=
            (conditions((grammi,stili,counter),Q,map,map_grammes,map_stiles,virus_times);
            conditions_airports(counter,Q,map,map_grammes,map_stiles,virus_times,tail)) 
      in
        (
        if (airport_list=[]) then  (*Otan exoume epeksergastei ta aerodormia*)
          ( (*Array2.update(virus_times,W_grammi,W_stili,counter);*)
            conditions((W_grammi+1,W_stili,counter+2),Q,map,map_grammes,map_stiles,virus_times);
            conditions((W_grammi,W_stili-1,counter+2),Q,map,map_grammes,map_stiles,virus_times);
            conditions((W_grammi,W_stili+1,counter+2),Q,map,map_grammes,map_stiles,virus_times);
            conditions((W_grammi-1,W_stili,counter+2),Q,map,map_grammes,map_stiles,virus_times);
            flood_fill_virus(Queue.dequeue(Q),Q,map,map_grammes,map_stiles,virus_times,[])
          )
        else (*Otan den exoume epeksergastei ta aerodromia*)
          ( (*Array2.update(virus_times,W_grammi,W_stili,counter);*)
            conditions((W_grammi+1,W_stili,counter+2),Q,map,map_grammes,map_stiles,virus_times);
            conditions((W_grammi,W_stili-1,counter+2),Q,map,map_grammes,map_stiles,virus_times);
            conditions((W_grammi,W_stili+1,counter+2),Q,map,map_grammes,map_stiles,virus_times);
            conditions((W_grammi-1,W_stili,counter+2),Q,map,map_grammes,map_stiles,virus_times);
            conditions_airports(counter+5,Q,map,map_grammes,map_stiles,virus_times,airport_list);
            flood_fill_virus(Queue.dequeue(Q),Q,map,map_grammes,map_stiles,virus_times,[])
          )
        )
    end
    );





fun find_path(T_grammi,T_stili,counter)= 
   if (counter=0) then ""
   else
    (
      let
         fun conditions(grammi,stili,counter,prev_grammi,prev_stili)=   
            (*stili,grammi,counter: pou einai ypopsifio na mpei sto path*)
              if(grammi>=0 andalso (grammi <= map_grammes-1)  andalso 
                stili>=0 andalso (stili <= map_stiles-1)  andalso (*Entos orion*)
                (Array2.sub(map,grammi,stili) <> String.sub("X",0)) andalso 
                (Array2.sub(map,grammi,stili)<> String.sub("W",0))  andalso
                (counter=Array2.sub(sotiris_times,grammi,stili) andalso 
                 ))   (*Einai to proigoumeno bima tis diadromis*)
              then true
              else false
          val down=conditions(T_grammi-1,T_stili,counter-1)
          val left=conditions(T_grammi,T_stili+1,counter-1)
          val right=conditions(T_grammi,T_stili-1,counter-1)
          val up=conditions(T_grammi+1,T_stili,counter-1)
      in
        if(down=true) then "D"^(find_path(T_grammi-1,T_stili,counter-1))
        else if(left=true) then "L"^(find_path(T_grammi,T_stili+1,counter-1))
        else if(right=true) then "R"^(find_path(T_grammi,T_stili-1,counter-1))
        else if(up=true) then "U"^(find_path(T_grammi+1,T_stili,counter-1))
        else "IMPOSSIBLE"
      end
    );


    

(*Synartisi Ektiposis int 2D Array*)
fun print_2D (array,grammi,stili,limit_grammi,limit_stili)=
    let 
      val print_value=Int.toString(Array2.sub(array,grammi,stili))
    in 
      if (String.size(print_value)=1) then 
       (
        if(grammi=limit_grammi andalso stili=limit_stili)  then
            print(" "^(Int.toString(Array2.sub(array,grammi,stili)))^" "^"\n")
        else if (stili=limit_stili) then
            (print(" "^(Int.toString(Array2.sub(array,grammi,stili)))^" "^"\n");
              print_2D(array,grammi+1,0,limit_grammi,limit_stili))
        else if(stili=0) then
            (print(Int.toString(Array2.sub(array,grammi,stili))^" ");
              print_2D(array,grammi,stili+1,limit_grammi,limit_stili))     
        else
          (print(" "^(Int.toString(Array2.sub(array,grammi,stili))^" "));
          print_2D(array,grammi,stili+1,limit_grammi,limit_stili))
       )
       else 
       (
        if(grammi=limit_grammi andalso stili=limit_stili)  then
            print(" "^(Int.toString(Array2.sub(array,grammi,stili)))^"\n")
        else if (stili=limit_stili) then
            (print(" "^(Int.toString(Array2.sub(array,grammi,stili)))^"\n");
              print_2D(array,grammi+1,0,limit_grammi,limit_stili))
        else if(stili=0) then
            (print(Int.toString(Array2.sub(array,grammi,stili)));
              print_2D(array,grammi,stili+1,limit_grammi,limit_stili))     
        else
          (print(" "^(Int.toString(Array2.sub(array,grammi,stili))));
          print_2D(array,grammi,stili+1,limit_grammi,limit_stili))
       )
    end;

(*
print_2D_lists(parent_array,0,0,map_grammes-1,map_stiles-1);
*)

(*print_2D(sotiris_times,0,0,map_grammes-1,map_stiles-1);*)


(*Synartisi Ektiposis int list 2D Array*)
fun print_2D_lists(array,grammi,stili,limit_grammi,limit_stili)=
    let 
      val [i,j]=Array2.sub(array,grammi,stili)
      val print_value="["^(Int.toString(i+1))^","^(Int.toString(j+1))^"]"
    in 
       (
        if(grammi=limit_grammi andalso stili=limit_stili)  then
            print(" "^print_value^" "^"\n")
        else if (stili=limit_stili) then
            (print(" "^print_value^" "^"\n");
              print_2D_lists(array,grammi+1,0,limit_grammi,limit_stili))
        else if(stili=0) then
              (print(print_value^" ");
              print_2D_lists(array,grammi,stili+1,limit_grammi,limit_stili))     
        else
          (print(" "^print_value^" ");
          print_2D_lists(array,grammi,stili+1,limit_grammi,limit_stili))
       )
    end;

(*(print("Virus Time Map:"^"\n");
print_2D(virus_times,0,0,map_grammes-1,map_stiles-1); print("\n"));*)
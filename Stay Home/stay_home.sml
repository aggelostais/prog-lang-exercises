(*Start:from https://stackoverflow.com/questions/3918288/turning~a~string~into~a~char~list~list~using~sml *)
(*Diabazei to arxeio kai ftiaxnei string list, me kathe grammi na einai ena string*)

fun linelist file =
    let val instr = TextIO.openIn file
        val str   = TextIO.inputAll instr
    in String.tokens Char.isSpace str
       before
       TextIO.closeIn instr
    end;

fun getsudo file = map explode (linelist file);
(*
val [args] = CommandLine.arguments();
*)
val map_string=linelist("input.txt");

(*End:from https://stackoverflow.com/questions/3918288/turning~a~string~into~a~char~list~list~using~sml *) 



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




(*Ftiaxnei to 2D-Array apo char me ton xarti*)
fun  converter ([])=[]
    |converter (head::tail)=explode(head)::converter(tail);

val map=Array2.fromList (converter(map_string));
val map_grammes= #1 (Array2.dimensions(map)); (*Theorontas oti arxizoume arihmisi apo to 0*)
val map_stiles= #2 (Array2.dimensions(map));  (*Theorontas oti arxizoume arihmisi apo to 0*)

(*Pairnei string list kai ta bazei sto pinaka,kai mas epistrefei ti lista me ta aerodromia kai S,T*)
(*Tha klithei converter2(map,map_grammes,map_stiles,map_stiles,([],0,0,0,0))*)
fun  converter2(array,x_length,y_length,0,(airport_list,S_x,S_y,T_x,T_y,W_x,W_y))= (*Sto 0 den ekteleitai*)
    (airport_list,S_x,S_y,T_x,T_y,W_x,W_y)
    |converter2(array,x_length,y_length,counter,(airport_list,S_x,S_y,T_x,T_y,W_x,W_y))=
    let 
      val current_y=y_length-counter
      fun  what_char(array,0,x_length,y,(airport_list,S_x,S_y,T_x,T_y,W_x,W_y))=
            (airport_list,S_x,S_y,T_x,T_y,W_x,W_y)
          |what_char(array,counter,x_length,y,(airport_list,S_x,S_y,T_x,T_y,W_x,W_y))=
          if(Array2.sub(array,x_length-counter,y)=String.sub("S",0)) then 
              what_char(array,counter-1,x_length,y,(airport_list,x_length-counter,y,T_x,T_y,W_x,W_y))
          else if(Array2.sub(array,x_length-counter,y)=String.sub("T",0)) then 
              what_char(array,counter-1,x_length,y,(airport_list,S_x,S_y,x_length-counter,y,W_x,W_y))
          else if(Array2.sub(array,x_length-counter,y)=String.sub("W",0)) then 
              what_char(array,counter-1,x_length,y,(airport_list,S_x,S_y,T_x,T_y,x_length-counter,y))
          else if(Array2.sub(array,x_length-counter,y)=String.sub("A",0)) then 
              what_char(array,counter-1,x_length,y,((x_length-counter,y)::airport_list,S_x,S_y,T_x,T_y,W_x,W_y))
          else 
              what_char(array,counter-1,x_length,y,(airport_list,S_x,S_y,T_x,T_y,W_x,W_y))    
        val tuple=what_char(array,x_length,x_length,current_y,(airport_list,S_x,S_y,T_x,T_y,W_x,W_y))
    in
      converter2(array,x_length,y_length,counter-1,tuple)
    end;

val tuple_result=converter2(map,map_grammes,map_stiles,map_stiles,([],0,0,0,0,0,0));
val S_grammi = #2 tuple_result;
val S_stili = #3 tuple_result;
val T_grammi = #4 tuple_result;
val T_stili = #5 tuple_result;
val W_grammi = #6 tuple_result;
val W_stili = #7 tuple_result;
val airport_list= #1 tuple_result;
(*Mexri edo exoume to xarti kai tis critical topothesies*)

val virus_times=Array2.array(map_grammes,map_stiles,0);

(*Efarmozei to flood fill tou iou ston pinaka virus_time*)
(*Kaleitai os flood_fill_virus((W_grammi,W_stili,0),Q,map,map_grammes-1,map_stiles-1,virus_times,airport_list);*)
fun flood_fill_virus((W_grammi,W_stili,counter),Q,map,map_grammes,map_stiles,virus_times,airport_list)= 
   if (Queue.isEmpty(Q) andalso counter<>0) then ()
   else
    (
      let
         fun conditions((grammi,stili,counter),Q,map,map_grammes,map_stiles,virus_times,airport_list)=   
           let
               fun conditions_airports(counter,Q,map,map_grammes,map_stiles,virus_times,[])= () (*Prepei na dothei to counter eggrafis gia ta aerodromia*)
                  |conditions_airports(counter,Q,map,map_grammes,map_stiles,virus_times,(grammi,stili)::tail)=
                      (conditions((grammi,stili,counter),Q,map,map_grammes,map_stiles,virus_times,[]);
                      conditions_airports(counter,Q,map,map_grammes,map_stiles,virus_times,tail)) 
            in 
                (*stili,grammi,counter: pou tha ginei eggrafi*)
                if(grammi>=0 andalso (grammi <= map_grammes)  
                  andalso stili>=0 andalso (stili <= map_stiles)  andalso
                  (Array2.sub(map,grammi,stili) <> String.sub("X",0)) andalso
                  (Array2.sub(map,grammi,stili)<> String.sub("W",0))  andalso
                  ((Array2.sub(virus_times,grammi,stili))=0  orelse (Array2.sub(virus_times,grammi,stili)>counter))
                  )
                then
                  ( 
                    if(airport_list<>[] andalso Array2.sub(map,grammi,stili)=String.sub("A",0)) (*An ftasame se aerodromio*)
                    then 
                      ( (*print(Int.toString(grammi+1)^" "^Int.toString(stili+1)^" "^Int.toString(counter)^" added to queue."^"\n");*)
                        Queue.enqueue(Q,(grammi,stili,counter)); 
                        Array2.update(virus_times,grammi,stili,counter);
                        conditions_airports(counter+5,Q,map,map_grammes,map_stiles,virus_times,airport_list);
                        true
                      )
                    else
                      ((*print(Int.toString(grammi+1)^" "^Int.toString(stili+1)^" "^Int.toString(counter)^" added to queue."^"\n");*)
                        Queue.enqueue(Q,(grammi,stili,counter)); 
                        Array2.update(virus_times,grammi,stili,counter);
                        (*(print("Virus Time Map:"^"\n");
                         print_2D(virus_times,0,0,map_grammes,map_stiles); print("\n"));*)
                        false
                      )
                  )
                else ((*print(Int.toString(grammi+1)^" "^Int.toString(stili+1)^" "^Int.toString(counter)^" not added to queue."^"\n");*)false)   
            end 
          val D=conditions((W_grammi+1,W_stili,counter+2),Q,map,map_grammes,map_stiles,virus_times,airport_list)
          val L=conditions((W_grammi,W_stili-1,counter+2),Q,map,map_grammes,map_stiles,virus_times,airport_list)
          val R=conditions((W_grammi,W_stili+1,counter+2),Q,map,map_grammes,map_stiles,virus_times,airport_list)
          val U=conditions((W_grammi-1,W_stili,counter+2),Q,map,map_grammes,map_stiles,virus_times,airport_list)
          val _=Queue.dequeue(Q); (*Afairoume to trexon stoixeio apo ti kefali*)
      in
        (
         if (Queue.isEmpty(Q)) then ()
         else if (D=true orelse L=true orelse R=true orelse U=true) (*Petixame aerodromio ara teleiosame*)
            then  flood_fill_virus(Queue.head(Q),Q,map,map_grammes,map_stiles,virus_times,[])         
         else 
            flood_fill_virus(Queue.head(Q),Q,map,map_grammes,map_stiles,virus_times,airport_list)         
        ) 
    end
    );

(*Dimiourgia ouras gia flood_fill_virus*)
val Q=Queue.mkQueue():(int*int*int) Queue.queue;
val _=Queue.enqueue(Q,(W_grammi,W_stili,0)); 

flood_fill_virus((W_grammi,W_stili,0),Q,map,map_grammes-1,map_stiles-1,virus_times,airport_list);     

val sotiris_times=Array2.array(map_grammes,map_stiles,0);
val parent_array=Array2.array(map_grammes,map_stiles,[0,0]);

fun flood_fill_sotiris((S_grammi,S_stili,counter),Q,map,map_grammes,map_stiles,sotiris_times,virus_times,parent_array)= 
   if (Queue.isEmpty(Q) andalso counter<>0) then ()
   else
    (
      let
         fun conditions((grammi,stili,counter),Q,map,map_grammes,map_stiles,sotiris_times,virus_times,parent_array,parent)=   
            (*stili,grammi,counter: pou tha ginei eggrafi*)
              if(grammi>=0 andalso (grammi <= map_grammes)  andalso stili>=0 andalso (stili <= map_stiles)  andalso (*Entos orion*)
                (Array2.sub(map,grammi,stili) <> String.sub("X",0)) andalso 
                (Array2.sub(map,grammi,stili)<> String.sub("W",0))  andalso
                (counter<Array2.sub(virus_times,grammi,stili))      andalso (*Den exei ftasei prota o ios*)
                (((Array2.sub(sotiris_times,grammi,stili))=0 andalso Array2.sub(map,grammi,stili) <> String.sub("S",0))
                    orelse (Array2.sub(sotiris_times,grammi,stili)>counter)) (*Isos xreiazetai diorthosi*)
                )
              then
                ( print(Int.toString(grammi+1)^" "^Int.toString(stili+1)^" "^Int.toString(counter)^" added to queue."^"\n");
                  
                Queue.enqueue(Q,(grammi,stili,counter)); Array2.update(sotiris_times,grammi,stili,counter);
                Array2.update(parent_array,grammi,stili,parent);
                print_2D(sotiris_times,0,0,map_grammes,map_stiles);
                print_2D_lists(parent_array,0,0,map_grammes,map_stiles)
                )
              else ((print(Int.toString(grammi+1)^" "^Int.toString(stili+1)^" "^Int.toString(counter)^" not added to queue."^"\n")))
          val _=Queue.dequeue(Q); (*Afairoume to trexon stoixeio apo ti kefali*)
      in
          ( 
            conditions((S_grammi+1,S_stili,counter+1),Q,map,map_grammes,map_stiles,sotiris_times,virus_times,parent_array,[S_grammi,S_stili]);
            conditions((S_grammi,S_stili-1,counter+1),Q,map,map_grammes,map_stiles,sotiris_times,virus_times,parent_array,[S_grammi,S_stili]);
            conditions((S_grammi,S_stili+1,counter+1),Q,map,map_grammes,map_stiles,sotiris_times,virus_times,parent_array,[S_grammi,S_stili]);
            conditions((S_grammi-1,S_stili,counter+1),Q,map,map_grammes,map_stiles,sotiris_times,virus_times,parent_array,[S_grammi,S_stili]);
            if (Queue.isEmpty(Q)) then ()
            else flood_fill_sotiris(Queue.head(Q),Q,map,map_grammes,map_stiles,sotiris_times,virus_times,parent_array)
          )
      end
    );

val _=Queue.enqueue(Q,(S_grammi,S_stili,0)); 
flood_fill_sotiris((S_grammi,S_stili,0),Q,map,map_grammes-1,map_stiles-1,sotiris_times,virus_times,parent_array);   
print_2D_lists(parent_array,0,0,map_grammes-1,map_stiles-1);


fun find_path(T_grammi,T_stili,parent_array,counter)= 
   if (counter=0) then ()
   else
    (
      let 
        val [parent_grammi,parent_stili]=Array2.sub(parent_array,T_grammi,T_stili)
      in 
        if(parent_grammi+1=T_grammi andalso parent_stili=T_stili) then (find_path(T_grammi-1,T_stili,parent_array,counter-1); print("D"))
        else if(parent_grammi=T_grammi andalso parent_stili-1=T_stili) then (find_path(T_grammi,T_stili+1,parent_array,counter-1); print("L"))
        else if(parent_grammi=T_grammi andalso parent_stili+1=T_stili) then (find_path(T_grammi,T_stili-1,parent_array,counter-1); print("R"))
        else if(parent_grammi-1=T_grammi andalso parent_stili=T_stili) then (find_path(T_grammi+1,T_stili,parent_array,counter-1); print("U"))
        else print("IMPOSSIBLE")
      end
    );

fun print_result()=
let 
  val path_length=Array2.sub(sotiris_times,T_grammi,T_stili);
in
  if(path_length=0) then print("IMPOSSIBLE"^"\n")
  else (print(Int.toString(Array2.sub(sotiris_times,T_grammi,T_stili))^"\n");
        find_path(T_grammi,T_stili,parent_array,path_length); print("\n"))
end;

print_result();
(*
val _ = OS.Process.exit(OS.Process.success); *)
(*
* use
* -main_powers(input_file);
* to get the result for data contained in
* input_file
*)

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
                         print ",";
                         print( Int.toString(h) );
                         help_print_list(t, false)
                         )
        in
        (
         print "[";
         help_print_list ( l, true );
         print "]\n"
         )
        end;
(*Metraei posa 1 yparxoun se lista*) (*N*)
fun count_ones [] = 0 (*An parei keni lista epistrefei 0*)
  | count_ones (h::t) =
        if h = 1 then 1 + count_ones t
        (*Allios an to head einai epistrefei 1+anadromiki ektelesi tou eautpu tis gia thn oyra*)
        else count_ones t
        ;

(*Pairnei enan akeraio kai epistrefei ANAPODA
ti dyadiki tou anaparastasi se pinaka*) (*N*)
fun tobin(n) =
  if n <= 0 then []
  else
    (n mod 2) :: tobin(n div 2)
  ;

(*Pairnei lista kai epistrefei ti thesi sth lista tou protou mh 0 stoixeiou*) (*N*)
(*H arithmisi ton theseon arxizei apo to 1*)
fun first_non_zero [] = 0
  | first_non_zero (h::t) =
        if h > 0 then 1
        else 1 + first_non_zero(t)
        ;


fun help_break ([], index) = [] (*An parei keni lista epistrefei keni*) (*N*)
  | help_break ((h::t), index)=
      if index <= 0 then h::t (*An index arnitikos opos einai i lista*)
       else  if index = 1 then (h-1)::t (*An index=1 tou afairei 1*)
        else if index = 2 then (h+2)::help_break(t, 1) (*An index=2 to auksanei kata 2 kai
                                                        efasmozei stin ypoloipi theorontas to epomeno value 1*)
          else h::help_break(t, index-1);              (*Proxoraei mexri na ftasei sto 2*)


(*breakin(dyadiki anaparstasi,k(dynameis tou 2)-arithmos_1_sti_dyadiki) (*N*)
Epistrefei ton pinaka apotelesma anapoda *)
fun breakin (l, times) =
  if times <= 0 then l
  else
    let
      val index = 1 + first_non_zero(tl l)
    in
      breakin(help_break(l, index), times - 1)
    end
    ;

(* Kobei ta midenika apo to telos*)
fun print_correct [] = ()
  | print_correct l =
        if( hd(l) = 0 ) then print_correct ( tl(l) )
        else print_list (rev l)
        ;

fun powers (n, k) =
        if k > n then print "[]\n"
        else
          let
            val bin_digits = tobin(n)
            val ones = count_ones(bin_digits)
          in
            if k < ones then print "[]\n"
            else
                print_correct( rev( breakin(bin_digits, k - ones)) )
          end
        ;

(*
* from
* https://stackoverflow.com/questions/29809722/reading-an-integer-file-to-an-integer-list-in-sml
* *)
fun get_int stream =
  Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) stream)
  ;

(*Diabazei kathe grammi tou arxeiou kai ektelei tin powers*)
fun main_powers(infile : string) =
        let
          val fstream = TextIO.openIn infile
          val lines = get_int fstream

          fun for 0 = ()
            | for i =
            (
             powers(get_int fstream, get_int fstream);
             for (i-1)
             )
        in
          for(lines)
        end;
main_powers("input.txt");

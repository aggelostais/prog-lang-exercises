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

fun count_ones [] = 0
  | count_ones (h::t) =
        if h = 1 then 1 + count_ones t
        else count_ones t
        ;

fun tobin(n) =
  if n <= 0 then []
  else
    (n mod 2) :: tobin(n div 2)
  ;

fun first_non_zero [] = 0
  | first_non_zero (h::t) =
        if h > 0 then 1
        else 1 + first_non_zero(t)
        ;

fun help_break ([], index) = []
  | help_break ((h::t), index)=
      if index <= 0 then h::t
      else
        if index = 1 then (h-1)::t
        else
          if index = 2 then (h+2)::help_break(t, 1)
          else
            h::help_break(t, index-1)
         ;


fun breakin (l, times) =
  if times <= 0 then l
  else
    let
      val index = 1 + first_non_zero(tl l)
    in
      breakin( help_break(l, index), times - 1)
    end
    ;

(* print_correct should take as arg a reversed list*)
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

fun powers2(infile : string) =
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
powers2()
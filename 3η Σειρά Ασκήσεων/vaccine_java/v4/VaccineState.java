import java.io.*;
import java.util.*;

public class VaccineState implements State{
    private List<Character> first,moves;
    private List<Integer> second;
    private List<Boolean> acgu;

    @Override
    public List<Character> Moves(){
        return moves;
    }
    //Constructor
    public VaccineState(List<Character> a,List<Integer> b,
                        List<Boolean> c, List<Character> d ){
        first=a;
        second=b;
        acgu=c;
        moves=d;
    }
    //Euresi lisis
    @Override
    public boolean success(){
        return first.size()==0;
    }

    @Override
    public boolean equals(Object o) { 
        // If the object is compared with itself then return true   
        if (o == this) return true; 
        if (o == null || getClass() != o.getClass()) return false;
        VaccineState c = (VaccineState) o; //typecast o to State gia na sygkrinoume ta data members
        return c.first==first && c.second==second && c.acgu==acgu;
    }

    @Override
    public int hashCode() {
      return Objects.hash(first,second,acgu);
    }

    @Override
    public Collection<State> accessible(){
        Collection<State> result= new ArrayList<>();

        //C-move Conditions
        if ((moves.size()==1 && (moves.get(moves.size()-1)!='c')) // complement shouldn't be first move
            || ((moves.size()>=2) && (moves.get(moves.size()-1)!='c') && 
            ((moves.get(moves.size()-1)=='p') || (moves.get(moves.size()-2)=='p')))){
                List<Character> new_moves= new ArrayList<>(moves);
                new_moves.add('c');
                result.add(new VaccineState(complement(first),second,acgu,new_moves));
            }

        // P-move Conditions
        if (second.get(0)==0){
            List<Character> p_1st= new ArrayList<>(first);
            char element=p_1st.remove(p_1st.size()-1); //Afairesi teleutaiou stoixeiou apo ti proti lista
            List<Integer> p_2nd= new ArrayList<>();
            p_2nd.add(1); //Arithmos stoixeion sti deuteri lista
            p_2nd.add(dict.get(element)); //proto gramma os int
            p_2nd.add(dict.get(element)); //teleutaio gramma os int
            List<Boolean> p_acgu= new ArrayList<>(acgu);
            p_acgu.set(dict.get(element),true);
            List<Character> p_moves= new ArrayList<>(moves);
            p_moves.add('p');
            result.add(new VaccineState(p_1st,p_2nd,p_acgu,p_moves));
        }
        else if (dict.get(first.get(first.size()-1))==second.get(2) // letter is the same with last one
        || (first.get(first.size()-1)=='A' && acgu.get(0)==false) // letter has not been used
        || (first.get(first.size()-1)=='C' && acgu.get(1)==false) // ->>-
        || (first.get(first.size()-1)=='G' && acgu.get(2)==false)
        || (first.get(first.size()-1)=='U' && acgu.get(3)==false)){
            List<Character> p_1st= new ArrayList<>(first);
            char element=p_1st.remove(p_1st.size()-1); //Afairesi teleutaiou stoixeiou apo ti proti lista
            List<Integer> p_2nd= new ArrayList<>(second);
            int previous=p_2nd.get(0);
            p_2nd.set(0,previous+1);
            p_2nd.set(2,dict.get(element)); //Teleutaio gramma protis listas ginetai teleutaio deuteris
            List<Boolean> p_acgu= new ArrayList<>(acgu);
            p_acgu.set(dict.get(element),true);
            List<Character> p_moves= new ArrayList<>(moves);
            p_moves.add('p');
            result.add(new VaccineState(p_1st,p_2nd,p_acgu,p_moves));
        }   

        //R-move Conditions
        if (second.get(0)>=2 //second queue must have at least 2 elements 
            && (moves.get(moves.size()-1)!='r') && 
            ((moves.get(moves.size()-1)=='p' || (moves.get(moves.size()-2)=='p')))){
                
                List<Integer> r_2nd= new ArrayList<>(second);
                int last=r_2nd.get(2); //Timi teleutaiou stoixeiou
                int First=r_2nd.get(1); //Timi protou stoixeiou
                r_2nd.set(2,First); //Bazei to proto stoixeio teleutaio
                r_2nd.set(1,last); //Bazei to teleutaio stoixeio stin arxi
                List<Character> r_moves= new ArrayList<>(moves);
                r_moves.add('r');
                result.add(new VaccineState(first,r_2nd,acgu,r_moves));
            }
        return result;
    }
     
    public static Hashtable<Character,Integer> dict = new Hashtable<Character,Integer>() {{
        put('A',0);
        put('C',1);
        put('G',2);
        put('U',3);
    }};

    // Efarmozei ti praxi complement kai epistrefei ti nea lista
    public static List<Character> complement(List<Character> List1){
        List<Character> temp = new ArrayList<>(List1);
        for(int i=0; i<temp.size(); i++){
            if (temp.get(i)=='A') {
                temp.set(i,'U');
            }
            else if (temp.get(i)=='U') {
                temp.set(i,'A');
            }
            else if (temp.get(i)=='C') { 
                temp.set(i,'G');
            }
            else {
            temp.set(i,'C');
            }
        }
        return temp;
    }
}
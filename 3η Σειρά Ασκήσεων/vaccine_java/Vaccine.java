import java.io.*;
import java.util.*;

public class Vaccine {
    
    //new HashMap<Integer, Integer>() {{ put(1, 1); put(2, 2); }};

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

    public class state{
        List<Character> first;
        List<Integer> second;
        List<Boolean> acgu;
        List<Character> moves;

        //Constructor
        public state(List<Character> a,List<Integer> b,
                    List<Boolean> c, List<Character> d ){
                        first=a;
                        second=b;
                        acgu=c;
                        moves=d;
        }

        //prints moves in expected format
        public void print(){
            String string = moves.toString() 
            .substring(1, 3 * moves.size() - 1) 
            .replaceAll(", ", ""); 
            System.out.println(string);
        }

        //Euresi lisis
        public Boolean success(){
            return first.size()==0;
        }

        @Override
        public boolean equals(Object o) { 
            // If the object is compared with itself then return true   
            if (o == this) { 
                return true; 
            } 
            state c = (state) o; //typecast o to state gia na sygkrinoume ta data members
            return c.first==this.first && c.second==this.second && c.acgu==this.acgu;
        }
        @Override
        public int hashCode() {
        return Objects.hash(first,second,acgu);
    }
    
  }

    public static Vector<state> accessible(state self){
        state state_c,state_p,state_r;
        
        Vaccine temp= new Vaccine();

        //C-move Conditions
        if ((self.moves.size()==1 && (self.moves.get(self.moves.size()-1)!='c')) // complement shouldn't be first move
            || ((self.moves.size()>=2) && (self.moves.get(self.moves.size()-1)!='c') && 
              ((self.moves.get(self.moves.size()-1)=='p') || (self.moves.get(self.moves.size()-2)=='p')))){
                List<Character> new_moves= new ArrayList<>(self.moves);
                new_moves.add('c');
                state_c = temp.new state(complement(self.first),self.second,self.acgu,new_moves);
            }
        else { 
            state_c=null;
        }

        // P-move Conditions
        if (self.second.get(0)==0){
            List<Character> p_1st= new ArrayList<>(self.first);
            char element=p_1st.remove(p_1st.size()-1); //Afairesi teleutaiou stoixeiou apo ti proti lista
            List<Integer> p_2nd= new ArrayList<>();
            p_2nd.add(1); //Arithmos stoixeion sti deuteri lista
            p_2nd.add(dict.get(element)); //proto gramma os int
            p_2nd.add(dict.get(element)); //teleutaio gramma os int
            List<Boolean> p_acgu= new ArrayList<>(self.acgu);
            p_acgu.set(dict.get(element),true);
            List<Character> p_moves= new ArrayList<>(self.moves);
            p_moves.add('p');
            state_p= temp.new state(p_1st,p_2nd,p_acgu,p_moves);
        }
        else if (dict.get(self.first.get(self.first.size()-1))==self.second.get(2) // letter is the same with last one
        || (self.first.get(self.first.size()-1)=='A' && self.acgu.get(0)==false) // letter has not been used
        || (self.first.get(self.first.size()-1)=='C' && self.acgu.get(1)==false) // ->>-
        || (self.first.get(self.first.size()-1)=='G' && self.acgu.get(2)==false)
        || (self.first.get(self.first.size()-1)=='U' && self.acgu.get(3)==false)){
            List<Character> p_1st= new ArrayList<>(self.first);
            char element=p_1st.remove(p_1st.size()-1); //Afairesi teleutaiou stoixeiou apo ti proti lista
            List<Integer> p_2nd= new ArrayList<>(self.second);
            int previous=p_2nd.get(0);
            p_2nd.set(0,previous+1);
            p_2nd.set(2,dict.get(element)); //Teleutaio gramma protis listas ginetai teleutaio deuteris
            List<Boolean> p_acgu= new ArrayList<>(self.acgu);
            p_acgu.set(dict.get(element),true);
            List<Character> p_moves= new ArrayList<>(self.moves);
            p_moves.add('p');
            state_p= temp.new state(p_1st,p_2nd,p_acgu,p_moves);
        }   
        else{
            state_p=null;
        }
        
        //R-move Conditions
        if (self.second.get(0)>=2 //second queue must have at least 2 elements 
            && (self.moves.get(self.moves.size()-1)!='r') && 
            ((self.moves.get(self.moves.size()-1)=='p' || (self.moves.get(self.moves.size()-2)=='p')))){
                
                List<Integer> r_2nd= new ArrayList<>(self.second);
                int last=r_2nd.get(2); //Timi teleutaiou stoixeiou
                int first=r_2nd.get(1); //Timi protou stoixeiou
                r_2nd.set(2,first); //Bazei to proto stoixeio teleutaio
                r_2nd.set(1,last); //Bazei to teleutaio stoixeio stin arxi
                List<Character> r_moves= new ArrayList<>(self.moves);
                r_moves.add('r');
                state_r=temp.new state(self.first,r_2nd,self.acgu,r_moves);
            }
            else{
                state_r=null;
            }

            //Return result tasks
            Vector<state> result=new Vector<state>();
            if (state_c!=null) 
                {result.add(state_c);}
            if(state_p!=null)
                {result.add(state_p);}
            if(state_r!=null)
                {result.add(state_r);}
            return result;
        
    }

    public static void solve(state self){
        Queue<state> Q = new ArrayDeque<state>(); //orismos tis listas Q
        Q.add(self);
        Set<state> seen = new HashSet<state>();
        seen.add(self);
        while(Q.size()!=0){ //Mexri na adeiasei i Q
            state s=Q.remove();
            Vector<state> possible=accessible(s);
            for(int i=0; i<possible.size(); i++){ //Gia oles tis pithanes epomenes katastaseis
                state curr=possible.get(i);
                if(curr.success()){ //An einai katastasi epityxias
                    //seen.add(curr); 
                    curr.print();
                    return;
                }
                
                //if  curr not in seen
                    if (seen.contains(curr)==false) {
                        Q.add(curr);
                        seen.add(curr);
                }
                
            }
        }
    }

    public static void solver(List<Character> first){
        List<Integer> second=new ArrayList<>();
        second.add(0);
        List<Boolean> acgu=new ArrayList<>();
        acgu.add(false);
        acgu.add(false);
        acgu.add(false);
        acgu.add(false);
        List<Character> moves=new ArrayList<>();
        Vaccine temp2=new Vaccine();
        state temp= temp2.new state(first,second,acgu,moves);
        solve(temp);
    }

    public static void main(String[] args) throws Exception
        {
            if(args.length == 0){
                System.out.println("Invalid usage: Please give an input file.");
                return;
            }
            Scanner s= new Scanner(new File(args[0]));
            
            int vaccines=s.nextInt();
            Vaccine mainObj = new Vaccine();
            s.nextLine();
            //Diabazei to arxeio kai ftiaxnei tin antstoixi 1h char list
            for (int i=1; i<=vaccines; i++) { 
                String line = s.nextLine();
                List<Character> first = new ArrayList<>();
                
                // For each character in the String 
                // add it to the List 
                for (char ch : line.toCharArray()) { 
                    first.add(ch); 
                }
                //System.out.println(first);
                //System.out.println(complement(first));
                //Klisi sinartisis solve
                
                mainObj.solver(first);
            }
        }
        //public void run(List<Character> firs,
        //List<Character> second,
        //List<Boolean> acgu,
        //List<Character> moves){
        //    state temp= new state(first,second,acgu,moves);
        //    solve(temp);
        //}
       
    
}
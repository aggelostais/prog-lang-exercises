import java.io.*;
import java.util.*;

public class Vaccine {
    

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
        List<Character> second;
        List<Boolean> acgu;
        List<Character> moves;

        //Constructor
        public state(List<Character> a,List<Character> b,
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
            return c.first==this.first && c.second==this.second;
        }
    }

    public Vector<state> accessible(state self){
        state state_c,state_p,state_r;
        Hashtable<Character,Integer> dict = new Hashtable<Character,Integer>();
        dict.put('A',0);
        dict.put('C',1);
        dict.put('G',2);
        dict.put('U',3);

        //C-move Conditions
        if ((self.moves.size()==1 && (self.moves.get(self.moves.size()-1)!='c')) // complement shouldn't be first move
            || ((self.moves.size()>=2) && (self.moves.get(self.moves.size()-1)!='c') && 
              ((self.moves.get(self.moves.size()-1)=='p') || (self.moves.get(self.moves.size()-2)=='p')))){
                List<Character> new_moves= new ArrayList<>(self.moves);
                new_moves.add('c');
                state_c = new state(complement(self.first),self.second,self.acgu,new_moves);
            }
        else { 
            state_c=null;
        }

        // P-move Conditions
        if (self.second.size()==0 // In the begining we can do p move
        || (self.first.get(self.first.size()-1)==self.second.get(self.second.size()-1)) // letter is the same with last one
        || (self.first.get(self.first.size()-1)=='A' && self.acgu.get(0)==false) // letter has not been used
        || (self.first.get(self.first.size()-1)=='C' && self.acgu.get(1)==false) // ->>-
        || (self.first.get(self.first.size()-1)=='G' && self.acgu.get(2)==false)
        || (self.first.get(self.first.size()-1)=='U' && self.acgu.get(3)==false)){
            List<Character> p_1st= new ArrayList<>(self.first);
            char element=p_1st.remove(p_1st.size()-1); //Afairesi teleutaiou stoixeiou apo ti proti lista
            List<Character> p_2nd= new ArrayList<>(self.second);
            p_2nd.add(element); //Prosthiki sti deuteri lista
            List<Boolean> p_acgu= new ArrayList<>(self.acgu);
            p_acgu.set(dict.get(element),true);
            List<Character> p_moves= new ArrayList<>(self.moves);
            p_moves.add('p');
            state_p= new state(p_1st,p_2nd,p_acgu,p_moves);
        }   
        else{
            state_p=null;
        }
        
        //R-move Conditions
        if (self.second.size()>=2 //second queue must have at least 2 elements 
            && (self.moves.get(self.moves.size()-1)!='r') && 
            ((self.moves.get(self.moves.size()-1)=='p' || (self.moves.get(self.moves.size()-2)=='p')))){
                List<Character> r_2nd= new ArrayList<>(self.second);
                char last=r_2nd.remove(r_2nd.size()-1); //Afairei to teleutaio stoixeio
                char first=r_2nd.remove(0); //Afairei to proto stoixeio
                r_2nd.add(first); //Bazei to proto stoixeio sto telos
                r_2nd.add(0,last); //Bazei to teleutaio stoixeio stin arxi
                List<Character> r_moves= new ArrayList<>(self.moves);
                r_moves.add('r');
                state_r=new state(self.first,r_2nd,self.acgu,r_moves);
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

    public void solve(state self){
        Queue<state> Q = new LinkedList<state>(); //orismos tis listas Q
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

    public void solver(List<Character> first){
        List<Character> second=new ArrayList<>();
        List<Boolean> acgu=new ArrayList<>();
        acgu.add(false);
        acgu.add(false);
        acgu.add(false);
        acgu.add(false);
        List<Character> moves=new ArrayList<>();
        state temp= new state(first,second,acgu,moves);
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
                
                Vaccine mainObj = new Vaccine();
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
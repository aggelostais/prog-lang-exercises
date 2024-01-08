import java.io.*;
import java.util.*;

public class Vaccine {
    
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
                Solver solver=new BFSolver();
                List<Integer> second=new ArrayList<>();
                second.add(0);
                List<Boolean> acgu=new ArrayList<>();
                acgu.add(false);acgu.add(false);acgu.add(false);acgu.add(false);
                List<Character> moves=new ArrayList<>();
                State initial= new VaccineState(first,second,acgu,moves);    
                System.out.println(first);
                printSolution(solver.solve(initial));
            }
        }
         //prints moves in expected format
         public static void printSolution(State s){
            String string = s.Moves().toString() 
            .substring(1, 3 * s.Moves().size() - 1) 
            .replaceAll(", ", ""); 
            System.out.println(string);
        }
       
}
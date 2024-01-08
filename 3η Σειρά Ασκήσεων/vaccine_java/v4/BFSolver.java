import java.io.*;
import java.util.*;

public class BFSolver implements Solver {
  @Override
  public State solve (State self) {
        Queue<State> Q = new ArrayDeque<>(); //orismos tis listas Q
        Q.add(self);
        Set<State> seen = new HashSet<>();
        seen.add(self);
        while(Q.size()!=0){ //Mexri na adeiasei i Q
            State s=Q.remove();
            Collection<State> possible=self.accessible();
            for(State curr: possible){ //Gia oles tis pithanes epomenes katastaseis
                if(curr.success()){ //An einai katastasi epityxias
                    //seen.add(curr); 
                    return curr;
                }
                //if  curr not in seen
                    if (seen.contains(curr)==false) {
                        Q.add(curr);
                        seen.add(curr);
                }
            }
        }
        return null;
    }
}
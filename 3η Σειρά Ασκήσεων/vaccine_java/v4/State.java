import java.io.*;
import java.util.*;

public interface State {
  // Returns whether the state is final, i.e. the goal of the search.
  public boolean success();
  
  public List<Character> Moves();
  // Returns a collection of the states that can be reached by making
  // all possible moves.  Some of these states may be bad.
  public Collection<State> accessible();
}
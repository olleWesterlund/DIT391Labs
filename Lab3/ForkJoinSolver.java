package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.LinkedList;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.atomic.AtomicBoolean;

/**
* <code>ForkJoinSolver</code> implements a solver for
* <code>Maze</code> objects using a fork/join multi-thread
* depth-first search.
* <p>
* Instances of <code>ForkJoinSolver</code> should be run by a
* <code>ForkJoinPool</code> object.
*/


public class ForkJoinSolver extends SequentialSolver {

    // A boolean shared between threads for checking when the goal is reached.
    private static AtomicBoolean isFinished = new AtomicBoolean();
    // A skipListSet shared between threads to keep track on cells that the diffrent threads have already visited.
    private final static ConcurrentSkipListSet<Integer> visitedCells = new ConcurrentSkipListSet<>();
    // A map of the differnet threads running ForkJoinSolver
    private Map<Integer, ForkJoinSolver> players;

    /**
    * Creates a solver that searches in <code>maze</code> from the
    * start node to a goal.
    *
    * @param maze   the maze to be searched
    */
    public ForkJoinSolver(Maze maze) {
        super(maze);
    }
    
    /**
    * Creates a solver that searches in <code>maze</code> from the
    * start node to a goal, forking after a given number of visited
    * nodes.
    *
    * @param maze        the maze to be searched
    * @param forkAfter   the number of steps (visited nodes) after
    *                    which a parallel task is forked; if
    *                    <code>forkAfter &lt;= 0</code> the solver never
    *                    forks new tasks
    */
    public ForkJoinSolver(Maze maze, int forkAfter) {
        this(maze);
        this.forkAfter = forkAfter;
        this.players = new HashMap<>();
    }

    /**
     * @param maze      the maze to be searched
     * @param forkAfter the number of steps (visited nodes) after
     *                  which a parallel task is forked; if
     *                  <code>forkAfter &lt;= 0</code> the solver never
     *                  forks new tasks
     * @param start     The cell to start from
     */
    private ForkJoinSolver(Maze maze, int forkAfter, int start) {
        this(maze, forkAfter);
        this.start = start;
    }
    
    /**
    * Searches for and returns the path, as a list of node
    * identifiers, that goes from the start node to a goal node in
    * the maze. If such a path cannot be found (because there are no
    * goals, or all goals are unreacheable), the method returns
    * <code>null</code>.
    *
    * @return   the list of node identifiers from the start node to a
    *           goal node in the maze; <code>null</code> if such a path cannot
    *           be found.
    */
    @Override
    public List<Integer> compute() {
        return parallelSearch();
    }
    
    private List<Integer> parallelSearch() {
        // Creating a new player on the cell with id start.
        int player = maze.newPlayer(start);
        // A counter to keep track on when to fork 
        int forkCount = 0;

        // Pushing the starting id on the frontier stack.
        frontier.push(start);

        // While the stack is not empty and while a thread has not found the goal,
        // we continue to search
        while (!frontier.empty() && !isFinished.get() ) {
            // Checking the top cell on the stack
            int currentNode = frontier.pop();
            // If the currentNode is the goal we move to that position, sets the boolean to
            // true and returns the path from start to the currentNode.
            if (maze.hasGoal(currentNode)) {
                maze.move(player, currentNode);
                isFinished.set(true);
                return pathFromTo(start, currentNode);
            }

            // If the currentNode is not in the shared list of visited cells, we add it to the list, 
            // and if the goal is not yet found we move the player to currentNode.
            if (visitedCells.add(currentNode) && !isFinished.get()) {
                maze.move(player, currentNode);

                for (int neighbor : maze.neighbors(currentNode)) {
                    if (forkAfter != 0 && (forkCount > forkAfter) && maze.neighbors(currentNode).size() > 2) {
                        forkCount = 0;
                        if (!visitedCells.contains(neighbor)) {
                            // Creating a new solver and starting a new thread on the same maze, with same forkAfter value
                            // and with starting position of neighbors id
                            ForkJoinSolver newSolver = new ForkJoinSolver(maze, forkAfter, neighbor);
                            players.put(currentNode, (ForkJoinSolver) newSolver.fork());
                        } 
                    } else {
                        frontier.push(neighbor);
                    }
                    if (!visitedCells.contains(neighbor)) {
                        predecessor.put(neighbor, currentNode);
                    }
                }
            }
            forkCount++;
        }

        List<Integer> path = new LinkedList<>();
        for (Map.Entry<Integer, ForkJoinSolver> currentSolver : players.entrySet()) {
            // For every ForkJoinSolver in the Map we collect the results and combines them
            List<Integer> firstList = currentSolver.getValue().join();
            if (firstList != null && path.isEmpty()) {
                List<Integer> secondList = pathFromTo(start, currentSolver.getKey());
                if (secondList != null) {
                    path.addAll(secondList);
                    path.addAll(firstList);
                }
            }
        }

        // If the list is not empty, we have a path from the start to the goal.
        // If not we return null.
        if (!path.isEmpty()) {
            return path;
        } else {
            return null;
        }
    }
}

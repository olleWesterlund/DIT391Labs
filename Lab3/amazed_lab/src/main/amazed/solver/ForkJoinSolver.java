package amazed.solver;

import amazed.maze.Maze;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
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
    private static AtomicBoolean isFinished = new AtomicBoolean();
    private final static ConcurrentSkipListSet<Integer> visitedCells = new ConcurrentSkipListSet<>();
    private Map<Integer, ForkJoinSolver> players;



    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze the maze to be searched
     */
    public ForkJoinSolver(Maze maze) {
        super(maze);
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze      the maze to be searched
     * @param forkAfter the number of steps (visited nodes) after
     *                  which a parallel task is forked; if
     *                  <code>forkAfter &lt;= 0</code> the solver never
     *                  forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter) {
        this(maze);
        this.forkAfter = forkAfter;
        this.players = new HashMap<>();
    }

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
     * @return the list of node identifiers from the start node to a
     * goal node in the maze; <code>null</code> if such a path cannot
     * be found.
     */
    @Override
    public List<Integer> compute() {
        return parallelSearch();
    }

    private List<Integer> parallelSearch() {
        int player = maze.newPlayer(start);
        int forkCount = 0;

        frontier.push(start);

        while (!frontier.empty() && !isFinished.get() ) {
            int currentNode = frontier.pop();
            if (maze.hasGoal(currentNode)) {
                maze.move(player, currentNode);
                isFinished.set(true);
                return pathFromTo(start, currentNode);
            }
            if (visitedCells.add(currentNode) && !isFinished.get()) {
                maze.move(player, currentNode);

                for (int neighbor : maze.neighbors(currentNode)) {
                    if (forkCount > forkAfter && maze.neighbors(currentNode).size() > 2) {
                        forkCount = 0;
                        if (!visitedCells.contains(neighbor)) {
                            players.put(currentNode, (ForkJoinSolver) new ForkJoinSolver(maze, forkAfter, neighbor).fork());
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
        for (Map.Entry<Integer, ForkJoinSolver> p : players.entrySet()) {
            List<Integer> firstList = p.getValue().join();
            if (firstList != null && path.isEmpty()) {
                List<Integer> secondList = pathFromTo(start, p.getKey());
                if (secondList != null) {
                    path.addAll(secondList);
                    path.addAll(firstList);
                }
            }
        }

        if (!path.isEmpty()) {
            return path;
        } else {
            return null;
        }
    }

}

# Principles-of-Programming-Languages-Homework-3
A Haskell program that creates random "perfect" 2d mazes using a modified version of Kruskal's algorithm.

Developed by:
Lefteris Karampas: mr.lef_21113@hotmail.com
Thanasis Filippidis: thanasisflpd@gmail.com

We have represented the maze as a graph where each node is a cell and and each vertice is a cell wall that has been removed.

Every cell with coordinates (x,y) has a unique id that can be calculated by the function id = y * width + x where width is the width of the
maze.

During the development of the main program we have used DFS without keeping the visited set and using flags to keep the states of the
search. For the visualization of the created maze we are using a sorted list of the id's while in the same time we translate each of
them back in coordinates.

For the bonus question we have used BFS with some other helping structures as an explored set and a frontier queue.

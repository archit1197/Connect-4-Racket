# Intelligent Agent for Connect 4 #

## Repository for CS152 Course Project ##

### THE PROBLEM STATEMENT ###

Our project is a scheme recreation of the famous two player strategy board game Connect Four. In the
two-player version of the game each player first chooses a color and then they take turns dropping
colored discs from the top into a seven-column, six-row vertically suspended grid. The pieces fall straight
down, occupying the next available space within the column. The challenge is to design a user v/s
computer AI version of the game, using heuristics to ensure efficient moves of the AI. Also designing a UI
for the game.

### AI Algorithm ###
1. Minmax
There are essentially two players playing the game - The computer is the
maximizing player and the user a minimizing player. Given a state of the board
there are multiple possibilities to play the next move. The objective is to choose the
best move that the maximizing player should play. For this all possible next states
are considered and that move is selected which gives the maximum score for the
maximizing player assuming the worst case where the opposite player also plays
his best game.The minimizing player picks up that next move which results in a
minimum score for the maximizing player .The game alternates between the two
players and evaluates until a specified depth.

2. Alpha-beta Pruning
A naive recursion will blow up the search space in a few depths itself so some
states are needed to be pruned or eliminated. The algorithm maintains two
values, alpha and beta, which represent the maximum score that the maximizing
player is assured of and the minimum score that the minimizing player is assured
of respectively. Initially alpha is a large negative value and beta is a large positive
value player. It can happen that when choosing a certain branch of a certain node
the minimum score that the minimizing player is assured of becomes less than
the maximum score that the maximizing player is assured of (beta <= alpha). If this is the case, the parent node should not choose this node, because it will make the score for the
parent node worse. Therefore, the other branches of the node do not have to be explored.


### LIMITATIONS ###
***Speed vs AI accuracy tradeoff*** - Designing heuristics for the game involves making a tradeoff
between speed of the game and the accuracy of the AI algorithm. We tried out several different
heuristics for the game, and discovered that an algorithm which gives more accurate answers
usually tends to take longer time. We have attached with the project submission a file showing
the incremental heuristics we tried out.

### INTERESTING FEATURES ###
1. ***Debounced Mouse Click*** -  Whenever a user unintentionally double-clicks the mouse or the laptop
mouse click is really sensitive, the player move was duplicated, not desired right? So to take care
of that we used the concept of states taught in class. We maintain a state variable ‘Debouncer’,
which becomes 0 on a mouse click , but is restored only after the a few iterations of the main
loop. This is enough to ensure a single click is registered.
2. ***How to Clone objects?*** -  In racket if we try copying a class instance to another directly using
define statement, it associates the same pointer with the new copy that is created.As a result if
we modify the new class instance the same changes get reflected in the original class instance.
To avoid this we created a new member function called clone that copies the original class fields
into the new class instance and returns the new object instance with the same fields as the
original class.

### INSTRUCTIONS ###

Racket is a general-purpose programming language as well as the world’s first ecosystem for language-oriented programming.
Install DrRacket in your preferred operating system from [here](https://download.racket-lang.org/)
Run the project by opening main.rkt in a racket compiler and running it. A dialog box having a 6X7 grid should open up.


Refer to the pdf document **CS152ProjectReport.pdf** for a detailed report of the project.

Hope you enjoy!


TEAM MEMBERS:
* Devansh Shah 150070004
* Archit Gupta 150070001
* Madhav Goel 150110017

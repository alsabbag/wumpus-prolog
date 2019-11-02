
# SCS 3547 Assignment 2
=======================

## Overview
A detailed analysis of a prolog-based implementation of Wumpus World.

## Team Members:
* Mark Hubbard
* Konrad Korzeniewski
* Mohammed Radha

## Objective

* Get a copy of the Wumpus World solution [here](https://github.com/rlegendi/wumpus-prolog/blob/master/wumpus.pl).  Run it [here](https://swish.swi-prolog.org/).
* Working in teams of (up to) 3, read and determine how all the code works.
* You can find online PDFs of Prolog tutorials and books to assist with the syntax and/or ask using Quercus.
* Write and submit an explanation of how each rule in the program works.
* If several rules are very similar you can simply describe one and how the others differ.

## Approach:

* Created a markdown document with embeded code snippets.
* Organized the documentation into major "sections" and "blocks" of rules based on the original code structure.
* Analyzed and documented the overall structure and flow of the program.
* Analyzed and documented sections, blocks, rules and facts.

## References
* Artificial Intelligence : A Modern Approach (Russel - Norvig).
* CSC384 Prolog Tutorial (Hojjat Ghaderi and Fahiem Bacchus, University of Toronto).
* [Wikipedia Prolog Entry](https://en.wikipedia.org/wiki/Prolog_syntax_and_semantics)
* [Prolog Guide](https://www.cis.upenn.edu/~matuszek/Concise%20Guides/Concise%20Prolog.html)
* Code by:
	* Author: Richard O. Legendi (with permission from above authors).
	* Version: v1.0, Jan. 31, 2007.

# Program Analysis


## Structural Analysis

The code can be organized into three main sections:

1. **Program Control** - managing the overall flow (starting at the main entry point) from initialization to termination.
2. **Agent/Environment Management** - the rules and facts related to agent movement and environment state.
3. **Knowledge Base** - the set of initial and dynamically determined facts.

In the original code, each section is demarcated into blocks of related rules using a comment block.

The following table shows how the rules are divided up into their respective sections and blocks.

| Program Control      		| Agent/Environment Management | Knowledge Management  
|:------------------------	|:--------------------------|:---------------------
| _Mainline:_			   		| _Perceptors:_             | _Knowledge Base (KB):_
| * start						| * adj                     | * ask_KB
|								| * adjacent                | * add\_gold_KB
| _Display standings:_		| * isBleezy                | * add\_pit_KB
| * standing					| * isGlittering            | * add\_wumpus_KB
| * stnd						| * isSmelly                | * assume_gold
|								| * bleezy                  | * assume_pit
| _Initializing:_			| * glittering              | * assume_wumpus
| * init						| * smelly                  | * permitted
| * init_game					|                           | * update_KB
| * init\_land_fig72		| _Perceptotion:_            | 
| * init_agent				| * make\_percept_sentence  | _Utils:_
| * init_wumpus				| * make_perception         | * not_member
| * visit						| * test_perception         |
|                          |                           | * 
| _Scheduling simulation:_	| _Updating states:_
| * step_pre					| * is_pit
| * take_steps				| * update\_agent_location
|								| * update_score
|								| * update_time

_Note: In the detailed analysis later in this document, a couple of blocks were relocated (as compared to the original program) to allow for a better flow (and to enable the division of labour) for the documentation.  Specifically, the "Initializaing" and "Display Standings" blocks were moved up into the Program Control section._

## Flow Analysis

### Core Flow

The main flow of the program is:

* start ->
  * init ->
     * init_game
     * init\_land_fig72
     * init_agent
     * init wumpus
  * take_steps ->
     * make\_percept_sentence - _detect surroundings_ ->
         * smelly - _is it smelly?_
         * bleezy - _is it bleezy?_
         * glittering - _is it glittering?_
     * agent_location - _resolve the agent location (to print)_
     * update_KB - _update what we know in the KB (based on percepts from agent location)_ ->
         * add\_wumpus_KB - _set Wumpus location facts_
         * add\_pit_KB -  _set pit location facts_
         * add\_gold_KB - _set gold location facts_
     * ask_KB - _query the KB to help decide on a next move_
     * update_time - _update the time elapsed so far_
     * update_score - _update the score so far_
     * step_pre - _check if the game has ended (i.e. get rich or die trying)_
         * take_steps - _recurse making moves until the game ends_

_Note: The above flow hierarchy doesn't exhaustively include all child rules.  It only goes as deep as necessarily to highlight the main control flow._


This version of Wumpus is somewhat simplifed from the class version:

1. The agent has no bow and arrow (and hence, no need to rotate in place to aim).
2. The agent can go directly to any valid grid square (but cannot move to a grid square it has already visited).

### Initial Board Configuration

```
% agent location fact
assert( agent_location([1,1]) )

% pit location facts
assert( pit_location([4,4]) )
assert( pit_location([3,3]) )
assert( pit_location([1,3]) )

% gold location fact  
assert( gold_location([3,2]) )

% Wumpus location fact
assert( wumpus_location([4,1]) )
```


|     |     |     |  P  |
|:---:|:---:|:---:|:---:|
|  P  |     |  P  |     |
|     |     |  G  |     |
|  A  |     |     |  W  |

A = Agent,
G = Gold,
P = Pit,
W = Wumpus

### Bugs?

There appears to be a few bugs in the logic.  For example (based on a sample run - see Appendix):

* The gold detection doesn't appear correct based on the following console output (with commentary added):

    > KB learn [3,2] - there's no gold here! **% Yes, there is!**.  
    > I'm going to: [3,2]  
    > There's still something to do...  
    > WON!  % See, I told you there was gold here!    

* The pit detection (and Wumpus detection) logic doesn't appear to be trustworthy. For example:
    > I'm in [1,2], seeing: [no,yes,no].  
    > KB learn [1,3] - its a Pit!  
    > KB learn [1,1] - its a Pit!  **% incorrect!**  
    > KB learn [2,2] - its a Pit!  **% incorrect!**  
    > KB learn [0,2] - its a Pit!  **% X=0 is an invalid co-ordinate value!**   

    The logic for declaring a pit based on a percept of Bleeze, blindly asserts that all adjacent squares around the agent location have a pit, replacing previously true facts with fake facts (e.g. in the above case, we already knew that the starting point [1,1] was not a pit, however, the Bleeze detected when the agent visited grid square [1,2] overrules that fact).  The logic for asserting Wumpus location facts appears to have the same issue.

* The flow seems to hit a glitch at location [3,1] near the Wumpus:

```
I'm in [1,1], seeing: [no,no,no]
I'm going to: [1,2]
I'm in [1,2], seeing: [no,yes,no]
I'm going to: [2,1]
I'm in [2,1], seeing: [no,no,no]
I'm going to: [2,2]
I'm in [2,2], seeing: [no,no,no]
I'm going to: [3,1]
I'm in [3,1], seeing: [yes,no,no]
I'm in [3,1], seeing: [no,no,no]  **% Where did the stench go?**
I'm going to: [2,3]
I'm in [2,3], seeing: [no,yes,no]
I'm going to: [3,2]
WON!
```


## Variables:


* Entity Locations: 
    * AL - agent location using grid square co-ordinates (i.e. x, y)
    * L - the "next" location for an agent using grid square co-ordinates.
    * GL - gold location using grid square co-ordinates.
    * WL - wumpus location using grid square co-ordinates.
    * VL = the accrued list of grid squares (as co-ordinates) already visited.
 * Grid Square Co-ordinates:
    * Z1, Z2, Z3, Z4 - the immediately adjacent grid squares of the agent (North, South, East, West).
* Game Status:
    * T - current elaped time
    * S - current score
* Perceptions:
    * Bleeze - a boolean value to indicate if the agent can perceive "bleeze".
    * Glitter - a boolean value to indicate if the agent can perceive glitter.
    * Stench - a boolean value to indicate if the agent can perceive a stench.     
* Agent Movement:
	 * Action - the next location for the agent as resolved by the knowledge base.
    * VisitedList - a list of co-ordinates for grid squares that the agent has visited.

_Note: There are a number of "single use" variables (i.e. used within a single rule):_

* Aloc - the agent location
* NewAL - the "next" location for an agent using grid square co-ordinates.
* NewScore - latest score following the latest move.
* NewTime - elapsed time following the latest move.
* Percept - unifies to an individual perception (Bleeze, Glitter, Stench)
* Perception - a triplet of Stench, Bleeze and Glitter (e.g. [yes, no, no]).
* X - a generic location variable (I guess because X marks the spot).
* [X1, Y1] and [X2, Y2] - instances of grid square locations used during location comparisons
* [X, y] and [U, V] - location variables used during list membership checks
* Xs and Ys - two list members (head and tail) used when building the visited_list
* Ls1 and Ls2 - locations being compared for adjacency while processing percepts

## Dynamics

The following predicates are declared to be dynamic so that clauses can be dynamically added (via assert) and removed (via retract):

* agent_location/1 - stores the current agent location as a grid square co-ordinate (x,y co-ordinates)
* gold_location/1,   % stores the gold location as a grid square (x,y co-ordinates)  
* pit_location/1,    % stores the pit location as a grid square (x,y co-ordinates)  
* time_taken/1,      % records the duration of the game (number)
* score/1,           % records the current score (number)
* visited/1,         % records the number of grid squares visited
* visited_cells/1,   % tracks the grid square co-ordinates (list of x,y co-ordinates)
* world_size/1,      % stores the size of the grid (n x n squares)
* wumpus_location/1, % stores the wumpus location (x,y co-ordinates)
* isPit/2,           % indicates whether the grid square is a pit (x,y co-ordinates and a boolean)
* isWumpus/2,        % is the grid square occupied by the Wumpus? ((x,y co-ordinates and a boolean)
* isGold/2           % is the grid square filled with the gold? ((x,y co-ordinates and a boolean)

_Note: the /n designates the number of arguments for the predicate._

```
:- dynamic ([
	     agent_location/1,
	     gold_location/1,
	     pit_location/1,
	     time_taken/1,
	     score/1,
	     visited/1,
	     visited_cells/1,
	     world_size/1,
	     wumpus_location/1,
        isPit/2,
        isWumpus/2,
        isGold/2
	    ]).
```

# Program Flow

## Mainline

### start

This rule is the entry point of the program (invoked using ?-start).  The query *start* (invoked from the command line) discovers and invokes the *start* rule to prove the query.  This rule executes both the init and take\_steps clauses to respond to the query.  The rule is proven if the init and take\_steps clauses succeed (as well as the format statements that print to the console). The predicate take\_steps either succeeds or fails depending on the fate of the agent in the game.

```
start :-
    format('Initializing started...~n', []),
    init,
    format('Let the game begin!~n', []),
    take_steps([[1,1]]).
```

## Initializing

### init

We can show that the init rule is successful if each of the specific init clauses are successful.  Overall, the facts around the game environment, its initial configuration of pits and gold and both the agent and wumpus are set.

```
init :-
    init_game,
    init_land_fig72,
    init_agent,
    init_wumpus.
```

### init_game

The initial game state retracts any pre-existing facts about the game and its environment, including the elapsed game time, the score, the number of squares the agent has visited, the location of the gold and the wumpus, and the list of visited squares.

It then asserts the following facts:

* the *time_taken* starts at 0
* the *score* starts at 0
* the number of *visited* grid squares is 1
* the list of *visited_cells* is empty ([]).

_Note: At this point, the location of the pits, gold, agent or Wumpus have not been set._

```
init_game :-
    retractall( time_taken(_) ),
    assert( time_taken(0) ),

    retractall( score(_) ),
    assert( score(0) ),

    retractall( visited(_) ),
    assert( visited(1) ),

    retractall( isWumpus(_,_) ),
    retractall( isGold(_,_) ),

    retractall( visited_cells(_) ),
    assert( visited_cells([]) ).
```

### init\_land_fig72

The followiing rule retracts and then asserts all the facts for the size of the board (*world_size*) and the location of the gold (*gold_location*) and all pits (*pit_location*) according to Figure 7.2 in Russel-Norvig's book (2nd Edition).

Consequently, the following facts are:

* The grid world contains 4x4 squares (*world_size(4)*).
* The gold is located at square X=3, Y=2 (*gold_location([3,2])*).
* There are three pits:
    1. at grid square [4,4] (*pit_location([4,4]*)
    2. at grid square [3,3] (*pit_location([3,3]*)
    3. at grid square [1,3] (*pit_location([1,3]*)

```
init_land_fig72 :-
    retractall( world_size(_) ),
    assert( world_size(4) ),

    retractall( gold_location(_) ),
    assert( gold_location([3,2]) ),

    retractall( pit_location(_) ),
    assert( pit_location([4,4]) ),
    assert( pit_location([3,3]) ),
    assert( pit_location([1,3]) ).
```

### init_agent

The followiing rule retracts all facts regarding the agent and then places it on grid square [1,1] (*agent_location([1,1]*). Additionaly, that square is added to the list of visited squares (via *visit([1,1]*).

```
init_agent :-
    retractall( agent_location(_) ),
    assert( agent_location([1,1]) ),
    visit([1,1]).
```
### init_wumpus

The followiing rule retracts all facts regarding the Wumpus location and then places it on grid square [4,1](*wumpus_location([4,1]*).

```
init_wumpus :-
    retractall( wumpus_location(_) ),
    assert( wumpus_location([4,1]) ).
```
### visit

The following rule sets the facts arouond the list of visited cells.  The specified grid square co-ordinates (*Xs*) are added to the end of the existinig list (*Ys*). To add to the end of the list, the list is first emptied (using retractall) and then reconstructed with the new grid square at the end of the list (*visited_cells[Ys|Xs]*).

```
visit(Xs) :-
    visited_cells(Ys),
    retractall( visited_cells(_) ),
    assert( visited_cells([Ys|Xs]) ).
```

## Scheduling Simulation:

These rules plot out the agents path through the world's grid squares.

### step_pre
Before a step is taken (not including the first step), the current state of the game is evaluated.

The initial clauses in the following rule gather the facts around the current location of the agent (*agent_location(AL)*), the location of the gold (*gold_location(GL)*) and the location of the Wumpus (*wumpus_location(GL)*).  The current score (*score(S)*) and game time (*time_taken(T)*) facts are also gathered.

Once the facts are all gathered into variables, a disjunctive set of clauses is invoked to determine whether the game is over or it should contine.  The three clauses are:

1. The agent location (*AL*) and gold location (*GL*) variables can be matched/unified (*AL=GL*), which consequently declares a win and prints the score and time taken.
2. The agent location (*GL*) and Wumpus location (*WL*) variables can be matched/unified (*AL=WL*), which consequently declares a loss and prints the score (*S*) and time taken(*T*).
3. If the game is not over, the next steps are taken based on the steps already taken (*take_steps(Visited_List)*).

```
step_pre(VisitedList) :-
    agent_location(AL),
    gold_location(GL),
    wumpus_location(WL),
    score(S),
    time_taken(T),

    ( AL=GL -> writeln('WON!'), format('Score: ~p,~n Time: ~p', [S,T])
    ; AL=WL -> format('Lost: Wumpus eats you!~n', []),
               format('Score: ~p,~n Time: ~p', [S,T])
    ; take_steps(VisitedList)
    ).
```

### take_steps

Taking a step involves taking a look before the leap, and several facts are asserted after making a move. 

First, this rule collects an overall set of facts into a variable (*make_percept_sentence(Perception)*).  These facts (or "sentence") are a triplet of stench, bleeze and glitter based on the current agent location (*AL*).  

The agent location is explicitly resolved via the *update_location(AL)* clause in this rule simply to facilitate the print statement.  Likewise, it is resolved later in the rule via the *update_location(Aloc)* to add it to the visited list (*VL*).

The knowledge base is updated with the overall perception facts from this agent location (*update_KB(Perception)*).

Both the time (*update_time*) and score (*update_score*) facts are updated.

The visited list (*VL*) is updated by adding the agent location (*AL*) to the start of the list.  This rule also checks the facts as to whether the agent is still "standing" (*standing*).  The ongoing scheduling of moves through rule recursion continues by this rule resolving the *step_pre(VL)* rule (which called this rule) passing it the updated visited list.

```
take_steps(VisitedList) :-
    make_percept_sentence(Perception),
    agent_location(AL),
    format('I\'m in ~p, seeing: ~p~n', [AL,Perception]),

    update_KB(Perception),
    ask_KB(VisitedList, Action),
    format('I\'m going to: ~p~n', [Action]),

    update_time,
    update_score,

    agent_location(Aloc),
    VL = [Aloc|VisitedList],
    standing,
    step_pre(VL).
```

## Display Standings

These methods print out the current state of the game after each agent move.

### standing

The following rule detects whether the agent is still standing.  The initial clauses collect the facts around wumpus location, gold location and agent location and unifies them into their respective global variables (*WL, GL and AL*).

After resolving the locations, it disjunctively:

1. Checks for a pit (and if true, it then prints out
Checks if the agent location matches with a pit location (*is_pit(yes, AL)*), and if so, prints out the agent's fate and resolves as a "fail" (which breaks out of the recursion in *take_steps*), or, 
2. Displays a summary of the overall current state of the game (*stnd(AL,GL,WL)*).

```
standing :-
    wumpus_location(WL),
    gold_location(GL),
    agent_location(AL),

    ( is_pit(yes, AL) -> format('Agent was fallen into a pit!~n', []),
      fail
    ; stnd(AL, GL, WL)
    ).
```

### stnd

The followiing rule resolves by printing out a statement on the current state of the agent.  The three arguments to the rule represent the agent location, the gold location and the Wumpus location.

First off, there's always something to do, even if that something is to verify there's nothing to do other than win or lose the game (because the following rule matches any location of the agent, gold or Wumpus)...
```
stnd(_, _, _) :-
    format('There\'s still something to do...~n', []).
```

However, in the following rule, if the AL location effectively unifies with the Wumpus location (by matching both the first and third arguments), it's game over (*stnd(AL,_,AL)*) and the rule resolves as a fail to break out of the recursion in *take_steps* 	and end the query (with a final response of "fail").

```
stnd(AL, _, AL) :-
    format('YIKES! You\'re eaten by the wumpus!', []),
    fail.
```

Finally, in the following rule, if the AL location unifies with the gold location (by matching both the first and second arguments), the agent wins (*stnd(AL,AL,_)*) and the rule resolves to true to break out of the recursion in *take_steps* with a final response of "true").

```
stnd(AL, AL, _) :-
    format('AGENT FOUND THE GOLD!!', []),
    true.
```


# Agent/Environment Management

## Updating States

These rules update the game metadata (time and score) as well as the current grid square of the agent (i.e. the archer).

### update_time

The following rule resolves the current time value (*T*) and increments it by 1 (*NewTime is T+1*).  The current time is retracted and the updated time is asserted (*time_taken(NewTime)*).

```
update_time :-
    time_taken(T),
    NewTime is T+1,
    retractall( time_taken(_) ),
    assert( time_taken(NewTime) ).
```

### update_score

The following rule resolves the current time value (*T*) and increments it by 1 (*NewTime is T+1*).  The current time fact is retracted and an updated time fact is asserted (*time_taken(NewTime)*).

* define the method
* current agent location on the grid, AL is agent location
* current gold location on the grid, GL is gold location
* current wumpus location on the grid, WL is wunous location
* close the method

```
update_score :-
    agent_location(AL),
    gold_location(GL),
    wumpus_location(WL),
    update_score(AL, GL, WL).
```    

The following rule resolves the current score value (*S*) and increments it by the points value supplied (*P*) to generate the new score (*NewScore is S+P*).  The current score fact is retracted and an updated time fact is asserted (*time_taken(NewTime)*).

```
update_score(P) :-
    score(S),
    NewScore is S+P,
    retractall( score(_) ),
    assert( score(NewScore) ).
```

The following rule resolves if the agent location and the gold location match (via the triplet provided, where the Wumpus location is irrelevant) and adds 1000 to the current score (for finding the gold) by calling the single argument score rule (*update_score(1000)*).

```
update_score(AL, AL, _) :-
    update_score(1000).
```

The following rule decrements the current score by 1 (*update_score(-1)*) each time it is invoked.  This rule a clause within the *take_steps* rule, hence the score is decremented on every agent move.

```
update_score(_,_,_) :-
    update_score(-1).
```
    
The following rule updates the agents new location (specified by the variable *NewAL*) by retracting any old location facts and asserting a new one (*assert( agent_location(NewAL)*).

```
update_agent_location(NewAL) :-
    retractall( agent_location(_) ),
    assert( agent_location(NewAL) ).
```

The following rule determines that the specified location (*X*) is not a pit, using negation by failure (*\+ pit_location(X)*). Prolog assumes that if it can't prove an assertion, then the assertion is false. If *pit_location(X)* can't be proved, it is assumed to be false, hence the rule succeeds indicating no pit at this location.

```
is_pit(no,  X) :-
    \+ pit_location(X).
```

The following rule shows that the specified location (*X*) is indeed a pit, by matching a pit location fact for this location (*pit_location(X)*).  If there is no pit location fact for this location, this rule does not succeed.

```
is_pit(yes, X) :-
    pit_location(X).
```

## Perception

### make_perception

The following rule appears to want to match a given perception triplet (e.g. *[no,yes,no]* spanning the trilogy of smelly, bleezy and gittering to the current agent location.  However, *isStinky(X)* and *isBleezie(X)* do not exist!

_Note: What is "bleezy" anyway! Must be a misspelled variant of "breezy"._

```
make_perception([_Stench,_Bleeze,_Glitter]) :-
    agent_location(AL),
    isStinky(AL),
    isBleezie(AL),
    isGlittering(AL).
```

### test_perception

The following rule simply prints a perception sentence to the console.

```
test_perception :-
	make_percept_sentence(Percept),
	format('I feel ~p, ',[Percept]).
```

### make\_percept_sentence

The following rule constructs a "percept sentence" which is a triplet of the boolean Stench, Bleeze and Glitter values (e.g. *[yes, no, no]*).

* _smelly_ resolves whether Stench is yes or no
* _bleezy_ resolves whether Bleeze is yes or no
* _glittering_ resolves whether Glitter is yes or no

```
make_percept_sentence([Stench,Bleeze,Glitter]) :-
	smelly(Stench),
	bleezy(Bleeze),
	glittering(Glitter).
```

The rule tree for make\_percept_sentence is:

* make\_percept_sentence
    * smelly
        * isSmelly
            * adjacent
                * adj
    * bleezy
        * isBleezy
           * adjacent
               * adj
    * glittering
        * isGlittering

_Note: The resolution processing may not go beyond the first level depending on the argument values.  See the rule descriptions for smelly, bleezy and glittering._

### adj

The “adj” relation defines what it means for two integers to be adjacent to each other (here meant to represent grid square locations where the integers are the co-ordinates along the x or y axis).

_Note: This set of rules assumes a 4x4 grid._

```
adj(1,2).
adj(2,1).
adj(2,3).
adj(3,2).
adj(3,4).
adj(4,3).
```

### adjacent

The following rule specifies that the points (X1, Y1) and (X2, Y2) 
are adjacent if they are either:

1. In the same column (same X) with their Y values adjacent (*adj(Y1, Y2)*).
2. In the same row (same Y) with their X values adjacent (*adj(X1, X2)*).
or on the same row (same y) and their x’s are “adj”.

```
adjacent( [X1, Y1], [X2, Y2] ) :-
    ( X1 = X2, adj( Y1, Y2 )
    ; Y1 = Y2, adj( X1, X2 )
    ).
```

### isSmelly

The following rule shows a location (*Ls1*) to be "smelly" if an adjacent location (*Ls2*) contains the Wumpus (*wumpus_location(Ls2)*).

```
isSmelly(Ls1) :-
    wumpus_location( Ls2 ),
    adjacent( Ls1, Ls2 ).
```

### isBleezy

The following rule shows a location (*Ls1*) to be "bleezy" if an adjacent location (*Ls2*) contains a pit (*pit_location(Ls2)*).    

```
isBleezy(Ls1) :-
    pit_location( Ls2 ),
    adjacent( Ls1, Ls2 ).
```

### isGlittering

The following rule shows a location (*[X1,Y1]*) to be "glittering" if it matches the gold location (*gold_location[X2,Y2]*).   Specifically, glittering is present when the specified location's x,y co-ordinates (*X1 and Y1*) match the gold location's x,y co-ordinates (*X2 and Y2*).
```
isGlittering( [X1, Y1] ) :-
    gold_location( [X2, Y2] ),
    X1 = X2,
    Y1 = Y2.
 ```

### bleezy

The following rule shows that *bleezy* is yes when the agent location and *isBleezy* are the same, otherwise it is no.

```
bleezy(yes) :-
    agent_location(AL),
    isBleezy(AL).
bleezy(no).
```
_Note: The order of the above two rules is important. The yes rule is given the first chance while processing a query for this single argument predicate.  If it can't resolve, then the no fact prevails.  If the fact came first, it would always take precedence during a query and would assert no._

### smelly

The following rule shows that *smelly* is yes when the agent location and *isSmelly* are the same, otherwise it is no.

```
smelly(yes) :-
    agent_location(AL),
    isSmelly(AL).
smelly(no).
```

_Note: Similar to bleezy, the order is important._

### glittering

The following rule shows that *glittering* is yes when the agent location and *isGlittering* are the same, otherwise no.

```
glittering(yes) :-
    agent_location(AL),
    isGlittering(AL).
glittering(no).
```

_Note: Similar to bleezy, the order is important._

## Knowledge Base

### update_KB

The following rule drives three other rules for:

1. resolving Wumpus facts based on the current boolean value of *Stench*
2. resolving pit facts based on the current boolean value of *Bleeze*
3. resolvinggold facts based on the current boolean value of *Glitter*

```
update_KB( [Stench,Bleeze,Glitter] ) :-
    add_wumpus_KB(Stench),
    add_pit_KB(Bleeze),
    add_gold_KB(Glitter).
```

### add\_wumpus_KB

The following rule resolves to show the four adjacent grid squares locations to the current agent location (*agent_location([X,Y])*) do not contain a Wumpus based on their being no stench detected at the current agent location.  The four adjacent squares (*Z1, Z2, Z3, Z4)*) are calculated and set using "is" for assignment.  Each of these calculated locations are then set as facts indicating the Wumpus is not at that particular location (e.g. *assume_wumpus(no, [X,Z1])*).

A rule for storing a confirmed location of the Wumpus is unnecessary, becasue by then, it's too late.

```
add_wumpus_KB(no) :-
    %agent_location(L1),
    %adjacent(L1, L2),
    %assume_wumpus(no, L2).
    agent_location([X,Y]),
    world_size(_),

    % Checking needed!!
    % adj will freeze for (4,_) !!

    Z1 is Y+1, assume_wumpus(no,[X,Z1]),
    Z2 is Y-1, assume_wumpus(no,[X,Z2]),
    Z3 is X+1, assume_wumpus(no,[Z3,Y]),
    Z4 is X-1, assume_wumpus(no,[Z4,Y]).
```

### add\_pit_KB

Based on the value of "Bleeze" used to invoke these rules (i.e. *add_pit_KB(Bleeze)*, facts about a pit location in adjacent grid squares can set.

The following rule (based on *Bleeze* being a "no") invokes clauses that assert facts that there is no pit adjacent to the current agent location.  The values for Z1,Z2,Z3,Z4 are already calculated.

```
add_pit_KB(no) :-
    agent_location([X,Y]),
    Z1 is Y+1, assume_pit(no,[X,Z1]),
    Z2 is Y-1, assume_pit(no,[X,Z2]),
    Z3 is X+1, assume_pit(no,[Z3,Y]),
    Z4 is X-1, assume_pit(no,[Z4,Y]).
```

The following rule (based on *Bleeze* being a "yes") invokes clauses that show there is indeed a pit adjacent to the current agent location.  The values for Z1,Z2,Z3,Z4 are already calculated.

```
add_pit_KB(yes) :-
    agent_location([X,Y]),
    Z1 is Y+1, assume_pit(yes,[X,Z1]),
    Z2 is Y-1, assume_pit(yes,[X,Z2]),
    Z3 is X+1, assume_pit(yes,[Z3,Y]),
    Z4 is X-1, assume_pit(yes,[Z4,Y]).
```

### add\_gold_KB

Based on the value of *Glitter* used to invoke these rules (i.e. *add\_gold_KB(Glitter)*, facts about the gold location in the current grid square can set.

The following rule (based on *Glitter* being a "no") invokes clauses that assert facts that there is no gold at the current agent location.

```
add_gold_KB(no) :-
    gold_location(GL),
    assume_gold(no, GL).
```

The following rule (based on *Glitter* being a "yes") invokes clauses that assert facts that there is indeed gold at the current agent location.  As an additional confirmation, the current agent location and the location of the gold must resolve to the same co-ordinates for the rule to succeed.

```
add_gold_KB(yes) :-
    gold_location([X1,Y1]),
    agent_location([X2,Y2]),
    X1 = X2, Y1 = Y2,
    assume_gold(yes, [X1,Y1]).
```

### assume_wumpus

These rules assert facts about the Wumpus location.

The following rule retracts any facts about the Wumpus at this location and then asserts there is no Wumpus at this location.
```
assume_wumpus(no, L) :-
    retractall( isWumpus(_, L) ),
    assert( isWumpus(no, L) ),
    format('KB learn ~p - no Wumpus there!~n', [L]).
```
The following rule retracts any facts about the Wumpus at this location and then asserts there is a Wumpus at this location.
```
assume_wumpus(yes, L) :-
    %wumpus_healthy, % Will be included ...
    retractall( isWumpus(_, L) ),
    assert( isWumpus(yes, L) ),
    format('KB learn ~p - possibly the Wumpus is there!~n', [L]).
```

### assume_pit

These rules assert facts about the location of pits.

The following rule retracts any facts about a pit at this location and then asserts there is no pit at this location.

```
assume_pit(no, L) :-
    retractall( isPit(_, L) ),
    assert( isPit(no, L) ),
    format('KB learn ~p - there\'s no Pit there!~n', [L]).
```

The following rule retracts any facts about a pit at this location and then asserts there is a pit at this location.

```
assume_pit(yes, L) :-
    retractall( isPit(_, L) ),
    assert( isPit(yes, L) ),
    format('KB learn ~p - its a Pit!~n', [L]).
```

### assume_gold

These rules assert facts about the location of the gold.

The following rule retracts any facts about gold at this location and then asserts there is no gold at this location.

```
assume_gold(no, L) :-
    retractall( isGold(_, L) ),
    assert( isGold(no, L) ),
    format('KB learn ~p - there\'s no gold here!~n', [L]).
```

The following rule retracts any facts about the gold at this location and then asserts there is gold at this location.

```
assume_gold(yes, L) :-
    retractall( isGold(_, L) ),
    assert( isGold(yes, L) ),
    format('KB learn ~p - GOT THE GOLD!!!~n', [L]).
```

### permitted

The followiing rule shows that grid square co-ordinate (*[X,Y]*) is valid (and hence permitted) as long as the X and Y co-ordinates fit on the game grid (i.e. *0<X<WS=1* and *0<Y<WS+1*).

```
permitted([X,Y]) :-
    world_size(WS),
    0 < X, X < WS+1,
    0 < Y, Y < WS+1.
```
### ask_KB

The followiing rule solves for an "action" in the form of a new location by resolving to a safe, valid, unvisited location (*L*) for the agent.

There are 3 key rule checks made:

1. **Checking for danger:** The clauses for ensuring that, as a result of this action, there is no Wumpus (*isWumpus(no,L)*) or a pit (*isPit(no, L)*) must be successful.
* **Checking for validity:** The action must ensure the agent remains on the playing grid (*permitted(L)*).
* **Checking for unique visits:** The action avoids revisiting a location (*not_member(L, VisitedList)*).

After the above checks, the agent location is updated (by unifying L using *update_agent_location(L)*), and the Action variable is unified with the location that passed all the above clauses.

```
ask_KB(VisitedList, Action) :-
    isWumpus(no, L),
    isPit(no, L),
    permitted(L),
    not_member(L, VisitedList),
    update_agent_location(L),
    Action = L.
```

## Utils

### not_member

These are high level list operations that detect whether a set of x,y co-ordinates exist in a list.

**Fact:** If the list is empty ([]), any co-ordinate pair value (_) is considered not to be a member (i.e. not_member is a fact).

```
not_member(_, []).
```

**Rule:** We can show a co-ordinate pair ([X,Y]) is not a member of a list of co-ordinates (the second argument), if we show that both the head of list ([U,V]) comparison to it (as a result of the unification processing) fails *and* it is also not a member of the rest of the list (Ys) which is performed by recursively checking the head of the list of the remaining list (Ys).

```
not_member([X,Y], [[U,V]|Ys]) :-
    ( X=U,Y=V -> fail
    ; not_member([X,Y], Ys)
    ).
```

# Appendix

## Sample Run
```
?-start
Initializing started...
Let the game begin!
I'm in [1,1], seeing: [no,no,no]
KB learn [1,2] - no Wumpus there!
KB learn [1,0] - no Wumpus there!
KB learn [2,1] - no Wumpus there!
KB learn [0,1] - no Wumpus there!
KB learn [1,2] - there's no Pit there!
KB learn [1,0] - there's no Pit there!
KB learn [2,1] - there's no Pit there!
KB learn [0,1] - there's no Pit there!
KB learn [3,2] - there's no gold here!
I'm going to: [1,2]
There's still something to do...
I'm in [1,2], seeing: [no,yes,no]
KB learn [1,3] - no Wumpus there!
KB learn [1,1] - no Wumpus there!
KB learn [2,2] - no Wumpus there!
KB learn [0,2] - no Wumpus there!
KB learn [1,3] - its a Pit!
KB learn [1,1] - its a Pit!
KB learn [2,2] - its a Pit!
KB learn [0,2] - its a Pit!
KB learn [3,2] - there's no gold here!
I'm going to: [2,1]
There's still something to do...
I'm in [2,1], seeing: [no,no,no]
KB learn [2,2] - no Wumpus there!
KB learn [2,0] - no Wumpus there!
KB learn [3,1] - no Wumpus there!
KB learn [1,1] - no Wumpus there!
KB learn [2,2] - there's no Pit there!
KB learn [2,0] - there's no Pit there!
KB learn [3,1] - there's no Pit there!
KB learn [1,1] - there's no Pit there!
KB learn [3,2] - there's no gold here!
I'm going to: [2,2]
There's still something to do...
I'm in [2,2], seeing: [no,no,no]
KB learn [2,3] - no Wumpus there!
KB learn [2,1] - no Wumpus there!
KB learn [3,2] - no Wumpus there!
KB learn [1,2] - no Wumpus there!
KB learn [2,3] - there's no Pit there!
KB learn [2,1] - there's no Pit there!
KB learn [3,2] - there's no Pit there!
KB learn [1,2] - there's no Pit there!
KB learn [3,2] - there's no gold here!
I'm going to: [3,1]
There's still something to do...
I'm in [3,1], seeing: [yes,no,no]
I'm in [3,1], seeing: [no,no,no]
KB learn [3,2] - no Wumpus there!
KB learn [3,0] - no Wumpus there!
KB learn [4,1] - no Wumpus there!
KB learn [2,1] - no Wumpus there!
KB learn [3,2] - there's no Pit there!
KB learn [3,0] - there's no Pit there!
KB learn [4,1] - there's no Pit there!
KB learn [2,1] - there's no Pit there!
KB learn [3,2] - there's no gold here!
I'm going to: [2,3]
There's still something to do...
I'm in [2,3], seeing: [no,yes,no]
KB learn [2,4] - no Wumpus there!
KB learn [2,2] - no Wumpus there!
KB learn [3,3] - no Wumpus there!
KB learn [1,3] - no Wumpus there!
KB learn [2,4] - its a Pit!
KB learn [2,2] - its a Pit!
KB learn [3,3] - its a Pit!
KB learn [1,3] - its a Pit!
KB learn [3,2] - there's no gold here!
I'm going to: [3,2]
There's still something to do...
WON!
Score: 995,
 Time: 6
true
```


#SCS 3547 Assignment 2
======================
## Overview
A detailed analysis of a prolog-based implementation of Wumpus World.
## Team Members:
* Mark Hubbard
* Konrad Korzeniewski
* Mohammed Radha

## Approach:
* Analyzed the overall structure and flow of the program.
* Organized the documentation into "sections" based on the original code structure.
* Documented sections, rules and facts in markdown.
* Documented clauses inline as comments.

## References
* Artificial Intelligence : A Modern Approach (Russel - Norvig)
* Code by:
	* Author: Richard O. Legendi (with permission from above authors)
	* Version: v1.0, Jan. 31, 2007

# Program Analysis


## Structural Analysis
The code can be organized into three main sections:

1. **Program Control** - managing the overall flow from initialization to termination.
2. **Agent/Environment Control** - the rules and facts related to agent movement and environment state.
3. **Knowledge Base** - the set of initial and dynamically determined facts.

In the original code, each section is demarcated into blocks of related rules using a comment block.

The following table shows how the rules are divided up into their respective sections and blocks.

| Program Control      		| Agent/Environment Control | Knowlege Management  
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
     * agent_location - _update the agent location_
     * update_KB - _update what we know in the KB_ ->
         * add\_wumpus_KB - _set Wumpus location if detected_
         * add\_pit_KB -  _set pit location if detected_
         * add\_gold_KB - _set gold location if detected_
     * ask_KB - _query the KB to help decide on a next move_
     * update_time - _update the time elapsed so far_
     * update_score - _update the score so far_
     * step_pre - _check if the game has ended (i.e. kill or be killed)_
         * take_steps - _recurse making moves until game ends_

## Variables:
* Co-ordinates:
    * X1, Y1 - grid square location of the agent (i.e. the archer).
    * X2, Y2 - proposed next adjacent grid square to which the agent may move.
    * Z1, Z2, Z3, Z4 - the immediately adjacent grid squares of the agent (North, South, East, West).
* Locations: 
    * AL - agent location using grid square coordinates (e.g. x, y)
    * GL - gold location using grid square coordinates (e.g. x, y)
    * WL - wumpus location using grid square coordinates (e.g. x, y)
    * VL = the accrued list of grid squares (as co-ordinates) already visited
* Game Status:
    * NewTime - elapsed following each move
    * NewScore - latest score following each move
* Perceptions (enum):
    * Bleeze - used to assert a fact that, from a given grid square, the agent can perceive "bleeze". 
    * Glitter - used to assert a fact that a given grid square contains gold.     
    * Stench - used to assert a fact that, from a given grid square, the agent can smell a stench.     
* Agent Movement:
	 * Action -  
    * Percept - 
    * VisitedList -  

## Dynamics
The following structures are made dynamic so that clauses can be dynamically added (via assert) and removed (via retract):

* agent_location/1 - stores the current agent location as a grid square co-ordinate (x,y co-ordinates)
* gold_location/1,   % stores the gold location as a grid square (x,y co-ordinates)  
* pit_location/1,    % stores the pit location as a grid square (x,y co-ordinates)  
* time_taken/1,      % records the duration of the game (number)
* score/1,           % records the current score (number)
* visited/1,         % marks the grid squares already visited (boolean)
* visited_cells/1,   % tracks the grid square co-ordinates (list of x,y co-ordinates)
* world_size/1,      % stores the size of the grid (n x n squares)
* wumpus_location/1, % stores the wumpus location (x,y co-ordinates)
* isPit/2,           % indicates whether the grid square is a pit (x,y co-ordinates and a boolean)
* isWumpus/2,        % is the grid square occupied by the Wumpus? ((x,y co-ordinates and a boolean)
* isGold/2           % is the grid square filled with the gold? ((x,y co-ordinates and a boolean)

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
This is the entry point of the program (invoked using ?-start).  The query "start" (invoked from the command line) discovers and invokes the "start" rule to prove the query.  The start rule executes the init and take_steps clauses to respond to the query.  The rule is proven if the init and take_steps clauses succeed (as well as the format statements that print to the console). The predicate "take_steps" either succeeds or fails depending on the fate of the agent in the game.

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

Note: At this point, the location of the pits, gold, agent or Wumpus have not been addressed.

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
This rule retracts and then asserts all the facts for the size of the board (*world_size*) and the location of the gold (*gold_location*) and all pits (*pit_location*) according to Figure 7.2 in Russel-Norvig's book (2nd Edition).

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
This rule retracts all facts regarding the agent and then places it on grid square [1,1] (*agent_location([1,1]*). Additionaly, that square is added to the list of visited squares (via *visit([1,1]*).

```
init_agent :-
    retractall( agent_location(_) ),
    assert( agent_location([1,1]) ),
    visit([1,1]).
```
### init_wumpus
This rule retracts all facts regarding the Wumpus and then places it on grid square [4,1](*wumpus_location([4,1]*).

```
init_wumpus :-
    retractall( wumpus_location(_) ),
    assert( wumpus_location([4,1]) ).
```
### visit
The rule sets the facts arouond the list of visited cells.  The specified grid square co-ordinates (*Xs*) are added to the end of the existinig list (*Ys*). To add to the end of the list, the list is first emptied (using retractall) and then reconstructed with the new grid square at the end of the list (*visited_cells[Ys|Xs]*).

```
visit(Xs) :-
    visited_cells(Ys),
    retractall( visited_cells(_) ),
    assert( visited_cells([Ys|Xs]) ).
```

## Scheduling simulation:
These methods plot out the agents path through the world's grid squares.

### step_pre
Before a step is taken (not including the first step), the current state of the game is evaluated.
The initial clauses in this rule gather the facts around the current location of the agent (*agent_location(AL)*), the location of the gold (*gold_location(GL)*) and the location of the Wumpus (*wumpus_location(GL)*).  The current score (*score(S)*) and game time (*time_taken(T)*) facts are also gathered.

Once the facts are all gathered into variables, a conjuctive clause is invoked to determine whether the game is over or it should contine.

The game is over if:

1. The agent location (*AL*) and gold location (*GL*) variables can be matched/unified (*AL=GL*), which consequently declares a win and prints the score and time taken.
2. The agent location (*GL*) and Wumpus location (*WL*) variables can be matched/unified (*AL=WL*), which consequently declares a loss and prints the score (*S*) and time taken(*T*).

If the game is not over, the next steps are taken based on the steps already taken (*take_steps(Visited_List)*).

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
Taking a step involves taking a look before the leap, and several fact updates after making a move. 

First, this rule collects an overall set of facts into a variable (*make_percept_sentence(Perception)*).  These facts (or "sentence") are a triplet of stench, bleeze and glitter based on the current agent location (*AL*).

The knowledge base is updated with the overall perception facts from this agent location.(*update_KB(Perception)*).

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
The rule detects whether the agent is still standing.  The initial clauses collect the facts around wumpus location, gold location and agent location and unifies them into their respective global variables (*WL, GL and AL*).

if the agent location matches with a pit location (*is_pit(yes, AL)*), then this rule prints out the fate and resolves as a "fail" (which breaks out of the recursion in *take_steps*).  It conjunctively displays a summary of the overall state of the agent (*stnd(AL,GL,WL)*) rule with the pit rule to display the summary of the current state.

```
standing :-
    wumpus_location(WL),
    gold_location(GL),
    agent_location(AL),

    ( is_pit(yes, AL) -> format('Agent was fallen into a pit!~n', []),
      fail
    ; stnd(AL, GL, WL)
      %\+ pit_location(yes, Al),
    ).
```

### stnd
This rule resolves by printing out a statement on the current state of the agent.  The three arguments to the rule represent the agent location, the gold location and the Wumpus location.

First off, there's always something to do (this rule matches any location variables)...
```
stnd(_, _, _) :-
    format('There\'s still something to do...~n', []).
```

However, if the AL location effectively unifies with the Wumpus location (by matching both the first and third arguments), it's game over (*stnd(AL,_,AL)*) and the rule fails.

```
stnd(AL, _, AL) :-
    format('YIKES! You\'re eaten by the wumpus!', []),
    fail.
```
If the AL location unifies with the gold location (by matching both the first and second arguments), the agent wins (*stnd(AL,AL,_)*) and the rule fails.

```
stnd(AL, AL, _) :-
    format('AGENT FOUND THE GOLD!!', []),
    true.
```


# Agent/Environment Management

## Updating States
These rules update the game metadata (time and score) as well as the current grid square of the agent (i.e. the archer).

### update_time
* define the methiod
* take current time 
* increment by 1 to get a new time
* remove old time fact 
* imput new time fact 

```
update_time :-
    time_taken(T),
    NewTime is T+1,
    retractall( time_taken(_) ),
    assert( time_taken(NewTime) ).
```

### update_score
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

* define the method
* current agent score, S is score
* define new score by adding value from the update method P to the score S
* remove old score whatever the current value is
* input the new score, NewScore is score

```
update_score(P) :-
    score(S),
    NewScore is S+P,
    retractall( score(_) ),
    assert( score(NewScore) ).
```

* This is a performance measure, add 1000 to the general score for getting the gold
* tile locations and updated score are variables 

```
update_score(AL, AL, _) :-
    update_score(1000).
```


* This is a performance measure, subtract 1 from the general score for any action taken 
* updated score is a variable

```
update_score(_,_,_) :-
    update_score(-1).
```
    
* agent location is NewAL
* remove current agent location
* input new agant location, NewAl is agent location

```
update_agent_location(NewAL) :-
    retractall( agent_location(_) ),
    assert( agent_location(NewAL) ).
```

* X is not in a pit
* X is a pit location, (predicate is fact terminated by a period) 

```
is_pit(no,  X) :-
    \+ pit_location(X).
```

* X is in the pit
* X is a pit location, (predicate is fact terminated by a period) 

```
is_pit(yes, X) :-
    pit_location(X).
```

## Perception

### make_perception

* records percepts at the agent location at grid square for variables 
* for Stench from isStinky
* for Bleeze from isBleezie
* for Glitter from isGlittering 

```
make_perception([_Stench,_Bleeze,_Glitter]) :-
    agent_location(AL),
    isStinky(AL),
    isBleezie(AL),
    isGlittering(AL).
```

### test_perception
* call all percepts as defined in make_percept_sentence
* Output text with a current percept (smelly/bleezy/glittering)

```
test_perception :-
	make_percept_sentence(Percept),
	format('I feel ~p, ',[Percept]).
```

### make\_percept_sentence
* define what percepts will be used in a sentence
* Stench is smelly
* Bleeze is bleezy
* Glitter is glittering

```
make_percept_sentence([Stench,Bleeze,Glitter]) :-
	smelly(Stench),
	bleezy(Bleeze),
	glittering(Glitter).
```

## Perceptors


%%% Institiation error!!!

%adj(X,Y) :-
%    world_size(WS),
%    ( X is Y+1, Y   < WS
%    ; X is Y-1, Y-1 > 0
%    ).

### adj

* The “adj” relation defines what it means for two integers 
(here meant to represent cell locations in the x or y direction) to be adjacent to each other.
* The “adjacent” rule specifies that the points (x1, y1) and (x2, y2) 
are adjacent if they are on the same column (same x) and their y’s are “adj”, 
or on the same row (same y) and their x’s are “adj”.

* Size of the playing matrix (WS) is calculated by X and Y 

```
adj(1,2).
adj(2,1).
adj(2,3).
adj(3,2).
adj(3,4).
adj(4,3).
```

### adjacent
Available tiles' location are related to each other

```
adjacent( [X1, Y1], [X2, Y2] ) :-
    ( X1 = X2, adj( Y1, Y2 )
    ; Y1 = Y2, adj( X1, X2 )
    ).
```

X1 and X2 are adjacent by being on the same column (X) and being related to row Y

```
%adjacent([X,Y1],[X,Y2]) :-
%    adj(Y1,Y2).
```

Similar as above but for rows (Y) with respoect to column X

```
%adjacent([X1,Y],[X2,Y]) :-
%    adj(X1,X2).
```

### isSmelly
smelly tile is detected if wumpus location is Ls2 and Ls1 is adjacent to Ls2.
```
isSmelly(Ls1) :-
    wumpus_location( Ls2 ),
    adjacent( Ls1, Ls2 ).
```

### isBleezy
    
bleezy tile is detected if pit location is Ls2 and Ls1 is adjacent to Ls2.

```
isBleezy(Ls1) :-
    pit_location( Ls2 ),
    adjacent( Ls1, Ls2 ).
```

### isGlittering
* Glittering is present when its attributes X1 and Y1 are passed gold location in X2 and Y2.
* Glittering's X1 is Gold location's X2.
* Glittering's Y1 is Gold location's Y2.
```
isGlittering( [X1, Y1] ) :-
    gold_location( [X2, Y2] ),
    X1 = X2,
    Y1 = Y2.
 ```

### bleezy
* Bleezy is yes when agent_location and isBleezy are the same values, otherwise no.
```
bleezy(yes) :-
    agent_location(AL),
    isBleezy(AL).
bleezy(no).
```

### smelly
* smelly is yes when agent location and isSmelly are the same values, otherwise no.
```
smelly(yes) :-
    agent_location(AL),
    isSmelly(AL).
smelly(no).
```

### glittering
* glittering is yes when agent_location and isGlittering are the same valuesm, otherwise no.
```
glittering(yes) :-
    agent_location(AL),
    isGlittering(AL).
glittering(no).
```

## Knowledge Base

### update_KB

```
update_KB( [Stench,Bleeze,Glitter] ) :-
    add_wumpus_KB(Stench),
    add_pit_KB(Bleeze),
    add_gold_KB(Glitter).
```
### add_wumpus_KB
if it would be 'yes' -> it would mean the player is eaten ;]
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
### add_pit_KB
```
add_pit_KB(no) :-
    agent_location([X,Y]),
    Z1 is Y+1, assume_pit(no,[X,Z1]),
    Z2 is Y-1, assume_pit(no,[X,Z2]),
    Z3 is X+1, assume_pit(no,[Z3,Y]),
    Z4 is X-1, assume_pit(no,[Z4,Y]).
```

% Checking needed!! If its not already in the KB !!!

```
add_pit_KB(yes) :-
    agent_location([X,Y]),
    Z1 is Y+1, assume_pit(yes,[X,Z1]),
    Z2 is Y-1, assume_pit(yes,[X,Z2]),
    Z3 is X+1, assume_pit(yes,[Z3,Y]),
    Z4 is X-1, assume_pit(yes,[Z4,Y]).
```

### add_gold_KB
```
add_gold_KB(no) :-
    gold_location(GL),
    assume_gold(no, GL).
```

```
add_gold_KB(yes) :-
    gold_location([X1,Y1]),
    agent_location([X2,Y2]),
    X1 = X2, Y1 = Y2,
    assume_gold(yes, [X1,Y1]).
```

### assume_wumpus
```
assume_wumpus(no, L) :-
    retractall( isWumpus(_, L) ),
    assert( isWumpus(no, L) ),
    format('KB learn ~p - no Wumpus there!~n', [L]).
```

```
assume_wumpus(yes, L) :-
    %wumpus_healthy, % Will be included ...
    retractall( isWumpus(_, L) ),
    assert( isWumpus(yes, L) ),
    format('KB learn ~p - possibly the Wumpus is there!~n', [L]).
```

### assume_pit
```
assume_pit(no, L) :-
    retractall( isPit(_, L) ),
    assert( isPit(no, L) ),
    format('KB learn ~p - there\'s no Pit there!~n', [L]).
```

```
assume_pit(yes, L) :-
    retractall( isPit(_, L) ),
    assert( isPit(yes, L) ),
    format('KB learn ~p - its a Pit!~n', [L]).
```
### assume_gold
```
assume_gold(no, L) :-
    retractall( isGold(_, L) ),
    assert( isGold(no, L) ),
    format('KB learn ~p - there\'s no gold here!~n', [L]).
```

```
assume_gold(yes, L) :-
    retractall( isGold(_, L) ),
    assert( isGold(yes, L) ),
    format('KB learn ~p - GOT THE GOLD!!!~n', [L]).
```

### permitted

```
permitted([X,Y]) :-
    world_size(WS),
    0 < X, X < WS+1,
    0 < Y, Y < WS+1.
```
### ask_KB

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

**Rule:** We can show a co-ordinate pair ([X,Y]) is not a member of a list of co-ordinates (the second argument), if we show that both the head of list ([U,V]) comparison (unification) fails *and* the pair is also not a member of the rest of the list (Ys) by recursively checking the head of the list of the remaining list (Ys).

```
not_member([X,Y], [[U,V]|Ys]) :-
    ( X=U,Y=V -> fail
    ; not_member([X,Y], Ys)
    ).
```


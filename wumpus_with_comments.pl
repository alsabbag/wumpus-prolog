%------------------------------------------------------------------------------
% SCS 3547 Assignment 2
%
% Team Members:
%   Mark Hubbard
%   Konrad Korzeniewski
%   Mohammed Radha
%
% Approach:
%   Added inline comments to describe each section, method, statement.
%   Added inline tracing to elaborate on the execution flow.
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% A Prolog Implementation of the Wumpus World described in
% Artificial Intelligence : A Modern Approach (Russel - Norvig)
%
% Mandatory Excercise 2007
% v1.0 - Jan. 31, 2007
% Richard O. Legendi
%
% Copied into prolog-examples with permission Richard O. Legendi
% Original exercise descriped in  Artificial Intelligence : A Modern Approach (Russel - Norvig)
%
% Usage:
% consult this file
% ?-start.
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% Methods:
%   * Overall Flow *
%       Mainline:
%           start
%
%       Display standings:
%           standing
%           stnd
%
%       Initializing:
%           init
%           init_game
%           init_land_fig72
%           init_agent
%           init_wumpus
%           visit
%
%       Scheduling simulation:
%           step_pre
%           take_steps
%
%   * Agent/Environment Control *
%       Perceptors:
%           adj
%           adjacent
%           isBleezy
%           isGlittering
%           isSmelly
%           bleezy
%           glittering
%           smelly
%
%       Perceptotion:
%           make_percept_sentence
%           make_perception
%           test_perception
%
%       Updating states:
%           is_pit
%           update_agent_location
%           update_score
%           update_time
%
%   * Knowledge Representation *
%       Knowledge Base (KB)
%           ask_KB
%           add_gold_KB
%           add_pit_KB
%           add_wumpus_KB
%           assume_gold
%           assume_pit
%           assume_wumpus
%           permitted
%           update_KB
%
%       Utils
%           not_member
%           not_member
%
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% Dymamic Methods:
%   Declaring dynamic methods used for storing variables.
%------------------------------------------------------------------------------
:- dynamic ([
	     agent_location/1,  % stores the current agent location as a grid quare (dynamic)
	     gold_location/1,   % stores the gold location as a grid square (static)  
	     pit_location/1,    % stores the pit location as a grid square (static)   
	     time_taken/1,      % records the duration of the game
	     score/1,           % tracks the score
	     visited/1,         % tracks the grid squares already visited (boolean)
	     visited_cells/1,   % tracks the grid square co-ordinates (x,y coordinates)
	     world_size/1,      % stores the size of the grid (n x n squares)
	     wumpus_location/1, % stores the wumpus location
             isPit/2,           % is the grid square a pit? (boolean)
             isWumpus/2,        % is the grid square occupied by the Wumpus? (boolean)
             isGold/2           % is the grid square filled with the gold? (boolean)
	    ]).
%------------------------------------------------------------------------------
% Global Variables:
%   Coordinates:
%       X1, Y1 = grid square location of the agent (i.e. the archer).
%       X2, Y2 = proposed next adjacent grid to which the agent may move.
%       Z1, Z2, Z3, Z4 = the immediately adjacent squares of the agent (N,E,W,S).
%   Locations: 
%       AL = agent location using grid square coordinates (e.g. x, y)
%       GL = gold location using grid square coordinates (e.g. x, y)
%       WL = wumpus location using grid square coordinates (e.g. x, y)
%       VL = the accrued list of grid squares (as co-ordinates) already visited
%   Game Status:
%       NewTime = elapsed following each move
%       NewScore = latest score following each move
%   Perceptions (enum):
%       Bleeze = 
%       Glitter =
%       Stench =
%   Agent Movement:
%       Action = 
%       Percept = 
%       VisitedList = 
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% Mainline:
%  Controls the overall flow of the program.
%------------------------------------------------------------------------------

% To start the game:
%   This is the entry point to the program.  Invoked using ?-start.
start :-
    format('Initializing started...~n', []),
    init,
    format('Let the game begin!~n', []),
    take_steps([[1,1]]).

%------------------------------------------------------------------------------
% Scheduling simulation:
%   These methods plot out the agents path through the world's grid squares.
%------------------------------------------------------------------------------

%
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

%
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

%------------------------------------------------------------------------------
% Updating states
%   These methods update the game metadata (time and score) as well as
%   the current grid square of the agent (i.e the archer).
%------------------------------------------------------------------------------

%
update_time :-
    time_taken(T),
    NewTime is T+1,
    retractall( time_taken(_) ),
    assert( time_taken(NewTime) ).

% define the methiod
% take current time 
% increment by 1 to get a new time
% remove old time fact 
% imput new time fact 


%
update_score :-
    agent_location(AL),
    gold_location(GL),
    wumpus_location(WL),
    update_score(AL, GL, WL).
    
% define the method
% current agent location on the grid, AL is agent location
% current gold location on the grid, GL is gold location
% current wumpus location on the grid, WL is wunous location
% close the method


%
update_score(P) :-
    score(S),
    NewScore is S+P,
    retractall( score(_) ),
    assert( score(NewScore) ).

% define the method
% current agent score, S is score
% define new score by adding value from the update method P to the score S
% remove old score whatever the current value is
% input the new score, NewScore is score


%
update_score(AL, AL, _) :-
    update_score(1000).

% Recursive predicate ? 

%
update_score(_,_,_) :-
    update_score(-1).

% ?????????????????????

%
update_agent_location(NewAL) :-
    retractall( agent_location(_) ),
    assert( agent_location(NewAL) ).
    
% agent location is NewAL
% remove current agent location
% input new agant location, NewAl is agent location

%
is_pit(no,  X) :-
    \+ pit_location(X).

% X is not in a pit
% X is a pit location, (predicate is fact terminated by a period) 

%
is_pit(yes, X) :-
    pit_location(X).
    
% X is in the pit
% X is a pit location, (predicate is fact terminated by a period) 

%------------------------------------------------------------------------------
% Display standings
%   These methods print out the current state of the game after each agent
%   move.
%------------------------------------------------------------------------------

%
standing :-
    wumpus_location(WL),
    gold_location(GL),
    agent_location(AL),

    ( is_pit(yes, AL) -> format('Agent was fallen into a pit!~n', []),
      fail
    ; stnd(AL, GL, WL)
      %\+ pit_location(yes, Al),
    ).

%
stnd(_, _, _) :-
    format('There\'s still something to do...~n', []).

%
stnd(AL, _, AL) :-
    format('YIKES! You\'re eaten by the wumpus!', []),
    fail.

%
stnd(AL, AL, _) :-
    format('AGENT FOUND THE GOLD!!', []),
    true.

%------------------------------------------------------------------------------
% Perception
%------------------------------------------------------------------------------

make_perception([_Stench,_Bleeze,_Glitter]) :-
    agent_location(AL),
    isStinky(AL),
    isBleezie(AL),
    isGlittering(AL).

% records percepts at the agent location at grid square for variables 
% for Stench from Stinky
% for Bleeze from Bleezie
% for Glitter from Glittering 


test_perception :-
	make_percept_sentence(Percept),
	format('I feel ~p, ',[Percept]).

% call all percepts as defined in make_percept_sentence
% Output text with a current percept (smelly/bleezy/glittering)


make_percept_sentence([Stench,Bleeze,Glitter]) :-
	smelly(Stench),
	bleezy(Bleeze),
	glittering(Glitter).

% define what percepts will be used in a sentence
% Stench is smelly
% Bleeze is bleezy
% Glitter is glittering

%------------------------------------------------------------------------------
% Initializing
%------------------------------------------------------------------------------

init :-
    init_game,
    init_land_fig72,
    init_agent,
    init_wumpus.

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

% To set the situation described in Russel-Norvig's book (2nd Ed.),
% according to Figure 7.2
init_land_fig72 :-
    retractall( world_size(_) ),
    assert( world_size(4) ),

    retractall( gold_location(_) ),
    assert( gold_location([3,2]) ),

    retractall( pit_location(_) ),
    assert( pit_location([4,4]) ),
    assert( pit_location([3,3]) ),
    assert( pit_location([1,3]) ).

init_agent :-
    retractall( agent_location(_) ),
    assert( agent_location([1,1]) ),

    visit([1,1]).

init_wumpus :-
    retractall( wumpus_location(_) ),
    assert( wumpus_location([4,1]) ).

visit(Xs) :-
    visited_cells(Ys),
    retractall( visited_cells(_) ),
    assert( visited_cells([Ys|Xs]) ).

%------------------------------------------------------------------------------
% Perceptors
%------------------------------------------------------------------------------

%%% Institiation error!!!

%adj(X,Y) :-
%    world_size(WS),
%    ( X is Y+1, Y   < WS
%    ; X is Y-1, Y-1 > 0
%    ).

%The “adj” relation defines what it means for two integers 
%(here meant to represent cell locations in the x or y direction) to be adjacent to each other.
%The “adjacent” rule specifies that the points (x1, y1) and (x2, y2) 
%are adjacent if they are on the same column (same x) and their y’s are “adj”, 
%or on the same row (same y) and their x’s are “adj”.

% size of the playing matrix (WS) is calculated by X and Y 


adj(1,2).
adj(2,1).
adj(2,3).
adj(3,2).
adj(3,4).
adj(4,3).

adjacent( [X1, Y1], [X2, Y2] ) :-
    ( X1 = X2, adj( Y1, Y2 )
    ; Y1 = Y2, adj( X1, X2 )
    ).

% Available tiles' location are related to each other


%adjacent([X1,Y],[X2,Y]) :-
%    adj(X1,X2).

% X1 and X2 are adjacent by being on the same column (X) and being related to row Y

%adjacent([X,Y1],[X,Y2]) :-
%    adj(Y1,Y2).

% similar as above but for rows (Y) with respoect to column X

isSmelly(Ls1) :-
    wumpus_location( Ls2 ),
    adjacent( Ls1, Ls2 ).

% smelly tile is detected if wumpus location is Ls2 and Ls1 is adjacent to Ls2

isBleezy(Ls1) :-
    pit_location( Ls2 ),
    adjacent( Ls1, Ls2 ).
    
 % bleezy tile is detected if pit location is Ls2 and Ls1 is adjacent to Ls2

isGlittering( [X1, Y1] ) :-
    gold_location( [X2, Y2] ),
    X1 = X2,
    Y1 = Y2.

% Glittering is present when its attributes X1 and Y1 are passed gold location in X2 and Y2
% Glittering's X1 is Gold location's X2
% Glittering's Y1 is Gold location's y2


bleezy(yes) :-
    agent_location(AL),
    isBleezy(AL).
bleezy(no).

% Bleezy is yes when agent_location and isBleezy are the same values, otherwise no

smelly(yes) :-
    agent_location(AL),
    isSmelly(AL).
smelly(no).

% smelly is yes when agent location and isSmelly are the same values, otherwise no

glittering(yes) :-
    agent_location(AL),
    isGlittering(AL).
glittering(no).

% Glittering is yes when agent_location and isGlittering are the same valuesm, otherwise no

%------------------------------------------------------------------------------
% Knowledge Base:
%------------------------------------------------------------------------------

update_KB( [Stench,Bleeze,Glitter] ) :-
    add_wumpus_KB(Stench),
    add_pit_KB(Bleeze),
    add_gold_KB(Glitter).

% if it would be 'yes' -> it would mean the player is eaten ;]
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

add_pit_KB(no) :-
    agent_location([X,Y]),
    Z1 is Y+1, assume_pit(no,[X,Z1]),
    Z2 is Y-1, assume_pit(no,[X,Z2]),
    Z3 is X+1, assume_pit(no,[Z3,Y]),
    Z4 is X-1, assume_pit(no,[Z4,Y]).

% Checking needed!! If its not already in the KB !!!
add_pit_KB(yes) :-
    agent_location([X,Y]),
    Z1 is Y+1, assume_pit(yes,[X,Z1]),
    Z2 is Y-1, assume_pit(yes,[X,Z2]),
    Z3 is X+1, assume_pit(yes,[Z3,Y]),
    Z4 is X-1, assume_pit(yes,[Z4,Y]).

add_gold_KB(no) :-
    gold_location(GL),
    assume_gold(no, GL).

add_gold_KB(yes) :-
    gold_location([X1,Y1]),
    agent_location([X2,Y2]),
    X1 = X2, Y1 = Y2,
    assume_gold(yes, [X1,Y1]).

assume_wumpus(no, L) :-
    retractall( isWumpus(_, L) ),
    assert( isWumpus(no, L) ),
    format('KB learn ~p - no Wumpus there!~n', [L]).

assume_wumpus(yes, L) :-
    %wumpus_healthy, % Will be included ...
    retractall( isWumpus(_, L) ),
    assert( isWumpus(yes, L) ),
    format('KB learn ~p - possibly the Wumpus is there!~n', [L]).

assume_pit(no, L) :-
    retractall( isPit(_, L) ),
    assert( isPit(no, L) ),
    format('KB learn ~p - there\'s no Pit there!~n', [L]).

assume_pit(yes, L) :-
    retractall( isPit(_, L) ),
    assert( isPit(yes, L) ),
    format('KB learn ~p - its a Pit!~n', [L]).

assume_gold(no, L) :-
    retractall( isGold(_, L) ),
    assert( isGold(no, L) ),
    format('KB learn ~p - there\'s no gold here!~n', [L]).

assume_gold(yes, L) :-
    retractall( isGold(_, L) ),
    assert( isGold(yes, L) ),
    format('KB learn ~p - GOT THE GOLD!!!~n', [L]).

permitted([X,Y]) :-
    world_size(WS),
    0 < X, X < WS+1,
    0 < Y, Y < WS+1.

ask_KB(VisitedList, Action) :-
    isWumpus(no, L),
    isPit(no, L),
    permitted(L),
    not_member(L, VisitedList),
    update_agent_location(L),
    Action = L.

%------------------------------------------------------------------------------
% Utils
%------------------------------------------------------------------------------

not_member(_, []).
not_member([X,Y], [[U,V]|Ys]) :-
    ( X=U,Y=V -> fail
    ; not_member([X,Y], Ys)
    ).

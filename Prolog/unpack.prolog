% unpack.prolog
% Caitlin Cassidy - 21 Oct 2016
% takes a set of nodes, edges, and grafts in a packed graph as input
%      *node(ID,Type,Name).
%      *edge(Parent,Child).
%      *graft(Parent,Child,Vias,ID).
% produces dot2texi code for unpacked graph

% allow dynamic assertions
:- dynamic unpacked_node/1.
:- dynamic unpacked_edge/2.
:- dynamic unpacked_branchpoint/2.
:- dynamic unzipped_graft/2.
:- dynamic node/3.


%%%%%%%%%%%%%%%%%%%%%%%
% command to run;     %
% follow with 'told.' %
%%%%%%%%%%%%%%%%%%%%%%%
unpack:-
	tell('unpacked_grafts_small.txt'),                                              % open output file
	setof(Goal,(node(Goal,_,_),not(edge(Goal,_))),Goals),                     % find non-parent/goal nodes
	iterate_goals(Goals),                                                     % iterate through goal nodes
	setof([Parent,Child,Vias,ID],graft(Parent,Child,Vias,ID),Grafts),         % find grafts
	iterate_grafts(Grafts),                                                   % add grafted branches
	setof(X,unpacked_node(X),Nodes),                                          % find the list of unpacked nodes
	setof([Parent,Child],unpacked_edge(Parent,Child),Edges),			      % find the list of unpacked edges
	write('\\begin{center}\n\\begin{tikzpicture}[>=latex, scale=2.0, transform shape]\n
	     \\begin{dot2tex}[dot,scale=2.0,tikzedgelabels,codeonly]\n\tdigraph G {\n
	     \t\tgraph [nodesep=\"0.5\", ranksep=\"0\"];\n\n'),				      % print dot2tex formatting
	print_nodes(Nodes),                                                       % format required nodes
	write('\n'),
	print_edges(Edges),                                                       % format required edges
	write('\n\t}\n\\end{dot2tex}\n\\end{tikzpicture}\n\\end{center}\n\n'),
	false.	                                                                  % backtrack to next unpacking

% asserts unpacked node, then unpacks Node's parents
% if Node is a branch point, only unpacks one parent
%      if branch point with same name has already been
%      unpacked, unpacks the corresponding parent

% (Base case) if node is already unpacked, ignore
unpack_node(Node):-
	unpacked_node(Node).

% (Base case) if node has no parents, assert unpacked
unpack_node(Node):-
	not(edge(_,Node)),
	assert2(unpacked_node(Node)).

% if node is not a branch point, unpack all parents
unpack_node(Node):-
	not(unpacked_node(Node)),                             % check if already unpacked
	not(node(Node,'branch point',_)),                     % if node is anything other than a branch point
	assert2(unpacked_node(Node)),                         % assert that Node is unpacked
	setof([Parent,Node],edge(Parent,Node),ParentEdges),   % find all Parent->Node edges
	unpack_parents(ParentEdges).                          % unpack all parent nodes

% if node is a branch point, and another branch point
% with the same name has already been unpacked, find parent
% of similar branch point in unpacked edges
unpack_node(Node):-
	not(unpacked_node(Node)),                         % if node is not already unpacked
	node(Node,'branch point',BranchPointName),	      % and if node is a branch point,
	unpacked_branchpoint(BranchPointName,BranchName), % and if a branch point with this name has already been unpacked
	node(Parent,'branch',BranchName),                 % find a branch node with the correct branch name
	edge(Parent,Node),                                % make sure branch node is a parent of Node
	assert2(unpacked_node(Node)),                     % assert that node is unpacked
	assert2(unpacked_edge(Parent,Node)),		      % assert that edge is unpacked
	unpack_node(Parent).				              % unpack parent node

% if node is a branch point (and no others with the same
% name have been unpacked), unpack one parent; will backtrack
% for others
unpack_node(Node):-
	not(unpacked_node(Node)),                                  % if node is not already unpacked
	node(Node,'branch point',BranchPointName),                 % and if node is a branch point
	not(unpacked_branchpoint(BranchPointName,BranchName)),     % and if no branch point with this name has been unpacked
	edge(Parent,Node),                                         % find a parent (any parent) of this branch point
	node(Parent,'branch',BranchName),                          % get the parent's name
	assert2(unpacked_branchpoint(BranchPointName,BranchName)), % assert that branch point is unpacked
	assert2(unpacked_node(Node)),                              % assert that node is unpacked
	assert2(unpacked_edge(Parent,Node)),                       % assert that edge is unpacked
	unpack_node(Parent).                                       % unpack parent node



% iterates through list of Parent->Child edges
% unpacks each parent node
unpack_parents([]). % base case (all parents unpacked)
unpack_parents([[Parent,Child]|T]):-      % for each edge
	assert2(unpacked_edge(Parent,Child)), %    assert that edge is unpacked
	unpack_node(Parent),                  %    unpack the parent node
	unpack_parents(T).                    %    move on to the next edge



% iterates through list of non-parent nodes
% unpacks each goal
iterate_goals([]). % base case
iterate_goals([H|T]):-
	unpack_node(H),
	iterate_goals(T).



% iterates through nodes in unpacked graph
% produces dot2texi formatting
print_nodes([]).
print_nodes([H|T]):-
	node(H,Style,Label),
	write('\t\t'),
	write(H),
	write(' [style=\"'),
	write(Style),
	write('\", label=\"'),
	write(Label),
	write('\"];\n'),
	print_nodes(T).



% iterates through edges in unpacked graph
% produces dot2texi formatting
print_edges([]).
print_edges([[H1,H2]|T]):-
	write('\t\t'),
	write(H1),
	write(' -> '),
	write(H2),
	write(';\n'),
	print_edges(T).



% re-defined assert and retract
% not immune to backtracking
assert2(X) :- assert(X).
assert2(X) :- retract(X), fail.

retract2(X)       :- X, reallyRetract2(X).
reallyRetract2(X) :- retract(X).
reallyRetract2(X) :- assert(X), fail.



% if a branch point in a via is unpacked, we don't need to copy its path
% so we remove the branch point from the via before unzipping
remove_unpacked_branchpoints([],[]).
remove_unpacked_branchpoints([[H1,H2]|T],Return):-         % if head is unpacked branch point
	unpacked_branchpoint(H1,H2),
	remove_unpacked_branchpoints(T,Return).                % remove unpacked bp's from tail
remove_unpacked_branchpoints([[H1,H2]|T],[[H1,H2]|T2]):-   % if head is not unpacked branch point
	not(unpacked_branchpoint(H1,H2)),
	remove_unpacked_branchpoints(T,T2).                    % remove unpacked bp's from tail and append


% iterate_grafts: iterates through grafts, making branch copies as necessary
% base case
iterate_grafts([]).

% if all vias are unpacked, move on
iterate_grafts([[_,_,Vias,_]|T]):-
	remove_unpacked_branchpoints(Vias,[]), % if all branch points in Vias unpacked
	iterate_grafts(T).                     % move on to next graft

% if no vias are unpacked, unzip all
iterate_grafts([[Parent,Child,Vias,ID]|T]):-
	remove_unpacked_branchpoints(Vias,NewVias),
	unzip_graft(Parent,NewVias,Child,ID),       % make copies along path
	retract2(unpacked_edge(Parent,Child)),      % retract (originalParent -> Child) edge
	assert2(unzipped_graft(NewVias,Child)),     % assert vias unzipped
	iterate_grafts(T).                          % move on to next graft



% unzip_graft: copies path from graft to branch point
% (base case) if vias already unzipped
unzip_graft(_,Vias,Prev,_):-
	unzipped_graft(Vias,Child),
	unpacked_edge(Parent,Child),	     % find end of unzippped path
	assert2(unpacked_edge(Parent,Prev)). % attach path to previous node

% (base case) if node is highest branch point
unzip_graft(Node,[[BranchPointName,BranchName]],Prev,ID):-
	node(Node,'branch point',BranchPointName),              % Node is top branch point
	edge(Parent,Node),                                      % find parent
	node(Parent,'branch',BranchName),                       % if parent is correct branch
	unpack_node(Parent),                                    % unpack parent branch
	string_concat(Node,ID,NodeCopy),                        % create copy of Node
	assert2(node(NodeCopy,'branch point',BranchPointName)), % assert node copy
	assert2(unpacked_node(NodeCopy)),                       % assert node copy is in unpacked graph
	assert2(unpacked_edge(Parent,NodeCopy)),		        % assert (branch -> node copy) is in unpacked edges
	assert2(unpacked_edge(NodeCopy,Prev)).                  % assert (node copy -> previous copy) is in unpacked edges

% if Node is a branch point, but not highest
unzip_graft(Node,Vias,Prev,ID):-
	node(Node,'branch point',BranchPointName),		        % Node is a branch point
	edge(Parent,Node),                                      % find parent node
	node(Parent,'branch',BranchName),                       % if Parent is correct branch
	member([BranchPointName,BranchName],Vias),              % if branch point in Vias
	string_concat(Node,ID,NodeCopy),                        % create copy of node
	delete(Vias,[BranchPointName,BranchName],NewVias),      % remove graft from Vias
	assert2(node(NodeCopy,'branch point',BranchPointName)), % assert node copy
	assert2(unpacked_node(NodeCopy)),                       % assert node copy is in unpacked graph
	assert2(unpacked_edge(NodeCopy,Prev)),                  % assert (node copy ->previous copy) is in unpacked edges
	unzip_graft(Parent,NewVias,NodeCopy,ID).                % move on up the tree

% if node is not a branch point
unzip_graft(Node,Vias,Prev,ID):-
	node(Node,Type,Label),		               % get node type and label
	not(Type = 'branch point'),                % make sure type is not 'branch point'
	not(unzipped_graft(Vias,_)),               % check if vias unzipped
	setof(Parent,edge(Parent,Node),Parents),
	%edge(Parent,Node),                         % find parent node
	string_concat(Node,ID,NodeCopy),           % create copy of node
	assert2(node(NodeCopy,Type,Label)),        % assert node copy
	assert2(unpacked_node(NodeCopy)),          % assert node copy is in unpacked graph
	assert2(unpacked_edge(NodeCopy,Prev)),     % assert (node copy -> previous copy) is in unpacked edges
	%unzip_graft(Parent,Vias,NodeCopy,ID).      % move on up the tree
	unzip_parents(Parents,Vias,NodeCopy,ID).


% unzip_parents: unzips graft of each parent node (buggy)
unzip_parents([],_,_,_). % base case
unzip_parents([H|T],Vias,NodeCopy,ID):- % if graft can be unzipped,
	unzip_graft(H,Vias,NodeCopy,ID),    % unzip and move on to next parent
	unzip_parents(T,Vias,NodeCopy,ID).
%unzip_parents([H|T],Vias,NodeCopy,ID):- % if unzip fails,
	%assert2(unpacked_edge(H,NodeCopy)), % attach bottom of path to child
	%unzip_parents(T,Vias,NodeCopy,ID).  % and move on to next parent
	
node(foo,'task','foo').
node(bar,'task','bar').
node(bp,'branch point','bp').
node(a,'branch','a').
node(b,'branch','b').
node(out,'output','out').
node(a_val,'value','1').
node(b_val,'value','2').
node(in1,'input','in').
node(in2,'input','in').
node(x,'x','x').

edge(a_val,a).
edge(b_val,b).
edge(a,bp).
edge(b,bp).
edge(bp,x).
edge(x,foo).
edge(foo,out).
edge(out,in1).
edge(out,in2).
edge(in1,bar).
edge(in2,bar).

graft(out,in1,[['bp','a']],1).
graft(out,in2,[['bp','b']],2).

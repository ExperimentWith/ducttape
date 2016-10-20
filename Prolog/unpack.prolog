% unpack.prolog
% Caitlin Cassidy - 19 Oct 2016
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
	tell('unpacked_grafts.txt'),                                                          % open output file
	setof(Goal,(node(Goal,_,_),not(edge(Goal,_))),Goals),                               % find non-parent/goal nodes
	iterate_goals(Goals),                                                                 % iterate through goal nodes
	setof([Parent,Child,Vias,ID],graft(Parent,Child,Vias,ID),Grafts),                   % find grafts
	iterate_grafts(Grafts),                                                               % add grafted branches
	setof(X,unpacked_node(X),Nodes),                                                    % find the list of unpacked nodes
	setof([Parent,Child],unpacked_edge(Parent,Child),Edges),			      % find the list of unpacked edges
	write('\\begin{center}\n\\begin{tikzpicture}[>=latex, scale=2.0, transform shape]\n
	     \\begin{dot2tex}[dot,scale=2.0,tikzedgelabels,codeonly]\n\tdigraph G {\n
	     \t\tgraph [nodesep=\"0.5\", ranksep=\"0\"];\n\n'),				      % print dot2tex formatting
	print_nodes(Nodes),                                                             % format required nodes
	write('\n'),
	print_edges(Edges),                                                             % format required edges
	write('\n\t}\n\\end{dot2tex}\n\\end{tikzpicture}\n\\end{center}\n\n'),
	false.	                                                                              % backtrack to next unpacking

% asserts unpacked node, then unpacks Node's parents
% if Node is a branch point, only unpacks one parent
%      if branch point with same name has already been
%      unpacked, unpacks the corresponding parent

% (Base case) if node is already unpacked, ignore
unpack_node(Node):-
	unpacked_node(Node).

unpack_node(Node):-
	not(edge(Parent,Node)),
	assert2(unpacked_node(Node)).

% if node is not a branch point, unpack all parents
unpack_node(Node):-
	not(unpacked_node(Node)),                             % check if already unpacked
	not(node(Node,'branch point',_)),                     % if node is anything other than a branch point
	assert2(unpacked_node(Node)),                         % assert that Node is unpacked
	setof([Parent,Node],edge(Parent,Node),ParentEdges), % find all Parent->Node edges
	unpack_parents(ParentEdges).                          % unpack all parent nodes

% if node is a branch point, and another branch point
% with the same name has already been unpacked, find parent
% of similar branch point in unpacked edges
unpack_node(Node):-
	not(unpacked_node(Node)),                         % if node is not already unpacked
	node(Node,'branch point',BranchPointName),	  % and if node is a branch point,
	unpacked_branchpoint(BranchPointName,BranchName), % and if a branch point with this name has already been unpacked
	node(Parent,'branch',BranchName),                 % find a branch node with the correct branch name
	edge(Parent,Node),                                % make sure branch node is a parent of Node
	assert2(unpacked_node(Node)),                     % assert that node is unpacked
	assert2(unpacked_edge(Parent,Node)),		  % assert that edge is unpacked
	unpack_node(Parent).				  % unpack parent node

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
unpack_parents([[Parent,Child]|T]):-          % for each edge
	assert2(unpacked_edge(Parent,Child)), % assert that edge is unpacked
	unpack_node(Parent),                  % unpack the parent node
	unpack_parents(T).                    % move on to the next edge



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
/*
print_edges([[H1,H2]|T]):-
	graft(H1,H2,Vias,_),
	write('\t\t'),
	write(H1),
	write(' -> '),
	write(H2),
	write(' [label=\"'),write(Vias),write('\", lblstyle="graft"];\n'),
	print_edges(T),!.
*/
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
remove_unpacked_branchpoints([[H1,H2]|T],Return):-
	unpacked_branchpoint(H1,H2),
	remove_unpacked_branchpoints(T,Return),!.
remove_unpacked_branchpoints([[H1,H2]|T],[[H1,H2]|T2]):-
	%not(unpacked_branchpoint(H1,H2)),
	remove_unpacked_branchpoints(T,T2).


% iterates through grafts, making branch copies as necessary
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
	retract2(unpacked_edge(Parent,Child)),      % retract edge
	assert2(unzipped_graft(NewVias,Child)),     % assert vias unzipped
	iterate_grafts(T).                          % move on to next graft



% unzip_graft: copies path from graft to branch point


% if not a branch point and vias already unzipped
unzip_graft(_,Vias,Prev,_):-
	unzipped_graft(Vias,Child),          % check if vias unzipped
	unpacked_edge(Parent,Child),	     % find end of path
	assert2(unpacked_edge(Parent,Prev)),!. % attach path to previous node

	
% base case: unpack parent of highest branch point
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
	node(Node,'branch point',BranchPointName),		        % node is a branch point
	edge(Parent,Node),                                      % find parent node
	node(Parent,'branch',BranchName),                       % if parent is correct branch
	member([BranchPointName,BranchName],Vias),
	string_concat(Node,ID,NodeCopy),                        % create copy of node
	delete(Vias,[BranchPointName,BranchName],NewVias),      % remove graft from vias
	assert2(node(NodeCopy,'branch point',BranchPointName)), % assert node copy
	assert2(unpacked_node(NodeCopy)),                       % assert node copy is in unpacked graph
	assert2(unpacked_edge(NodeCopy,Prev)),                  % assert (node copy ->previous copy) is in unpacked edges
	unzip_graft(Parent,NewVias,NodeCopy,ID).                % move on up the tree

% if node is not a branch point
unzip_graft(Node,Vias,Prev,ID):-
	node(Node,Type,Label),		           % get node type and label
	not(Type = 'branch point'),            % if type is not 'branch point'
	%findall(Parent,edge(Parent,Node),Parents),
	edge(Parent,Node),                     % find parent node
	string_concat(Node,ID,NodeCopy),       % create copy of node
	assert2(node(NodeCopy,Type,Label)),    % assert node copy
	assert2(unpacked_node(NodeCopy)),      % assert node copy is in unpacked graph
	assert2(unpacked_edge(NodeCopy,Prev)), % assert (node copy -> previous copy) is in unpacked edges
	unzip_graft(Parent,Vias,NodeCopy,ID).  % move on up the tree
	%unzip_parents(Parents,Vias,NodeCopy,ID).  % move on up the tree

/*
unzip_parents([],_,_,_).
unzip_parents([H|T],Vias,NodeCopy,ID):-
	unzip_graft(H,Vias,NodeCopy,ID),
	unzip_parents(T,Vias,NodeCopy,ID).
*/

% nodes in packed graph
% node(ID,Type,Name).
node(b01,'branch','small').
node(b01_value,'string','Small training corpus').
node(b02,'branch','large').
node(b02_value,'string','Large training corpus').
node(b03,'branch','standard').
node(b03_value,'string','Standard test set').
node(b04,'branch','random').
node(b04_value,'string','Randomized test set').
node(b05,'branch','train').
node(b06,'branch','test').
node(b07,'branch','src').
node(b08,'branch','small').
node(b09,'branch','large').
node(b10,'branch','standard').
node(b11,'branch','random').
node(b12,'branch','train').
node(b13,'branch','test').
node(b14,'branch','tgt').
node(b15,'branch','small').
node(b16,'branch','large').
node(b17,'branch','standard').
node(b18,'branch','random').
node(b19,'branch','train').
node(b20,'branch','test').
node(b21,'branch','one').
node(b22,'branch','yes').
node(b23,'branch','no').
node(b23_no,'file','/dev/null').
node(b24,'branch','bar').
node(b24_value,'string','bar').
node(b25,'branch','bar').
node(b26,'branch','1').
node(b26_value,'string','1').
node(b27,'branch','2').
node(b27_value,'string','2').
node(b28,'branch','3').
node(b28_value,'string','3').
node(p01,'branch point','TrainCorpus').
node(p02,'branch point','TestSplit').
node(p03,'branch point','DataSet').
node(p04,'branch point','Side').
node(p05,'branch point','TrainCorpus').
node(p06,'branch point','TestSplit').
node(p07,'branch point','DataSet').
node(p08,'branch point','TrainCorpus').
node(p09,'branch point','TestSplit').
node(p10,'branch point','DataSet').
node(p11,'branch point','OnlyOne').
node(p12,'branch point','UseDict').
node(p13,'branch point','Foo').
node(p14,'branch point','Foo').
node(p15,'branch point','Seed').
node(t01,'task','extract_dictionary').
node(t01_out,'','out').
node(t01_param,'param','param').
node(t02,'task','preproc').
node(t02_in1,'','in').
node(t02_in1_a,'file','test.tgt').
node(t02_in1_b,'file','rand.tgt').
node(t02_in1_c,'file','sm.tgt').
node(t02_in1_d,'file','lg.tgt').
node(t02_in1_e,'file','test.src').
node(t02_in1_f,'file','rand.src').
node(t02_in1_g,'file','sm.src').
node(t02_in1_h,'file','lg.src').
node(t02_in2,'','dict_in').
node(t02_out,'','out').
node(t03,'task','process_dict').
node(t03_in,'','in').
node(t03_out,'','out').
node(t04,'task','build_model').
node(t04_in1,'','src').
node(t04_in2,'','tgt').
node(t04_in3,'','dict').
node(t04_out,'','out').
node(t05,'task','nothing').
node(t05_in,'param','x').
node(t05_out,'','out').
node(t06,'task','corpus_counts').
node(t06_in1,'','bar').
node(t06_in2,'','src').
node(t06_in3,'','tgt').
node(t06_out,'','out').
node(t07,'task','optimize').
node(t07_in,'','in').
node(t07_out,'','out').
node(t07_p,'param','seed').
node(t08,'task','evaluate_all').
node(t08_in1,'','counts').
node(t08_in2,'','weights').
node(t08_in3,'','model').
node(t08_out,'','out').
node(t09,'task','evaluate_one').
node(t09_in1,'','counts').
node(t09_in2,'','weights').
node(t09_in3,'','model').
node(t09_out,'','out').



% edges in packed graph
% edge(Parent,Child).
edge(b01,p01).
edge(b01_value,b01).
edge(b02,p01).
edge(b02_value,b02).
edge(b03,p02).
edge(b03_value,b03).
edge(b04,p02).
edge(b04_value,b04).
edge(b05,p03).
edge(b06,p03).
edge(b07,p04).
edge(b08,p05).
edge(b09,p05).
edge(b10,p06).
edge(b11,p06).
edge(b12,p07).
edge(b13,p07).
edge(b14,p04).
edge(b15,p08).
edge(b16,p08).
edge(b17,p09).
edge(b18,p09).
edge(b19,p10).
edge(b20,p10).
edge(b21,p11).
edge(b22,p12).
edge(b23,p12).
edge(b23_no,b23).
edge(b24,p13).
edge(b24_value,b24).
edge(b25,p14).
edge(b26,p15).
edge(b26_value,b26).
edge(b27,p15).
edge(b27_value,b27).
edge(b28,p15).
edge(b28_value,b28).
edge(p01,b05).
edge(p02,b06).
edge(p03,t01_param).
edge(p04,t02_in1).
edge(p05,b12).
edge(p06,b13).
edge(p07,b07).
edge(p08,b19).
edge(p09,b20).
edge(p10,b14).
edge(p11,t03_in).
edge(p12,t04_in3).
edge(p13,t05_in).
edge(p14,t06_in1).
edge(p15,t07_p).
edge(t01,t01_out).
edge(t01_out,b21).
edge(t01_out,t02_in2).
edge(t01_param,t01).
edge(t02,t02_out).
edge(t02_in1,t02).
edge(t02_in1_a,b17).
edge(t02_in1_b,b18).
edge(t02_in1_c,b15).
edge(t02_in1_d,b16).
edge(t02_in1_e,b10).
edge(t02_in1_f,b11).
edge(t02_in1_g,b08).
edge(t02_in1_h,b09).
edge(t02_in2,t02).
edge(t02_out,t04_in1).
edge(t02_out,t04_in2).
edge(t02_out,t06_in2).
edge(t02_out,t06_in3).
edge(t03,t03_out).
edge(t03_in,t03).
edge(t03_out,b22).
edge(t04,t04_out).
edge(t04_in1,t04).
edge(t04_in2,t04).
edge(t04_in3,t04).
edge(t04_out,t07_in).
edge(t04_out,t08_in3).
edge(t04_out,t09_in3).
edge(t05,t05_out).
edge(t05_in,t05).
edge(t05_out,b25).
edge(t06,t06_out).
edge(t06_in1,t06).
edge(t06_in2,t06).
edge(t06_in3,t06).
edge(t06_out,t08_in1).
edge(t06_out,t09_in1).
edge(t07,t07_out).
edge(t07_in,t07).
edge(t07_out,t08_in2).
edge(t07_out,t09_in2).
edge(t07_p,t07).
edge(t08,t08_out).
edge(t08_in1,t08).
edge(t08_in2,t08).
edge(t08_in3,t08).
edge(t09,t09_out).
edge(t09_in1,t09).
edge(t09_in2,t09).
edge(t09_in3,t09).

% grafts in packed graph
% graft(Parent,Child,Vias).
%    where Vias is list of [BranchPoint,Branch] tuples
graft(t02_out,t06_in2,[['Side','src']],01).
graft(t02_out,t06_in3,[['Side','tgt']],02).
graft(t02_out,t04_in2,[['DataSet','train'],['Side','tgt']],03).
graft(t02_out,t04_in1,[['DataSet','train'],['Side','src']],04).

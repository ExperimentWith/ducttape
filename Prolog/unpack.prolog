%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% unpack.prolog									   %
% takes a set of nodes, edges, and grafts as input %
% returns the unpacked graphs (in LaTeX)		   %
% Caitlin Cassidy - 06 Nov 2016 				   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic unpacked_node/1.
:- dynamic edge/2.
:- dynamic unpacked_branchpoint/2.
:- dynamic unpacked_edge/2.
:- dynamic node/4.
:- dynamic graft/4.

% Command to run
% first prints packed graph
% then prints each unpacking
go:-
	tell('unpacked_graphs.tex'), % open output file
	print_latex, % print LaTeX formatting
	setof(Node,(Type,Label,Via)^node(Node,Type,Label,Via),Nodes), % set of nodes in packed graph
	setof([Parent,Child],edge(Parent,Child),Edges), % set of edges in packed graph
	print_graph(Nodes,Edges), % print dot2tex packed graph
	% for each unpacking, print in dot2tex
	setof([UnpackedNodes,UnpackedEdges],(unpack(UnpackedNodes,UnpackedEdges),print_graph(UnpackedNodes,UnpackedEdges)),Graphs),
	write('\\end{document}'), % more LaTeX formatting
	told. % close output file

% the big one
% non-deterministacally unpackes input graph
unpack(UnpackedNodes,UnpackedEdges):-
	setof([GraftParent, GraftChild, GraftVias, GraftID],graft(GraftParent,GraftChild,GraftVias,GraftID),Grafts), % set of grafts
	setof(GoalNode,(GoalType,GoalLabel,GoalChild,GoalVia)^(node(GoalNode, GoalType, GoalLabel,GoalVia),not(edge(GoalNode, GoalChild))),Goals), % set of leaves
	iterate_grafts(Grafts), % unzip each graft
	iterate_goals(Goals,[]), % unpack remaining nodes
	setof(UnboundNode,unpacked_node(UnboundNode),UnboundNodes), % set of unpacked nodes
	setof([BranchPointName,BranchName],unpacked_branchpoint(BranchPointName,BranchName),UnpackedVias), % set of global branch point values
	bind_nodes(UnboundNodes,UnpackedVias), % add unpacked branch points to vias list for each node
	setof(Copies,(CopyType,CopyLabel,CopyVias)^setof(CopyNode,node(CopyNode,CopyType,CopyLabel,CopyVias),Copies),Sets), % set of copied nodes; e.g. [[a,a_1],[b],[c],[d,d_1,d_2],[e]]
	delete_copies(Sets), % delete all but original copy
	%setof(Node,(Type,Label,Via)^node(Node,Type,Label,Via),UnpackedNodes), % set of nodes in packed graph
	%setof([Parent,Child],edge(Parent,Child),UnpackedEdges). % set of edges in packed graph
	setof(UnpackedNode, unpacked_node(UnpackedNode), UnpackedNodes), % set of unpacked nodes
	setof([UnpackedParent, UnpackedChild], (unpacked_edge(UnpackedParent, UnpackedChild),unpacked_node(UnpackedParent),unpacked_node(UnpackedChild)), UnpackedEdges). % set of unpacked edges

% unzip_graft
% copies path from graft child to this branch point
% binds nodes to this branch point
unzip_graft(Node, _, _, _) :- % Case 1: Node is a root
	not(edge(_, Node)).
unzip_graft(Node, NodeCopy, Vias, GraftID) :- % Case 2: Node is a branch point in Vias
	node(Node, 'branch point', BranchPointName,_), % node is branch point
	member([BranchPointName,BranchName],Vias), % branch point is in vias
	edge(Parent, Node), % find a parent
	node(Parent, branch, BranchName,_), % find correct parent
	string_concat(Parent,GraftID,ParentCopy), % create parent copy
	assert2(node(ParentCopy,branch,BranchName,Vias)), % assert parent copy with graft vias
	assert2(edge(ParentCopy,NodeCopy)), % assert copied edge
	unzip_graft(Parent, ParentCopy, Vias, GraftID). % unzip graft
unzip_graft(Node, NodeCopy, Vias, GraftID) :- % Case 3: Node is a branch point, but not in vias
	node(Node, 'branch point', BranchPointName,_), % node is branch point
	not(member([BranchPointName,_],Vias)), % branch point is not in vias (not bound)
	setof(Parent, edge(Parent, Node), Parents), % get set of parents
	unzip_parents(Parents, NodeCopy, Vias, GraftID). % unzip each parent
unzip_graft(Node, NodeCopy, Vias, GraftID) :- % Case 4: Node is not a branch point
	node(Node, Type, _,_), % get type and label
	not(Type == 'branch point'), % node is not a branch point
	setof(Parent, edge(Parent, Node), Parents), % get set of parents
	unzip_parents(Parents, NodeCopy, Vias, GraftID). % unzip each parent

% unpack_node
% adds nodes and edges to unpacked graph
unpack_node(Node,_) :- % Case 1: Node is already unpacked
	unpacked_node(Node). % do nothing
unpack_node(Node,Via) :- % Case 2: Node is a root
	not(unpacked_node(Node)), % check not unpacked
	not(edge(_, Node)), % Node has no parent
	node(Node,Type,Label,OldVia), % get type, label, and old via
	union2(Via,OldVia,NewVia), % add Via to OldVia
	retract2(node(Node,Type,Label,OldVia)), % retract node with old via
	assert2(node(Node,Type,Label,NewVia)), % assert node with new via
	assert2(unpacked_node(Node)). % assert node unpacked
unpack_node(Node,Via) :- % Case 3: Node is not a branch point
	not(unpacked_node(Node)), % check not unpacked
	node(Node,Type,Label,OldVia), % get type, label, and old via
	not(Type == 'branch point'), % check not branch point
	union2(Via,OldVia,NewVia), % add Via to OldVia
	retract2(node(Node,Type,Label,OldVia)), % retract node with old via
	assert2(node(Node,Type,Label,NewVia)), % assert node with new via
	assert2(unpacked_node(Node)), % assert node unpacked
	setof([Parent, Node], edge(Parent, Node), Parents), % set of parent nodes
	unpack_parents(Parents,NewVia). % unpack each parent
unpack_node(Node,Via) :- % Case 4: Node is branch point that has already been unpacked
	not(unpacked_node(Node)), % check not unpacked
	node(Node, 'branch point', BranchPointName,OldVia), % get old via
	union2(Via,OldVia,NewVia), % add Via to OldVia
	unpacked_branchpoint(BranchPointName, BranchName), % find branch name
	edge(Parent, Node), % find a parent
	node(Parent, branch, BranchName,_), % find correct parent
	retract2(node(Node, 'branch point',BranchPointName,OldVia)), % retract node with old via
	assert2(node(Node, 'branch point',BranchPointName,NewVia)), % assert node with new via
	assert2(unpacked_node(Node)), % assert node unpacked
	assert2(unpacked_edge(Parent, Node)), % assert edge is unpacked
	unpack_node(Parent,NewVia). % unpack parent
unpack_node(Node,Via) :- %Case 5: Node is branch point that has not been unpacked
	not(unpacked_node(Node)), % check not unpacked
	node(Node, 'branch point', BranchPointName,_), % check branch point
	not(unpacked_branchpoint(BranchPointName, BranchName)), % check not unpacked branch point
	edge(Parent, Node), % find a parent
	node(Parent, branch, BranchName,_), % get branch name
	assert2(unpacked_branchpoint(BranchPointName, BranchName)), % assert branch point unpacked
	assert2(unpacked_node(Node)), % assert node unpacked
	assert2(unpacked_edge(Parent, Node)), % assert edge unpacked
	unpack_node(Parent,Via). %unpack parent
unpack_node(Node,Via):- % Case 6: Node is a branch point that is bound
	not(unpacked_node(Node)), % check not unpacked
	node(Node, 'branch point', BranchPointName,BoundVias), % get binding
	edge(Parent, Node), % find the parent
	node(Parent, branch, BranchName,_), % get parent branch name
	member([BranchPointName,BranchName],BoundVias), % check correct binding (maybe unnecessary)
	assert2(unpacked_node(Node)), % assert node unpacked
	assert2(unpacked_edge(Parent, Node)), % assert edge unpacked
	unpack_node(Parent,Via). %unpack parent

% unzip_parents
% iterates through graft parents
unzip_parents([], _, _, _).
unzip_parents([Parent|Tail], NodeCopy, Vias, GraftID) :-
	node(Parent,ParentType,ParentLabel,_), % get parent type and label
	string_concat(Parent,GraftID,ParentCopy), % create parent copy
	assert2(node(ParentCopy,ParentType,ParentLabel,Vias)), % assert parent copy with graft vias
	assert2(edge(ParentCopy,NodeCopy)), % assert copied edge
	unzip_graft(Parent, ParentCopy, Vias, GraftID), % unzip graft from this parent
	unzip_parents(Tail, NodeCopy, Vias, GraftID). % move on to the next parent

% unpack_parents
% iterates through node parents
unpack_parents([],_).
unpack_parents([[Parent, Child]|Tail],Via) :-
	assert2(unpacked_edge(Parent, Child)), % assert edge unpacked
	unpack_node(Parent,Via),
	unpack_parents(Tail,Via).

% iterate_grafts
% unzips each graft
iterate_grafts([]).
iterate_grafts([[Parent, Child, Vias, GraftID]|Tail]) :-
	node(Parent,ParentType,ParentLabel,_), % get parent type and label
	string_concat(Parent,GraftID,ParentCopy), % create parent copy
	assert2(node(ParentCopy,ParentType,ParentLabel,Vias)), % assert parent copy with graft vias
	retract2(edge(Parent, Child)), % retract original edge
	assert2(edge(ParentCopy,Child)), % assert copied edge
	retract2(node(Child,ChildType,ChildLabel,_)), % retract child with old vias
	assert2(node(Child,ChildType,ChildLabel,Vias)), % assert child with graft vias
	retract2(graft(Parent,Child,Vias,GraftID)), % retract original graft
	assert2(graft(ParentCopy,Child,Vias,GraftID)), % assert copied graft
	unzip_graft(Parent, ParentCopy, Vias, GraftID), % unzip graft
	iterate_grafts(Tail). % keep going

% iterate_goals
% unzpacks each leaf node
iterate_goals([],_).
iterate_goals([Node|Tail],Via) :-
	unpack_node(Node,Via),
	iterate_goals(Tail,Via).

% bind_nodes
% adds branch annotations to each node
bind_nodes([],_).
bind_nodes([Node|Tail],Vias):-
	node(Node,Type,Label,OldVias), % get type, label, and original vias
	union2(Vias,OldVias,NewVias), % add global branch points to vias
	retract2(node(Node,Type,Label,OldVias)), % retract node with old vias
	assert2(node(Node,Type,Label,NewVias)), % assert node with new vias
	bind_nodes(Tail,Vias). % keep going

% delete_copies
% for each set of copied nodes
%    deletes all but the first
delete_copies([]).
delete_copies([[_|Copies]|T]):-
	delete_aux(Copies),
	delete_copies(T).

% delete_aux
% retracts each node in a list
delete_aux([]).
delete_aux([Node|Tail]):-
	retract2(unpacked_node(Node)),
	delete_aux(Tail).

% node(NodeID,Type,Label,EmptyVias).
node(foo,task,foo,[]).
node(x,input,x,[]).
node(bp,'branch point',bp,[]).
node(a,branch,a,[]).
node(higher_bp,'branch point','higher BP',[]).
node(g,branch,g,[]).
node(l,input,l,[]).
node(h,branch,h,[]).
node(m, input,m,[]).
node(b,branch,b,[]).
node(n,input,n,[]).
node(o,input,o,[]).
node(out,output,out,[]).
node(bar,task,bar,[]).
node(in1,input,in,[]).
node(in2,input,in,[]).
node(in,input,in,[]).
node(y,output,y,[]).
node(baz,task,baz,[]).
node(w,input,w,[]).
node(z,input,z,[]).
node(diffBP,'branch point','different BP',[]).
node(c,branch,c,[]).
node(d,branch,d,[]).


% edge(Parent, Child).
edge(x,foo).
edge(a,bp).
edge(b,bp).
edge(higher_bp,a).
edge(higher_bp,b).
edge(g,higher_bp).
edge(h,higher_bp).
edge(bp,x).
edge(l,g).
edge(m,h).
edge(n,g).
edge(o,h).
edge(foo,out).
edge(out,in).
edge(out,in2).
edge(out,in1).
edge(in1,bar).
edge(in2,bar).
edge(in,bar).
edge(bar,y).
edge(w,baz).
edge(y,w).
edge(z,baz).
edge(diffBP,z).
edge(c,diffBP).
edge(d,diffBP).

% graft(Parent,Child,Vias,GraftID).
graft(out, in1, [[bp, a]], '_1').
graft(out, in2, [[bp, b],['higher BP',h]], '_2').

% print LaTeX-ready graph
print_graph(UnpackedNodes,UnpackedEdges) :-
	write('\\begin{center}\n\\begin{tikzpicture}[>=latex, scale=2.0, transform shape]\n\n\t\\begin{dot2tex}[dot,scale=2.0,tikzedgelabels,codeonly]\n\tdigraph G {\n\n\t\t\tgraph [nodesep="0.5", ranksep="0"];\n\n'),
	print_nodes(UnpackedNodes),
	write('\n'),
	print_edges(UnpackedEdges),
	write('\n\t}\n\\end{dot2tex}\n\\end{tikzpicture}\n\\end{center}\n\n').

% print formatted nodes
print_nodes([]).
print_nodes([Node|Tail]) :-
	node(Node,Type,Label,Vias),
	write('\t\t'),
	write(Node),
	write(' [style="'),
	write(Type),
	write('", label="'), write(Label),
	%write(': '), write(Vias),
	write('"];\n'),
	print_nodes(Tail).

% print formatted edges
print_edges([]).
print_edges([[Parent,Child]|Tail]):-
	graft(Parent,Child,Vias,_),
	write('\t\t'),
	write(Parent),
	write(' -> '),
	write(Child),
	graft_string(Vias,'',GraftString),
	write(' [label=\"['),write(GraftString),write(']\", lblstyle="graft"];\n'),
	print_edges(Tail),!.
print_edges([[Parent,Child]|Tail]) :-
	write('\t\t'),
	write(Parent),
	write(' -> '),
	write(Child),
	write(';\n'),
	print_edges(Tail).
	
graft_string([[BranchPoint,Branch]],SoFar,Return):-
	string_concat(SoFar,BranchPoint,A),
	string_concat(A,':',B),
	string_concat(B,Branch,Return),!.
graft_string([[BranchPoint,Branch]|Tail],SoFar,Return):-
	not(Tail == []),
	string_concat(SoFar,BranchPoint,A),
	string_concat(A,':',B),
	string_concat(B,Branch,C),
	string_concat(C,',',D),
	graft_string(Tail,D,Return).

% assert2 and retract2
% alternatives that are not immune to backtracking
assert2(X) :-
	assert(X).
assert2(X) :-
	retract(X),
	fail.

retract2(X) :-
	call(X),
	reallyRetract2(X).

reallyRetract2(X) :-
	retract(X).
reallyRetract2(X) :-
	assert(X),
	fail.

% union2
% adds new vias to old vias
% if a node is already bound to a branch point
%    do not overwrite
union2([],L,L).
union2([[H1,_]|T],L,R2):-
	member([H1,_],L),!,
	union2(T,L,R),
	sort(R,R2).
union2([H|T],L,[H|R2]):-
	union2(T,L,R),
	sort(R,R2).

% LaTeX formatting
print_latex:-
	write('\\documentclass[a0,14pt]{sciposter}'),nl,
	write('%\\usepackage{acl2015}'),nl,
	write('%\\usepackage{times}'),nl,
	write('%\\usepackage{listings}'),nl,
	write('\\usepackage{fancyvrb}'),nl,
	write('\\usepackage{latexsym}'),nl,
	write('\\usepackage[margin=1in]{geometry}'),nl,
	write('\\usepackage[forceshell,outputdir={auto_generated/}]{dot2texi}'),nl,
	write('\\usepackage{tikz}'),nl,
	write('\\usetikzlibrary{shapes,arrows,shadows,shadows.blur,positioning,fit}'),nl,nl,
	write('% Note: When compiling this document using TeXShop on Mac OS X, '),nl,
	write('%       if dot2tex is installed using fink, the following workaround can be used '),nl,
	write('%       to ensure that TeXShop can find dot2tex'),nl,
	write('%'),nl,
	write('%       sudo ln -s /sw/bin/dot2tex /usr/texbin/dot2tex'),nl,nl,nl,
	write('\\title{A workflow management acid test}'),nl,nl,
	write('\\author{Lane Schwartz \\textnormal{and} Jonathan Clark}'),nl,nl,
	write('\\institute{University of Illinois at Urbana-Champaign}'),nl,nl,
	write('\\date{}'),nl,nl,
	write('\\definecolor{darkpastelgreen}{rgb}{0.01, 0.75, 0.24}'),nl,nl,
	write('\\pgfdeclarelayer{background}'),nl,
	write('\\pgfdeclarelayer{foreground}'),nl,
	write('\\pgfsetlayers{background,main,foreground}'),nl,nl,
	write('\\tikzstyle{branch} = [ellipse, draw=none, inner sep=0.3mm, fill=blue, drop shadow, text centered, anchor=north, text=white]'),nl,
	write('\\tikzstyle{task}   = [rectangle, draw=none, rounded corners=2mm, fill=orange, drop shadow, text centered, anchor=north, text=white, inner sep=1mm]'),nl,
	write('\\tikzstyle{branch point} = [rectangle, draw=none, fill=red, drop shadow, text centered, anchor=north, text=white]'),nl,nl,
	write('%\\tikzstyle{graft}  = [sloped,pos=0.1,fill=blue!20]'),nl,
	write('\\tikzstyle{graft}  = [fill=blue!20]'),nl,nl,
	write('\\tikzstyle{param}   = [rectangle]'),nl,
	write('\\tikzstyle{input}   = [ellipse]'),nl,
	write('\\tikzstyle{output}   = [ellipse]'),nl,nl,nl,
	write('\\tikzstyle{file} = [ellipse, draw, inner sep=0.3mm, fill=darkpastelgreen, text centered, anchor=north, text=white]'),nl,
	write('\\tikzstyle{string} = [rectangle, draw, inner sep=0.3mm, fill=darkpastelgreen, text centered, anchor=north, text=white]'),nl,nl,nl,
	write('\\begin{document}'),nl,
	write('\\maketitle'),nl.

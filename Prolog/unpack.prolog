#!/usr/bin/prolog

% unpack.prolog
% takes a set of nodes, edges, and grafts as input
% returns the unpacked graphs (in LaTeX)
% Caitlin Cassidy - 18 Nov 2016

:- initialization main.

:- dynamic unpacked_node/5.
:- dynamic edge/2.
:- dynamic unpacked_branchpoint/2.
:- dynamic unpacked_edge/2.
:- dynamic node/5.
:- dynamic graft/4.
:- dynamic unique_node/6.
:- dynamic current_id/1.

main:-
        profile(go),
        halt(0).

% Command to run
% first prints packed graph
% then prints each unpacking
go:-
	current_prolog_flag(argv,Argv),
        nth0(0,Argv,Argument0), % get input file
        nth0(1,Argv,Argument1), % get output file
        consult(Argument0), % read packed graph from input file
        tell(Argument1), % open output file
	print_latex, % print LaTeX formatting
	setof([Node,Original,Type,Label,Via],node(Node,Original,Type,Label,Via),Nodes), % set of nodes in packed graph
	setof([[Parent,ParentOriginal,ParentType,ParentLabel,ParentVias],[Child,ChildOriginal,ChildType,ChildLabel,ChildVias]], (edge(Parent, Child),node(Parent,ParentOriginal,ParentType,ParentLabel,ParentVias),node(Child,ChildOriginal,ChildType,ChildLabel,ChildVias)), Edges), % set of unpacked edges
	print_graph(Nodes,Edges,true),!, % print dot2tex packed graph; true = print graft labels; ! = no need to backtrack beyond this point (for speed)
	%unpack(UnpackedNodes,UnpackedEdges),
	%print_graph(UnpackedNodes,UnpackedEdges,false),
	setof([UnpackedNodes,UnpackedEdges],unpack(UnpackedNodes,UnpackedEdges),Graphs), % set of possible combos
	print_graphs(Graphs,false), % print each unpacking; false = do not print graft labels
	write('\\end{document}'), % more LaTeX formatting
	told. % close output file
	

% the big one
% produces one possible unpacking of input graph
unpack(UnpackedNodes,UnpackedEdges):-
	setof([GraftParent, GraftChild, GraftVias, GraftID],graft(GraftParent,GraftChild,GraftVias,GraftID),Grafts), % set of grafts
	setof(GoalNode,(GoalType,GoalOriginal,GoalLabel,GoalChild,GoalVia)^(node(GoalNode, GoalOriginal,GoalType, GoalLabel,GoalVia),not(edge(GoalNode,GoalChild))),Goals), % set of leaves
	iterate_grafts(Grafts),!, % copy path from each graft to roots; ! = no need to backtrack beyond this point (for speed)
	iterate_goals(Goals,[]), % unpack from each leaf
	setof([UnboundNode,UnboundOriginal,UnboundType,UnboundLabel,UnboundVias],unpacked_node(UnboundNode,UnboundOriginal,UnboundType,UnboundLabel,UnboundVias),UnboundNodes), % set of unpacked nodes
        setof([BranchPointName,BranchName],unpacked_branchpoint(BranchPointName,BranchName),UnpackedVias), % set of global branch point values
        bind_nodes(UnboundNodes,UnpackedVias), % add unpacked branch points to vias list for each node
        setof(Copies,(CopyOriginal,CopyType,CopyLabel,CopyVias)^setof(CopyNode,unpacked_node(CopyNode,CopyOriginal,CopyType,CopyLabel,CopyVias),Copies),Sets), % set of duplicate nodes; e.g. [[a,a_1],[b],[c],[d,d_1,d_2],[e]]
        delete_copies(Sets), % delete all but original copy
	%setof([UnpackedNode,NodeOriginal,NodeType,NodeLabel,NodeVias], node(UnpackedNode,NodeOriginal,NodeType,NodeLabel,NodeVias), UnpackedNodes), % set of unpacked nodes
        %setof([[UnpackedParent,ParentOriginal,ParentType,ParentLabel,ParentVias], [UnpackedChild,ChildOriginal,ChildType,ChildLabel,ChildVias]], (edge(UnpackedParent, UnpackedChild),node(UnpackedParent,ParentOriginal,ParentType,ParentLabel,ParentVias),node(UnpackedChild,ChildOriginal,ChildType,ChildLabel,ChildVias)), UnpackedEdges). % set of unpacked edges
	setof([UnpackedNode,NodeOriginal,NodeType,NodeLabel,NodeVias], unpacked_node(UnpackedNode,NodeOriginal,NodeType,NodeLabel,NodeVias), UnpackedNodes), % set of unpacked nodes
	setof([[UnpackedParent,ParentOriginal,ParentType,ParentLabel,ParentVias], [UnpackedChild,ChildOriginal,ChildType,ChildLabel,ChildVias]], (unpacked_edge(UnpackedParent, UnpackedChild),unpacked_node(UnpackedParent,ParentOriginal,ParentType,ParentLabel,ParentVias),unpacked_node(UnpackedChild,ChildOriginal,ChildType,ChildLabel,ChildVias)), UnpackedEdges). % set of unpacked edges

% unzip_graft
% copies path from graft child to root
% binds nodes to specified branch values
unzip_graft(Node, _, _, _) :- % Case 1: Node is a root
	not(edge(_, Node)).
unzip_graft(Node, NodeCopy, Vias, GraftID) :- % Case 2: Node is a branch point in Vias
	node(Node, _,'branch point', BranchPointName,_), % node is branch point
	member([BranchPointName,BranchName],Vias), % branch point is in vias
	edge(Parent, Node), % find a parent
	node(Parent, ParentOriginal,branch, BranchName,_), % find correct parent
	string_concat(Parent,GraftID,ParentCopy), % create parent copy
	assert(node(ParentCopy,ParentOriginal,branch,BranchName,Vias)), % assert parent copy with graft vias
	assert(edge(ParentCopy,NodeCopy)), % assert copied edge
	unzip_graft(Parent, ParentCopy, Vias, GraftID). % keep going
unzip_graft(Node, NodeCopy, Vias, GraftID) :- % Case 3: Node is a branch point, but not in vias
	node(Node, _, 'branch point', BranchPointName,_), % node is branch point
	not(member([BranchPointName,_],Vias)), % branch point is not in vias
	setof(Parent, edge(Parent, Node), Parents), % get set of parents
	unzip_parents(Parents, NodeCopy, Vias, GraftID). % unzip each parent
unzip_graft(Node, NodeCopy, Vias, GraftID) :- % Case 4: Node is not a branch point
	node(Node, _, Type, _,_), % get type and label
	not(Type == 'branch point'), % node is not a branch point
	setof(Parent, edge(Parent, Node), Parents), % get set of parents
	unzip_parents(Parents, NodeCopy, Vias, GraftID). % unzip each parent

unpack_node(Node,_) :- % Case 1: Node is already unpacked
        unpacked_node(Node,_,_,_,_).
unpack_node(Node,Via) :- % Case 2: Node is a root
        node(Node,Original,Type,Label,OldVia), % get type, label, and old via
	not(unpacked_node(Node,Original,Type,Label,_)), % check not unpacked
        not(edge(_, Node)), % Node has no parent
        union2(Via,OldVia,NewVia), % add Via to OldVia
        assert2(unpacked_node(Node,Original,Type,Label,NewVia)). % assert node with new via
unpack_node(Node,Via) :- % Case 3: Node is not a branch point
        node(Node,Original,Type,Label,OldVia), % get type, label, and old via
        not(unpacked_node(Node,Original,Type,Label,_)), % check not unpacked
	not(Type == 'branch point'), % check not branch point
        union2(Via,OldVia,NewVia), % add Via to OldVia
        assert2(unpacked_node(Node,Original,Type,Label,NewVia)), % assert node with new via
        setof([Parent, Node], edge(Parent, Node), Parents), % set of parent nodes
        unpack_parents(Parents,NewVia). % unpack each parent
unpack_node(Node,Via) :- % Case 4: Node is branch point that has already been unpacked
        node(Node, Original,'branch point', BranchPointName,OldVia), % get old via
	not(unpacked_node(Node,Original,'branch point',BranchPointName,_)), % check not unpacked
        union2(Via,OldVia,NewVia), % add Via to OldVia
        unpacked_branchpoint(BranchPointName, BranchName), % find branch name
        edge(Parent, Node), % find a parent
        node(Parent, _, branch, BranchName,_), % find correct parent
        assert2(unpacked_node(Node, Original, 'branch point',BranchPointName,NewVia)), % assert node with new via
        assert2(unpacked_edge(Parent, Node)), % assert edge is unpacked
        unpack_node(Parent,NewVia). % unpack parent
unpack_node(Node,Via) :- %Case 5: Node is branch point that has not been unpacked
        node(Node,Original , 'branch point', BranchPointName,_), % check branch point
	not(unpacked_node(Node,Original,'branch point',BranchPointName,_)), % check not unpacked
        not(unpacked_branchpoint(BranchPointName, BranchName)), % check not unpacked branch point
        edge(Parent, Node), % find a parent
        node(Parent, _, branch, BranchName,_), % get branch name
        assert2(unpacked_branchpoint(BranchPointName, BranchName)), % assert branch point unpacked
        assert2(unpacked_node(Node,Original,'branch point',BranchPointName,Via)), % assert node unpacked
        assert2(unpacked_edge(Parent, Node)), % assert edge unpacked
        unpack_node(Parent,Via). %unpack parent
unpack_node(Node,Via):- % Case 6: Node is a branch point that is bound
        node(Node,Original,'branch point', BranchPointName,BoundVias), % get binding
	not(unpacked_node(Node,Original,'branch point',BranchPointName,_)), % check not unpacked
        edge(Parent, Node), % find the parent
        node(Parent, _,branch, BranchName,_), % get parent branch name
        member([BranchPointName,BranchName],BoundVias), % check correct binding (maybe unnecessary)
        assert2(unpacked_node(Node,Original,'branch point',BranchPointName,BoundVias)), % assert node unpacked
        assert2(unpacked_edge(Parent, Node)), % assert edge unpacked
        unpack_node(Parent,Via).

% unzip_parents
% iterates through graft parents
unzip_parents([], _, _, _).
unzip_parents([Parent|Tail], NodeCopy, Vias, GraftID) :-
	node(Parent,ParentOriginal,ParentType,ParentLabel,_), % get parent type and label
	string_concat(Parent,GraftID,ParentCopy), % create parent copy
	assert(node(ParentCopy,ParentOriginal,ParentType,ParentLabel,Vias)), % assert parent copy with graft vias
	assert(edge(ParentCopy,NodeCopy)), % assert copied edge
	unzip_graft(Parent, ParentCopy, Vias, GraftID), % unzip graft from this parent
	unzip_parents(Tail, NodeCopy, Vias, GraftID). % move on to the next parent

% unpack_parents
% iterates through node parents
unpack_parents([],_).
unpack_parents([[Parent, Child]|Tail],Via) :-
	assert2(unpacked_edge(Parent, Child)), % assert edge unpacked
	unpack_node(Parent,Via), % unpack from this parent
	unpack_parents(Tail,Via). % move on to the next parent

% iterate_grafts
% unzips each graft
iterate_grafts([]).
iterate_grafts([[Parent, Child, Vias, GraftID]|Tail]) :-
	node(Parent,ParentOriginal,ParentType,ParentLabel,_), % get parent type and label
	string_concat(Parent,GraftID,ParentCopy), % create parent copy
	assert(node(ParentCopy,ParentOriginal,ParentType,ParentLabel,Vias)), % assert parent copy with graft vias
	retract(edge(Parent, Child)), % retract original edge
	assert(edge(ParentCopy,Child)), % assert copied edge
	retract(node(Child,ChildOriginal,ChildType,ChildLabel,_)), % retract child with old vias
	assert(node(Child,ChildOriginal,ChildType,ChildLabel,Vias)), % assert child with graft vias
	unzip_graft(Parent, ParentCopy, Vias, GraftID), % unzip graft
	iterate_grafts(Tail). % keep going

% iterate_goals
% unzpacks each leaf node
iterate_goals([],_).
iterate_goals([Node|Tail],Via) :-
	unpack_node(Node,Via),
	iterate_goals(Tail,Via).

% bind_nodes
% fills in branch values at each node
bind_nodes([],_).
bind_nodes([[Node,Original,Type,Label,OldVias]|Tail],Vias):-
        union2(Vias,OldVias,NewVias), % add global branch points to vias
        retract2(unpacked_node(Node,Original,Type,Label,OldVias)), % retract node with old vias
        assert2(unpacked_node(Node,Original,Type,Label,NewVias)), % assert node with new vias
        bind_nodes(Tail,Vias). % keep going

% delete_copies
% for each set of copied nodes
%    deletes all but the first
delete_copies([]).
delete_copies([[Original|Copies]|T]):-
        delete_aux(Original,Copies),
        delete_copies(T).

% delete_aux
% retracts each node in a list
%     and attaches its children to the original
delete_aux(_,[]).
delete_aux(Original,[Node|Tail]):-
        retract2(unpacked_node(Node,_,_,_,_)),
        setof(Child,unpacked_edge(Node,Child),Children),
        copy_children(Original,Children),
        delete_aux(Original,Tail).

% attach copy's children to original node
copy_children(_,[]).
copy_children(Original,[Child|Tail]):-
        assert2(unpacked_edge(Original,Child)),
        copy_children(Original,Tail).

print_graphs([],_).
print_graphs([[Nodes,Edges]|Tail],PrintGrafts):-
	print_graph(Nodes,Edges,PrintGrafts),
	print_graphs(Tail,PrintGrafts).

% print LaTeX-ready graph
print_graph(UnpackedNodes,UnpackedEdges,PrintGrafts) :-
	write('\\begin{center}\n\\begin{tikzpicture}[>=latex, scale=1.0, transform shape]\n\n\t\\begin{dot2tex}[dot,scale=1.0,tikzedgelabels,codeonly]\n\tdigraph G {\n\n\t\t\tgraph [nodesep="0.5", ranksep="0"];\n\n'),
	print_nodes(UnpackedNodes),
	write('\n'),
	print_edges(UnpackedEdges,PrintGrafts),
	write('\n\t}\n\\end{dot2tex}\n\\end{tikzpicture}\n\\end{center}\n\n').

% print formatted nodes
print_nodes([]).
print_nodes([[Node,_,Type,Label,Vias]|Tail]) :-
	write('\t\t'), write(Node),
	write(' [style="'),	write(Type),
	write('", label="'), write(Label),
	%write(': '), write(Vias),
	write('"];\n'),
	print_nodes(Tail).

% print formatted edges
print_edges([],_).
print_edges([[[Parent,_,_,_,_],[Child,_,_,_,_]]|Tail],true):-
	graft(Parent,Child,Vias,_),
	write('\t\t'),
	write(Parent),
	write(' -> '),
	write(Child),
	graft_string(Vias,'',GraftString),
	write(' [label=\"['),write(GraftString),write(']\", lblstyle="graft"];\n'),
	print_edges(Tail,true),!.
print_edges([[[Parent,_,_,_,_],[Child,_,_,_,_]]|Tail],PrintGrafts) :-
	write('\t\t'),
	write(Parent),
	write(' -> '),
	write(Child),
	write(';\n'),
	print_edges(Tail,PrintGrafts).

% make graft labels pretty(ish)
graft_string([[BranchPoint,Branch]],SoFar,Return):-
	string_concat(SoFar,BranchPoint,A),
	string_concat(A,':',B),
	string_concat(B,Branch,Return),!.
graft_string([[BranchPoint,Branch]|Tail],SoFar,Return):-
	not(Tail == []),
	string_concat(SoFar,BranchPoint,A),
	string_concat(A,':',B),
	string_concat(B,Branch,C),
	string_concat(C,' , ',D),
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

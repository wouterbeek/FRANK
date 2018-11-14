:- use_module(library(closure)).
:- use_module(library(frank)).

run(C, D, Path) :-
  term_prefix('http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#', C),
  path_closure(triple_, C, D, Path).

triple_(C, D) :-
  triple(D, rdfs:subClassOf, C).

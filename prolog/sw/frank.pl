:- module(
  frank,
  [
    frank_id/1,      % -Id
    frank_sameas/2,  % ?Term1, ?Term2
    frank_term/1,    % ?Term
    frank_term_id/2, % ?Term, ?Id
    frank_triple/3   % ?S, ?P, ?O
  ]
).

/** <module> FRANK

Federated Resource Architecture for Networked Knowledge

@author Wouter Beek
@version 2018
*/

:- use_module(library(aggregate)).
:- use_module(library(error)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_ntriples)).

:- use_module(library(http/http_client2)).
:- use_module(library(media_type)).
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_term)).
:- use_module(library(uri_ext)).

:- initialization
   rdf_assert_prefixes,
   curl.

:- rdf_meta
   frank_sameas(o, o),
   frank_term(o),
   frank_term_id(o, ?),
   frank_triple(r, r, o).





%! frank_id(-Id:atom) is nondet.

frank_id(Id) :-
  sameas_uri([id], [], Uri),
  frank_request(Uri, Id).



%! frank_sameas(+Term1:rdf_term, +Term2:rdf_term) is semidet.
%! frank_sameas(+Term1:rdf_term, -Term2:rdf_term) is nondet.
%! frank_sameas(-Term1:rdf_term, +Term2:rdf_term) is nondet.
%! frank_sameas(-Term1:rdf_term, -Term2:rdf_term) is nondet.

frank_sameas(Term1, Term2) :-
  ground(Term1), !,
  frank_term_id(Term1, Id),
  frank_term_id(Term2, Id).
frank_sameas(Term1, Term2) :-
  ground(Term2), !,
  frank_sameas(Term2, Term1).
frank_sameas(Term1, Term2) :-
  frank_term(Term1),
  frank_sameas(Term1, Term2).



%! frank_term(+Term:rdf_term) is semidet.
%! frank_term(-Term:rdf_term) is nondet.

frank_term(Term) :-
  lodalot_uri([term], [], Uri),
  frank_request(Uri, Term).



%! frank_term_id(+Term:rdf_term, -Id:atom) is semidet.
%! frank_term_id(-Term:rdf_term, +Id:atom) is nondet.

frank_term_id(Term, Id) :-
  ground(Term), !,
  rdf_term_to_atom(Term, TermAtom),
  sameas_uri([id], [term(TermAtom)], Uri),
  frank_request(Uri, Id).
frank_term_id(Term, Id) :-
  ground(Id), !,
  sameas_uri([term], [id(Id)], Uri),
  frank_request(Uri, Term).
frank_term_id(Term, Id) :-
  instantiation_error(args(Term,Id)).



%! frank_triple(?S:rdf_nonliteral, ?P:iri, ?O:rdf_term) is nondet.

frank_triple(S, P, O) :-
  convlist(rdf_query_term, [subject(S),predicate(P),object(O)], Query),
  lodalot_uri([triple], Query, Uri),
  http_open2(Uri, In, [accept(nt)]),
  call_cleanup(
    (
      rdf_read_ntriples(In, Triples, []),
      member(rdf(S,P,O), Triples)
    ),
    close(In)
  ).

% semi-deterministic
rdf_query_term(Compound1, Compound2) :-
  ground(Compound1),
  Compound1 =.. [Key,Term],
  rdf_term_to_atom(Term, Atom),
  Compound2 =.. [Key,Atom].





% GENERICS %

%! frank_request(+Uri:atom, -X:term) is nondet.

frank_request(Uri, X) :-
  catch(http_open2(Uri, In, [accept(json)]), E, true),
  (   var(E)
  ->  call_cleanup(
        json_read_dict(In, L, [value_string_as(atom)]),
        close(In)
      ),
      (is_list(L) -> member(X, L) ; X = L)
  ;   pp_error(E),
      fail
  ).

pp_error(error(http_status(_,Msg),_)) :-
  ansi_format([fg(red)], "~s\n", [Msg]).



%! lodalot_uri(+Segments:list(atom), +Query:list(compound), -Uri:atom) is det.

lodalot_uri(Segments, Query, Uri) :-
  uri_comps(Uri, uri(https,'hdt.lod.labs.vu.nl',Segments,Query,_)).



%! sameas_uri(+Segments:list(atom), +Query:list(compound), -Uri:atom) is det.

sameas_uri(Segments, Query, Uri) :-
  uri_comps(Uri, uri(https,'sameas.cc',Segments,Query,_)).

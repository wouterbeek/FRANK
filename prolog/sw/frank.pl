:- module(
  frank,
  [
    count/4,   % ?S, ?P, ?O, -N
    id/1,      % -Id
    sameas/2,  % ?Term1, ?Term2
    term/1,    % ?Term
    term/2,    % +TermRole, ?Term
    term_id/2, % ?Term, ?Id
    triple/3   % ?S, ?P, ?O
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
   count(r, r, o, -),
   sameas(o, o),
   subject(r),
   term(o),
   term(+, o),
   term_id(o, ?),
   triple(r, r, o).





%! count(?S:rdf_nonliteral, ?P:iri, ?O:rdf_term, -N:nonneg) is det.

count(S, P, O, N) :-
  triple_uri_([count], S, P, O, Uri),
  json_request_(Uri, N).



%! id(-Id:atom) is nondet.

id(Id) :-
  sameas_uri_([id], [], Uri),
  json_request_(Uri, Id).



%! sameas(+Term1:rdf_term, +Term2:rdf_term) is semidet.
%! sameas(+Term1:rdf_term, -Term2:rdf_term) is nondet.
%! sameas(-Term1:rdf_term, +Term2:rdf_term) is nondet.
%! sameas(-Term1:rdf_term, -Term2:rdf_term) is nondet.

sameas(Term1, Term2) :-
  ground(Term1), !,
  (term_id(Term1, Id) -> term_id(Term2, Id) ; Term2 = Term1).
sameas(Term1, Term2) :-
  ground(Term2), !,
  sameas(Term2, Term1).
sameas(Term1, Term2) :-
  term(Term1),
  sameas(Term1, Term2).



%! term(+Term:rdf_term) is semidet.
%! term(-Term:rdf_term) is nondet.

term(Term) :-
  lodalot_uri_([term], [], Uri),
  json_request_(Uri, Term).


%! subject(+TermRole:atom, +Term:rdf_term) is semidet.
%! subject(+TermRole:atom, -Term:rdf_term) is nondet.
%
% TermRole:
%   - node
%   - object
%   - predicate
%   - shared
%   - sink
%   - source
%   - subject
%   - term

term(TermRole, Term) :-
  lodalot_uri_([TermRole], [], Uri),
  json_request_(Uri, Term).



%! term_id(+Term:rdf_term, -Id:atom) is semidet.
%! term_id(-Term:rdf_term, +Id:atom) is nondet.

term_id(Term, Id) :-
  ground(Term), !,
  rdf_term_to_atom(Term, TermAtom),
  sameas_uri_([id], [term(TermAtom)], Uri),
  request(Uri, Id).
term_id(Term, Id) :-
  ground(Id), !,
  sameas_uri_([term], [id(Id)], Uri),
  request(Uri, TermAtom),
  rdf_atom_to_term(TermAtom, Term).
term_id(Term, Id) :-
  instantiation_error(args(Term,Id)).



%! triple(?S:rdf_nonliteral, ?P:iri, ?O:rdf_term) is nondet.

triple(S, P, O) :-
  triple_uri_([], S, P, O, Uri),
  catch(http_call(Uri, triple_(S, P, O), [accept(nt)]), E, error_(E)).





% GENERICS %

%! error_(+E:compound) is semidet.

error_(E) :-
  ansi_format([fg(red)], "~w\n", [E]),
  fail.



%! json_(?Term:term, +In:stream) is nondet.
%
% There is a barber who shaves all and only men who do not shave
% themselves and only men who do not shave themselves.
%
% Does the barber shave himself?

json_(Term, In) :-
  set_stream(In, encoding(utf8)), % TBD
  call_cleanup(
    json_read_dict(In, L, [value_string_as(atom)]),
    close(In)
  ),
  (is_list(L) -> member(Term, L) ; Term = L).



%! json_request_(+Uri:atom, -Term:term) is nondet.

json_request_(Uri, Term) :-
  catch(http_call(Uri, json_(Term), [accept(json),failure(404)]), E, error_(E)).



%! lodalot_uri_(+Segments:list(atom), +Query:list(compound), -Uri:atom) is det.

lodalot_uri_(Segments, Query, Uri) :-
  uri_comps(Uri, uri(https,'hdt.lod.labs.vu.nl',Segments,Query,_)).



% semi-deterministic
query_term_(Compound1, Compound2) :-
  ground(Compound1),
  Compound1 =.. [Key,Term],
  rdf_term_to_atom(Term, Atom),
  Compound2 =.. [Key,Atom].



%! sameas_uri_(+Segments:list(atom), +Query:list(compound), -Uri:atom) is det.

sameas_uri_(Segments, Query, Uri) :-
  uri_comps(Uri, uri(https,'sameas.cc',Segments,Query,_)).



%! triple_(?S:non_literal, ?P:iri, ?O:rdf_term, +In:stream) is nondet.

triple_(S, P, O, In) :-
  call_cleanup(
    (
      rdf_read_ntriples(In, Triples, []),
      member(rdf(S,P,O), Triples)
    ),
    close(In)
  ).



%! triple_uri_(+Segments:list(atom), ?S:rdf_nonliteral, ?P:iri, ?O:rdf_term, -Uri:atom) is det.

triple_uri_(Segments, S, P, O, Uri) :-
  convlist(query_term_, [subject(S),predicate(P),object(O)], Query),
  lodalot_uri_([triple|Segments], Query, Uri).

customer(hugh).
customer(ida).
customer(jeremy).
customer(leroy).
customer(stella).

rose(cottage_beauty).
rose(golden_sunset).
rose(mountain_bloom).
rose(pink_paradise).
rose(sweet_dreams).

event(anniversary).
event(charity_auction).
event(retirement).
event(senior_prom).
event(wedding).

item(balloons).
item(candles).
item(chocolates).
item(place_cards).
item(streamers).

solve :-

	rose(JeremyRose), rose(HughRose), rose(IdaRose), rose(LeroyRose), rose(StellaRose),
	all_different([JeremyRose, HughRose, IdaRose, LeroyRose, StellaRose]),

	item(JeremyItem), item(HughItem), item(IdaItem), item(LeroyItem), item(StellaItem),
	all_different([JeremyItem, HughItem, IdaItem, LeroyItem, StellaItem]),

	event(JeremyEvent), event(HughEvent), event(IdaEvent), event(LeroyEvent), event(StellaEvent),
	all_different([JeremyEvent, HughEvent, IdaEvent, LeroyEvent, StellaEvent]),

	Quads = [ [jeremy, JeremyRose, JeremyEvent, JeremyItem],
		  [hugh, HughRose, HughEvent, HughItem],
		  [ida, IdaRose, IdaEvent, IdaItem],
		  [leroy, LeroyRose, LeroyEvent, LeroyItem],
		  [stella, StellaRose, StellaEvent, StellaItem] ],

	member([jeremy, _, senior_prom, _], Quads),
	\+ member([stella, _, wedding, _], Quads),
	member([stella, cottage_beauty, _, _], Quads),

	member([hugh, pink_paradise, _, _], Quads),
	\+ member([hugh, _, charity_auction, _], Quads),
	\+ member([hugh, _, wedding, _], Quads),

	member([_, _, anniversary, streamers], Quads),
	member([_, _, wedding, balloons], Quads),

	member([_, sweet_dreams, _, chocolates], Quads),
	\+ member([jeremy, mountain_bloom, _, _], Quads),

	member([leroy, _, retirement, _], Quads),
	member([_, _, senior_prom, candles], Quads),

	tell(jeremy, JeremyRose, JeremyEvent, JeremyItem),
  tell(hugh, HughRose, HughEvent, HughItem),
  tell(ida, IdaRose, IdaEvent, IdaItem),
  tell(leroy, LeroyRose, LeroyEvent, LeroyItem),
	tell(stella, StellaRose, StellaEvent, StellaItem).




all_different([H | T]) :- member(H, T), !, fail.
all_different([_ | T]) :- all_different(T).
all_different([_]).

tell(W, X, Y, Z) :-
    write(W), write(' got the '), write(X),
    write(' and the '), write(Z), write(' for '), write(Y), nl.

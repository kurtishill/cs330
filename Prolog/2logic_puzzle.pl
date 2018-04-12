teacher(appleton).
teacher(gross).
teacher(knight).
teacher(evoy).
teacher(parnell).

subject(english).
subject(gym).
subject(history).
subject(math).
subject(science).

state(california).
state(florida).
state(maine).
state(oregon).
state(virginia).

activity(antiquing).
activity(camping).
activity(sightseeing).
activity(spelunking).
activity(skiing).

solve :-

	subject(AppletonSubject), subject(GrossSubject), subject(KnightSubject), subject(EvoySubject), subject(ParnellSubject),
	all_different([AppletonSubject, GrossSubject, KnightSubject, EvoySubject, ParnellSubject]),

	state(AppletonState), state(GrossState), state(KnightState), state(EvoyState), state(ParnellState),
	all_different([AppletonState, GrossState, KnightState, EvoyState, ParnellState]),

	activity(AppletonActivity), activity(GrossActivity), activity(KnightActivity), activity(EvoyActivity), activity(ParnellActivity),
	all_different([AppletonActivity, GrossActivity, KnightActivity, EvoyActivity, ParnellActivity]),

	Quads = [ [appleton, AppletonSubject, AppletonState, AppletonActivity],
		  [gross, GrossSubject, GrossState, GrossActivity],
		  [knight, KnightSubject, KnightState, KnightActivity],
		  [evoy, EvoySubject, EvoyState, EvoyActivity],
		  [parnell, ParnellSubject, ParnellState, ParnellActivity] ],

	( 
		member([gross, math, _, _], Quads);
		member([gross, science, _, _], Quads)
	),

	( 
		member([gross, _, _, antiquing], Quads) -> member([gross, _, florida, _], Quads);
		member([gross, _, california, _], Quads)
	),

	( 
		member([_, science, california, skiing], Quads);
		member([_, science, florida, skiing], Quads)
	),

	( 
		member([evoy, history, maine, _], Quads);
		member([evoy, history, oregon, _], Quads)
	),

	member([parnell, _, _, spelunking], Quads),
	( 
		member([appleton, english, virginia, _], Quads);
		member([parnell, _, virginia, _], Quads)
	),

	
	\+ member([_, gym, maine, _], Quads),
	\+ member([_, _, maine, sightseeing], Quads),
	

	\+ member([gross, _, _, camping], Quads),
	( 
		member([appleton, _, _, camping], Quads);
		member([gross, _, _, camping], Quads)
	),

	( 
		member([appleton, _, _, antiquing], Quads);
		member([gross, _, _, antiquing], Quads)
	),

	
	tell(knight, KnightSubject, KnightState, KnightActivity),
	tell(gross, GrossSubject, GrossState, GrossActivity),
	tell(evoy, EvoySubject, EvoyState, EvoyActivity),
	tell(appleton, AppletonSubject, AppletonState, AppletonActivity),
	tell(parnell, ParnellSubject, ParnellState, ParnellActivity).

	

	


all_different([H | T]) :- member(H, T), !, fail.
all_different([_ | T]) :- all_different(T).
all_different([_]).


tell(W, X, Y, Z) :- write(W), write(', who teaches ' ), write(X), 
write(' went '), write(Z), write(' in '), write(Y), nl.
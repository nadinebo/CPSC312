/* Clue */
/* 
By: Mike Fink (o0p4) and Nadine Bolotov (c5n7)
Due Date: Thursday, November 27th, 12:00pm, use handin project2 */



clue :- setUp.

setUp :-		validateSuspects([scarlet,plum,peacock,green,mustard,white]),
				write_ln('Please enter rooms used (eg. [room1,room2].): '),
				read(Rooms),
				validateRooms(Rooms),nl,
				write_ln('Please enter weapons used (eg. [weapon1,weapon2].): '),
				read(Weapons),
				validateWeapons(Weapons),nl,
				write_ln('Please enter the number of players: '),
				read(Players),
				validatePlayersNumber(Players),
				createPlayerList(Players),
				write_ln('Please enter which player you are (eg. player2 = 2): '),
				read(P),
				validateMyPlayerNumber(P),
				write_ln('Please enter your PERSON cards: '),
				read(PCard),
				validateMyPeople(PCard),
				write_ln('Please enter your LOCATION cards: '),
				read(LCard),
				validateMyLocations(LCard),
				write_ln('Please enter your WEAPON cards: '),
				read(WCard),
				validateMyWeapons(WCard),
				write_ln('Please enter whose turn it is (eg. player1 = 1): '),
				read(Turn),
				validatePlayer(Turn),
				assert(inroom(start)),
				assert(pastroom(start)),
				write_ln('Please enter your character name (your piece on the board): '),
				read(Me),
				validateMe(Me),
				write_ln('Game initialized! To view the full list of instructions type: help.\nIf it is your turn, type myTurn. If it is another player\'s turn type otherTurn.'),!.



validsuspect(scarlet).
validsuspect(plum).
validsuspect(peacock).
validsuspect(green).
validsuspect(mustard).
validsuspect(white).

validplayersnum(2).
validplayersnum(3).
validplayersnum(4).
validplayersnum(5).
validplayersnum(6).

validplayer(1).
validplayer(2).
validplayer(3).
validplayer(4).
validplayer(5).
validplayer(6).

:- dynamic me/1.
:- dynamic myperson/1.
:- dynamic myweapon/1.
:- dynamic myroom/1.
:- dynamic myplayer/1.
:- dynamic numofplayers/1.
:- dynamic playerlist/1.
:- dynamic validweapon/1.
:- dynamic validroom/1.
:- dynamic suggestion/1.
:- dynamic showncards/1.
%:- dynamic cardsshown/1.
:- dynamic shownrooms/1.
:- dynamic shownpeople/1.
:- dynamic shownweapons/1.
:- dynamic shownbyme/1.
:- dynamic notshown/1.
:- dynamic myplay/1.
:- dynamic inroom/1.
:- dynamic pastroom/1.

:- dynamic possibleroom/1.
:- dynamic possibleperson/1.
:- dynamic possibleweapon/1.

:- dynamic hascard/2.
:- dynamic doesnothavecard/2.
:- dynamic has1of3cards/4.
:- dynamic assumedoesnothaveany/4.
:- dynamic assumenothavecard/2.
:- dynamic assumehascard/2.

:- dynamic isapossibility/1.

validateRooms([]).
validateRooms([H|T]) :- not(validroom(H)),
						assert(possibleroom(H)),
						assert(validroom(H)), validateRooms(T).

validateWeapons([]).
validateWeapons([H|T]) :- not(validweapon(H)),
						assert(possibleweapon(H)),
						assert(validweapon(H)), validateWeapons(T).

validateSuspects([]).
validateSuspects([H|T]) :- validsuspect(H),assert(possibleperson(H)),
						validateSuspects(T).


validatePlayersNumber(N) :- validplayersnum(N),assert(numofplayers(N)).

validatePlayer(P) :- validplayer(P).

validateMe(M) :- validsuspect(M),assert(me(M)),!.

validateMyPlayerNumber(N) :- validatePlayer(N),assert(myplayer(N)).

validateMyPeople([]).
validateMyPeople([H|T]) :- validateMyPerson(H), validateMyPeople(T).

validateMyPerson(S) :- validsuspect(S),assert(myperson(S)),
	myplayer(Player),assert(hascard(Player,S)).

validateMyLocations([]).
validateMyLocations([H|T]) :- validateMyLocation(H), validateMyLocations(T).

validateMyLocation(L) :- validroom(L),assert(myroom(L)),
	myplayer(Player),assert(hascard(Player,L)).

validateMyWeapons([]).
validateMyWeapons([H|T]) :- validateMyWeapon(H), validateMyWeapons(T).

validateMyWeapon(W) :- validweapon(W),assert(myweapon(W)),
	myplayer(Player),assert(hascard(Player,W)).



processTurn(T,T,P,L) :- assert(myplayer(T)),suggestFirst(P,L),!.
processTurn(T,Me,P,L).
suggestFirst(P,L)
	:- writeln('We are just starting here! Why don\'t you try: '),
		write(P),
		write(' in the '),write(L),
		write(' with a '),validweapon(W),write(W),write('?'),!.
		/* select random weapon from valid ones later */



subtract(X,Y,Z) :- Z is X - Y.

createPlayerList(1) :- assert(playerlist(1)),!.
createPlayerList(N) :- not(playerlist(N)),assert(playerlist(N)),
						subtract(N,1,Z),createPlayerList(Z).

printInRoomSize :- findall(H,inroom(H),Z),length(Z,N),writeln(N).

getInRoomSize :- findall(H,inroom(H),Z),length(Z,N).

movedTo(R) :- retract(inroom(X)), asserta(pastroom(X)),
				assert(inroom(R)).

wasin :- forall(pastroom(R),writeln(R)).


issuggested(Person,Room,Weapon,Player) :-	validsuspect(Person),validroom(Room),
											validweapon(Weapon),playerlist(Player),
											assert(suggestion([Person,Room,Weapon,Player])),
											writeln('Were any cards shown after this suggestion?'),
											read(R),
											shownCard(R,Person,Room,Weapon,Player).

mysuggestion(Person,Room,Weapon) :- assert(myplay([Person,Room,Weapon])).

shown(Card,room,Player) :- validroom(Card),assert(shownrooms([Card,Player])),
							validplayer(Player),removeRoomFromPossibilities(Card),
							writeln('Wasn\'t here! Do we have the answer? '),accuse,!.

shown(Card,person,Player) :- validsuspect(Card),assert(shownpeople([Card,Player])),
							validplayer(Player),removePersonFromPossibilities(Card),
							writeln('Innocent! Do we have the answer? '),accuse,!.

shown(Card,weapon,Player) :- validweapon(Card),assert(shownweapons([Card,Player])),
							validplayer(Player),removeWeaponFromPossibilities(Card),
							writeln('Clean! Do we have the answer? '),accuse,!.



noShownCard(Person,Room,Weapon,Player) :- assert(notshown([asked: Player, query: Person, Room, Weapon])).

hasnoneof(Person,Room,Weapon,Player) :- noShownCard(Person,Room,Weapon,Player).

shownCard(no,Person,Room,Weapon,Player):- solution(Person,Room,Weapon). %noShownCard(Person,Room,Weapon,Player). /* TODO */
shownCard(yes,Person,Room,Weapon,Player) :-	writeln('Enter the number of the player that showed the card:'),
												read(N),
						writeln('Enter the card shown:'),
						read(Card),
						shown(N,Card),
												myplayer(Me),howManyDontHave(Player,Me,N),deduceTheyDontHaveCards(Person,Room,Weapon,N),
												assert(showncards([showed: N,asked: Player, query: Person,Room,Weapon])).

ishowed(Card,Player,Person,Room,Weapon) :-		assert(shownbyme([showed: Card, to: Player, query: Person,Room,Weapon])).


validCard(Card) :- validweapon(Card).
validCard(Card) :- validroom(Card).
validCard(Card) :- validsuspect(Card).
cardType(Card,suspect) :- validsuspect(Card).
cardType(Card,weapon) :- validweapon(Card).
cardType(Card,room) :- validroom(Card).

shown(P,Card) :- validCard(Card),assert(hascard(P,Card)).

possibilities(Card) :-
	  validCard(Card),
	  not(owns(_,Card)),
	  not((cardType(Card,T),
	       cardType(OtherCard,T),
	       not(Card=OtherCard),
	       confirmed(OtherCard))).

confirmed(Card) :-
	forall(validplayer(P), logicDoesNotHave(P,Card)).

likely(Card) :-
	possibilities(Card),
	not(assumeOwns(_,Card)),
	not((cardType(Card,T),
	       cardType(OtherCard,T),
	       not(Card=OtherCard),
	       probable(OtherCard))).

probable(Card) :-
	forall(validplayer(P), assumeDoesNotHave(P,Card)).

logicDoesNotHave(P,Card) :- doesnothavecard(P,Card).
logicDoesNotHave(P,Card) :- me(P), not(hascard(P,Card)).

assumeDoesNotHave(P,Card) :- doesnothavecard(P,Card).
assumeDoesNotHave(P,Card) :- assumenothavecard(P,Card).
assumeDoesNotHave(P,Card) :- me(P),not(hascard(P,Card)).
assumeDoesNotHave(P,Card) :-
	(doesNotHaveBoth(P,Card,OtherCard);
	 doesNotHaveBoth(P,OtherCard,Card)),
	hascard(P,OtherCard),
	assert(assumenothavecard(P,Card)).

doesNotHaveBoth(P,Card1,Card2) :-
	(assumedoesnothaveany(P,Card,Card1,Card2);
	assumedoesnothaveany(P,Card1,Card,Card2);
	assumedoesnothaveany(P,Card1,Card2,Card)),
	hascard(P,Card).


owns(P,Card) :- hascard(P,Card).
owns(P,Card) :-
	(hasAtLeastOne(P,Card,OtherCard);
	 hasAtLeastOne(P,OtherCard,Card)),
	doesnothavecard(P,OtherCard),
	assert(hascard(P,Card)), clearAssume. %Comment out clearAssume if owns not working, the assume logic is maybe stretching things.

clearAssume :-
	hascard(P,Card),
	not(P1=P),
	foreach(assumehascard(P1,Card),retract(assumehascard(P1,Card))).
clearAssume :-
	hascard(P,Card),
	assumenothavecard(P,Card),
	retract(assumenothavecard(P,Card)).
clearAssume.


assumeOwns(P,Card) :- hascard(P,Card).
assumeOwns(P,Card) :- assumehascard(P,Card).
assumeOwns(P,Card) :-
	not(P1=P),
	suggestedBothTwice(P1,Card,Card1,Card2),
	(has1of3cards(P,Card,Card1,Card2);
	has1of3cards(P,Card1,Card,Card2);
	has1of3cards(P,Card1,Card2,Card);
	has1of3cards(P,Card,Card2,Card1);
	has1of3cards(P,Card2,Card,Card1);
	has1of3cards(P,Card2,Card1,Card)),
	assert(assumehascard(P,Card)).

suggestedBothTwice(P,Card,Card1,Card2) :-
	(assumedoesnothaveany(P,Card,Card1,Card2);
	assumedoesnothaveany(P1,Card1,Card,Card2);
	assumedoesnothaveany(P1,Card1,Card2,Card);
	assumedoesnothaveany(P1,Card,Card2,Card1);
	assumedoesnothaveany(P1,Card2,Card1,Card);
	assumedoesnothaveany(P1,Card2,Card,Card1)),
	(assumedoesnothaveany(P,X,Card1,Card2);
	assumedoesnothaveany(P1,Card1,X,Card2);
	assumedoesnothaveany(P1,Card1,Card2,X);
	assumedoesnothaveany(P1,X,Card2,Card1);
	assumedoesnothaveany(P1,Card2,Card1,X);
	assumedoesnothaveany(P1,Card2,X,Card1)),
	not(X=Card).


hasAtLeastOne(P,Card1,Card2) :-
	(has1of3cards(P,Card,Card1,Card2);
	has1of3cards(P,Card1,Card,Card2);
	 has1of3cards(P,Card1,Card2,Card)),
	doesnothavecard(P,Card).

myTurn :-
	checkAccusation,
	writeln('Do you want to make a logical suggestion, a tricky suggestion, or a sly suggestion? Enter logical./tricky./sly.'),
	read(R),
	myTurn(R),
	writeln('Please enter your suggested suspect: '),
	read(Suspect),
	writeln('Now your suggested room: '),
	read(Room),
	writeln('Now your suggested weapon: '),
	read(Weapon),
	writeln('Make your suggestion to the other players!'),!,
	myplayer(Me),
	suggested(Suspect,Room,Weapon,Me).

myTurn(logical) :- createSuggestion.
myTurn(tricky) :- createTrickySuggestion.
myTurn(sly) :- createSlySuggestion.

otherTurn :-
	writeln('Whose turn is it?'),
	read(Player),
	writeln('Please enter the suspect they suggested: '),
	read(Person),
	writeln('Now the suggested room: '),
	read(Room),
	writeln('Now the suggested weapon: '),
	read(Weapon),!,
	suggested(Person,Room,Weapon,Player).

createSuggestion :-
	suggestACombination(Card1,Card2,Card3),
	printSuggestion(Card1,Card3,Card2),!.

printSuggestion(Card1,Card2,Card3) :-
       writeln('May we suggest: '),
		write(Card1),
		write(' in the '),write(Card3),
		write(' with a '),write(Card2),write('?\n'),!.

% Tricky suggestions insert one of your own cards into a suggestion
createTrickySuggestion :-
	myplayer(Me),
	findall(Card,hascard(Me,Card),List),
	random_member(RandCard,List),
	suggestCorrectFormat(RandCard).

/*% Sly suggestions insert a card owned by the player furthest away from
% you in play order into a suggestion. If you do not have this info a
% tricky suggestion is offered instead.*/
createSlySuggestion :-
	myplayer(Me),
	getNextPlayer(Me,Next),
	findall(Card,hascard(Next,Card),List),
	random_member(RandCard,List),
	suggestCorrectFormat(RandCard).
createSlySuggestion :-
	writeln('Not enough info to make a sly suggestion, here is a tricky one instead.'),
	createTrickySuggestion.

suggestCorrectFormat(Card) :-
	cardType(Card,suspect),
	suggestACombination(Card,Card1,Card2),
	printSuggestion(Card,Card2,Card1).
suggestCorrectFormat(Card) :-
	cardType(Card,room),
	suggestACombination(Card1,Card,Card2),
	printSuggestion(Card1,Card2,Card).
suggestCorrectFormat(Card) :-
	cardType(Card,weapon),
	suggestACombination(Card1,Card2,Card),
	printSuggestion(Card1,Card,Card2).

/*% Avoid suggesting cards we know are the correct answer if possible by
% using one of our cards, if not possible, use a possible card. If we
% don't know the answer, guess a likely card. If there aren't any likely
% cards, guess a possible card.
*/
suggestACombination(Card1,Card2,Card3) :-
	suggestSuspect(Card1),
	suggestRoom(Card2),
	suggestWeapon(Card3).

% Complicated version.
/*
suggestACombination(Card1,Card2,Card3) :-
	cardType(Card1,suspect), likely(Card1), not(checkPerson(Card1)),!, %lock Prolog into the likely card
	cardType(Card2,weapon), likely(Card2), not(checkWeapon(Card2)),!,
	cardType(Card3,room), likely(Card3), not(checkRoom(Card3)).
suggestACombination(Card1,Card2,Card3) :-
	cardType(Card3,room), likely(Card3), not(checkRoom(Card3)),!,
	cardType(Card1,suspect), likely(Card1), not(checkPerson(Card1)),!,
	cardType(Card2,weapon), likely(Card2), not(checkWeapon(Card2)).
suggestACombination(Card1,Card2,Card3) :-
	cardType(Card2,weapon), likely(Card2), not(checkWeapon(Card2)),!,
	cardType(Card1,suspect), likely(Card1), not(checkPerson(Card1)),!,
	cardType(Card3,room), likely(Card3),  not(checkRoom(Card3)).
suggestACombination(Card1,Card2,Card3) :-
	cardType(Card1,suspect), possibilities(Card1), not(checkPerson(Card1)),
	cardType(Card2,weapon), possibilities(Card2), not(checkWeapon(Card2)),
	cardType(Card3,room), possibilities(Card3), not(checkRoom(Card3)).
suggestACombination(Card1,Card2,Card3) :-
	myplayer(Me),
	cardType(Card1,suspect), hascard(Me,Card1), possibilities(Card1),
	cardType(Card2,weapon), hascard(Me,Card2), possibilities(Card2),
	cardType(Card3,room), hascard(Me,Card3), possibilities(Card3).
*/

suggestSuspect(Card) :-
	checkPerson(Card1), cardType(Card,suspect), myplayer(Me), hascard(Me,Card), not(Card=Card1).
suggestSuspect(Card) :-
	checkPerson(Card1), cardType(Card,suspect), possibilities(Card), not(Card=Card1).
suggestSuspect(Card) :-
	cardType(Card,suspect), likely(Card).
suggestSuspect(Card) :-
	cardType(Card,suspect), possibilities(Card).
suggestSuspect(Card) :-
	 myplayer(Me), hascard(Me,Card).

suggestRoom(Card) :-
	checkRoom(Card1), cardType(Card,room), myplayer(Me), hascard(Me,Card), not(Card=Card1).
suggestRoom(Card) :-
	checkRoom(Card1), cardType(Card,room), possibilities(Card), not(Card=Card1).
suggestRoom(Card) :-
	cardType(Card,room), likely(Card).
suggestRoom(Card) :-
	cardType(Card,room), possibilities(Card).
suggestRoom(Card) :-
	myplayer(Me), hascard(Me,Card).

suggestWeapon(Card) :-
	checkWeapon(Card1), cardType(Card,weapon), myplayer(Me), hascard(Me,Card), not(Card=Card1).
suggestWeapon(Card) :-
	checkWeapon(Card1), cardType(Card,weapon), possibilities(Card), not(Card=Card1).
suggestWeapon(Card) :-
	cardType(Card,weapon), likely(Card).
suggestWeapon(Card) :-
	cardType(Card,weapon), possibilities(Card).
suggestWeapon(Card) :-
	myplayer(Me), hascard(Me,Card).

/* Enter the next suggestion. */
suggested(Person,Room,Weapon,Player) :-
	validplayer(Player),
	validroom(Room),
	validweapon(Weapon),
	validsuspect(Person),
	assert(assumedoesnothaveany(Player,Person,Room,Weapon)),
	writeln('Were any cards shown after this suggestion? yes/no'),
	read(R),
	assert(suggestion([Person,Room,Weapon,Player])),
	playerShowedCard(R,Person,Room,Weapon,Player),
	checkAccusation,!.

playerShowedCard(no,Person,Room,Weapon,Player):-
	getNextPlayer(Player,NextPlayer),
	recordDoesNotHave(Player,NextPlayer,Person,Room,Weapon),
	assert(notshown([Person,Room,Weapon,Player])),
	checkAccusation.
playerShowedCard(yes,Person,Room,Weapon,Player) :-
	myplayer(Player),
	writeln('Enter the number of the player that showed the card:'),
	read(N),
	writeln('Which card were you shown?'),
	read(Card),
	shown(N,Card),
	getNextPlayer(N,NextPlayer),
	assert(has1of3cards(N,Person,Room,Weapon)),
	recordDoesNotHave(Player,NextPlayer,Person,Room,Weapon).
playerShowedCard(yes,Person,Room,Weapon,Player) :-
	writeln('Enter the number of the player that showed the card:'),
	read(N),
	getNextPlayer(N,NextPlayer),
	assert(has1of3cards(N,Person,Room,Weapon)),
	recordDoesNotHave(Player,NextPlayer,Person,Room,Weapon).


recordDoesNotHave(Suggester,Suggester,_,_,_).
recordDoesNotHave(Suggester,Player,Person,Room,Weapon) :-
	getNextPlayer(Player,NextPlayer),
	assert(doesnothavecard(Player,Person)),
	assert(doesnothavecard(Player,Room)),
	assert(doesnothavecard(Player,Weapon)),
	recordDoesNotHave(Suggester,NextPlayer,Person,Room,Weapon).


modulo(M,N,Z) :- Z is mod(M,N).

getNextPlayer(P,Next) :-
	numofplayers(N),
	subtract(P,2,M),
	modulo(M,N,I),
	Next is I+1.

checkAccusation :-
	checkAccuse(Person,Room,Weapon),
	writeln('Accuse: '),
		write(Person),
		write(' in the '),write(Room),
		write(' with a '),write(Weapon),write('!'),!.
checkAccusation :-
	checkPerson(Person),
	writeln('We know whodunnit: '),
	write(Person).
checkAccusation :-
	checkRoom(Room),
	writeln('We know where it was done: '),
	write(Room).
checkAccusation :-
	checkWeapon(Weapon),
	writeln('We know the weapon: '),
	write(Weapon).
checkAccusation.

checkPerson(Person) :-
	cardType(Person,suspect),
	possibilities(Person),
	not((cardType(OtherPerson,suspect),
	    possibilities(OtherPerson),
	    not(Person=OtherPerson))).

checkRoom(Room) :-
	cardType(Room,room),
	possibilities(Room),
	not((cardType(OtherRoom,room),
	    possibilities(OtherRoom),
	    not(Room=OtherRoom))).

checkWeapon(Weapon) :-
	cardType(Weapon,weapon),
	possibilities(Weapon),
	not((cardType(OtherWeapon,weapon),
	    possibilities(OtherWeapon),
	    not(Weapon=OtherWeapon))).

checkAccuse(Person,Room,Weapon) :-
	checkPerson(Person),
	checkRoom(Room),
	checkWeapon(Weapon).
/*
	cardType(Person,person),
	cardType(Room,room),
	cardType(Weapon,weapon),
%	confirmed(Person), %these do not quite work logically
	%confirmed(Room),
	%confirmed(Weapon).
	possibilities(Person),
	not(cardType(OtherPerson,person),
	    possibilities(OtherPerson),
	    not(Person=OtherPerson)),
	possibilities(Room),
	not(cardType(OtherRoom,room),
	    possibilities(OtherRoom),
	    not(Room=OtherRoom)),
	possibilities(Weapon),
	not(cardType(OtherWeapon,weapon),
	    possibilities(OtherWeapon),
	    not(Weapon=OtherWeapon)).
*/

showPossible :-
	forall(possibilities(Card),writeln(Card)),!.

showLikely :-
	forall(likely(Card),writeln(Card)),!.



me :- me(Name),writeln(Name).

showRooms :- forall(validroom(R), writeln(R)).

showWeapons :- forall(validweapon(W), writeln(W)).

showSuspects :- forall(validsuspect(S), writeln(S)).

showPlayers :- forall(playerlist(P),writeln(P)).


showSuggested :- forall(suggestion(S), writeln(S)).

showMySuggestions :- forall(myplay(S), writeln(S)).

showShownCards :- forall(showncards(S), writeln(S)).

showShownRooms :-	forall(shownrooms(R), writeln(R)).

showShownPeople :-	forall(shownpeople(P), writeln(P)).

showShownWeapons :-	forall(shownweapons(W), writeln(W)).

showShownByMe :- forall(shownbyme(S), writeln(S)).

showNotShown :- forall(notshown(C), writeln(C)).

showPossibleRooms :- forall(possibleroom(R), writeln(R)).

showPossiblePeople :- forall(possibleperson(P), writeln(P)).

showPossibleWeapons :- forall(possibleweapon(W),writeln(W)).

whereami :- inroom(R),writeln(R).



showall :-	writeln('--------------------------'),
			tab(2),write('C U R R E N T'),tab(2),write('G A M E'),
			writeln(''),
			writeln('--------------------------'),
			tab(7),writeln('Suspects:'),
			showSuspects,
			tab(7),writeln('Rooms:'),
			showRooms,
			tab(7),writeln('Weapons:'),
			showWeapons,nl,
			tab(7),writeln('MOVES MADE'),
			writeln('--------------------------'),
			tab(7),writeln('Suggested:'),
			showSuggested,nl,
		
			tab(7),writeln('Shown Cards:'),
	
			showShownCards,nl,
		
			tab(7),writeln('Not Shown Cards:'),
			showNotShown,nl,
			tab(7),writeln('POSSIBILIES'),
			writeln('--------------------------'),
			tab(7),writeln('Possible Cards:'),
			showPossible,

			%tab(7),writeln('Likely Cards:'),
			%showLikely,
			writeln(''),
			writeln('--------------------------'),
			tab(7),writeln('You are in room:'),
			whereami,
			tab(7),writeln('Before this you were in room:'),
			wasin,
			writeln(''),
			writeln('--------------------------------------------------------'),
			tab(5),writeln('To view the full list of instructions type: help.'),!.


getAllRooms :- findall(H,validroom(H),Z),writeln(Z).

getRoomsSize :- findall(H,validroom(H),Z),length(Z,N),writeln(N).


printPossibleRoomsSize :- findall(H,possibleroom(H),Z),length(Z,N),writeln(N).

printPossiblePeopleSize :- findall(H,possibleperson(H),Z),length(Z,N),writeln(N).

printPossibleWeaponsSize :- findall(H,possibleweapon(H),Z),length(Z,N),writeln(N).


foundRoom :- findall(H,possibleroom(H),Z),length(Z,N),N =:= 1.

foundPerson :- findall(H,possibleperson(H),Z),length(Z,N),N =:= 1.

foundWeapon :- findall(H,possibleweapon(H),Z),length(Z,N),N =:= 1.


foundAll :- foundRoom, foundPerson, foundWeapon.


removeRoomFromPossibilities(R) :- retract(possibleroom(R)).

removePersonFromPossibilities(P) :- retract(possibleperson(P)).

removeWeaponFromPossibilities(W) :- retract(possibleweapon(W)).

removeMyRoom :- myroom(R),retract(possibleroom(R)),!.

removeMyPerson :- myperson(P),retract(possibleperson(P)),!.

removeMyWeapon :- myweapon(W),retract(possibleweapon(W)),!.


accuse :- foundAll,write('It was '),possibleperson(P), write(P),
			write(' in the '),possibleroom(R), write(R),
			write(' with a '),possibleweapon(W), write(W).



help :-		writeln('-------------------------'),
			tab(2),write('U S E R'),tab(2),write('M A N U A L'),
			writeln(''),
			writeln('-------------------------'),
			writeln('Here are the list of available options:'),
			writeln(''),
			tab(3),writeln('	showall:		To view all relevant game information, type: showall.'),
			writeln(''),
			tab(3),writeln('	otherTurn:		To record a suggestion made by a player. '),
			
		
			writeln(''),
			tab(3),writeln('	myTurn:			To get suggestions, type: myTurn.'),
			writeln(''),

			tab(3),writeln('	showPlayers:		To view players participating in the game, type: showPlayers.'),
			writeln(''),
			tab(3),writeln('	showPossible:		To view all possible rooms/people/weapons type: showPossible. '),
			writeln(''),
			tab(3),writeln('	showLikely:		To view all likely rooms/people/weapons type: showPossible. '),
			
			writeln(''),
			tab(3),writeln('	me:			To find what player piece you are on the board, type: me.'),
			writeln(''),
			tab(3),writeln('	whereami:		To view what room you are in on the board, type: whereami.'),
			writeln(''),
			tab(3),writeln('	movedTo:		To move your player into a different room, type: movedTo(<room>).'),
			writeln(''),
			tab(3),writeln('	wasin:			To view last visited room(descending order of recency), type: wasin.').





/*		*/

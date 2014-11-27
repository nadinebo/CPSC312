/* Clue */
/* Due Date: Thursday, November 27th, 12:00pm, use handin project2 */
/*
Requirements:

I need help to play the board game Clue (known as Cluedo in Europe).
Your job is to build me a Clue Player Assistant using the Prolog programming language.
The more work that your program does for me while I'm playing -- in other words, the
more it makes me look like an expert Clue player -- the better your program is.

----------------------------------------------------------------------------------------
Minimum deliverables:

The program should allow me to:
>	initialize the game setup easily
	-	which rooms and weapons are used in
	-	this version of the game?
	-	how many players?
	-	the order of play (whose turn next?)
	-	which cards am I holding?
	-	and so on...

The program should allow me to:
>	keep track of my own play
	-	I tell the program my suggestion ("I suggested Mrs. White, the rope, and
		the kitchen.")
	-	I tell the program what I learned ("Player X showed me the rope" or "Nobody
		showed me anything")

The program should allow me to:
>	see the contents of the database on demand
>	know when to make an accusation
	-	"Hey, Kurt, all that's left is Mr. Green, the knife, and the library.
		Go for it dude!"

The program should also have:
>	easy-to-understand documentation describing what the program can and can't do,
	how to use it, how it works
>	easy-to-use interface

The program just described would be an electronic note pad, and would be worth a C or
maybe a low B, depending on factors such as quality of programming, extensibility,
documentation, interface, and maybe other factors. This is easy.

----------------------------------------------------------------------------------------
The NEXT level:

In addition to the minimum deliverables, the program should:
>	take advantage of what can be inferred from the suggestions of other players
	-	"Player X suggested Miss Scarlet, the candlestick, and the conservatory. Player
		Y showed her a card."
>	tell me which suggestion to make next "You should suggest Col. Mustard, the wrench,
	and the billiard room."

This program would be much more than a note pad, and would earn a B to an A, depending
on the factors noted previously.

----------------------------------------------------------------------------------------
The A+ level:

The program should go beyond what's been described so far. Some suggestions:
>	build models of what the other players might know and use the models to assess how
	close they might be to winning
>	advise me to make a suggestion that might throw other players off
>	other things you might think of.





*** Possible Heuristics:
	+	Same player saying 2 same things and a different 3rd one (eg. a,b,c and a,b,d)
		they might have cards a,b.
	+	No cards shown for a set of suggestions! (at least one of the 3 is the right one)


*/

test3 :- validateRooms([kitchen,bar,bedroom,garage,library]),
		validateWeapons([wrench,flamethrower,gun,sewingneedle,rope]),
		validateSuspects([scarlet,plum,peacock,green,mustard,white]),
		validatePlayersNumber(3),
		createPlayerList(3),
		validateMyPerson(mustard),
		validateMyLocation(bar),
		validateMyWeapon(flamethrower),
		validateMyWeapon(rope),
		validateMyPlayerNumber(1),
		validateMe(green),
		assert(hascard(1,bar)),
		assert(hascard(1,mustard)),
		assert(hascard(1,flamethrower)),
		assert(hascard(1,rope)),
		assert(inroom(start)),
		assert(pastroom(start)),
		write_ln('Game initialized! To view the full list of instructions type: help.').


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


stub(_).

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

subtract(X,Y,Z) :- Z is X - Y.

createPlayerList(1) :- assert(playerlist(1)),!.
createPlayerList(N) :- not(playerlist(N)),assert(playerlist(N)),
						subtract(N,1,Z),createPlayerList(Z).

printInRoomSize :- findall(H,inroom(H),Z),length(Z,N),writeln(N).

getInRoomSize :- findall(H,inroom(H),Z),length(Z,N).

movedTo(R) :- retract(inroom(X)), asserta(pastroom(X)),
				assert(inroom(R)).

wasin :- forall(pastroom(R),writeln(R)).


/* Fill this in when someone suggests */
/* Later on can keep track of whether the card was shown or not */
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


/* THIS STUFF WAS CHANGED SO IT MIGHT BE NOT WORKING RIGHT NOW */
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

/*noShownCard(Person,Room,Weapon,Player) :- assert(notshown([asked: Player, query: Person, Room, Weapon])).

%hasnoneof(Person,Room,Weapon,Player) :- noShownCard(Person,Room,Weapon,Player).

shownCard(no,Person,Room,Weapon,Player):- solution(Person,Room,Weapon). %noShownCard(Person,Room,Weapon,Player).
shownCard(yes,Person,Room,Weapon,Player) :-	writeln('Enter the number of the player that showed the card:'),
												read(N),
												assert(showncards([showed: N,asked: Player, query: Person,Room,Weapon])).
*/

/* ====================================================================================
   MIKE'S SECTION BUILT ON TOP OF EXISTING STRUCTURE
   ====================================================================================*/
% Valid Card confirms that a card is one used in the game.
validCard(Card) :- validweapon(Card).
validCard(Card) :- validroom(Card).
validCard(Card) :- validsuspect(Card).

% Card Type checks what type of card a card is. Can be used to determine
% card type or to ensure a card is of a certain type.
cardType(Card,suspect) :- validsuspect(Card).
cardType(Card,weapon) :- validweapon(Card).
cardType(Card,room) :- validroom(Card).

% When we are shown a card, this function inserts that information into
% our database.
shown(P,Card) :- validCard(Card),assert(hascard(P,Card)).

% Checks that a card is valid, that no one else owns the card that we
% know of, and that we haven't confirmed a different card. If those
% things are true then the card might be in the envelope.
possibilities(Card) :-
	  validCard(Card),
	  not(owns(_,Card)),
	  not((cardType(Card,T),
	       cardType(OtherCard,T),
	       not(Card=OtherCard),
	       confirmed(OtherCard))).

% If all players do not have a card then it is confirmed in the
% envelope.
confirmed(Card) :-
	forall(validplayer(P), logicDoesNotHave(P,Card)).

% Likely cards rely on assumptions that might not be true. All possible
% cards are likely cards. A card is likely if we don't assume that
% someone else owns it, and if we don't already have another probable
% card.
likely(Card) :-
	possibilities(Card),
	not(assumeOwns(_,Card)),
	not((cardType(Card,T),
	       cardType(OtherCard,T),
	       not(Card=OtherCard),
	       probable(OtherCard))).

% Probable cards are cards that we assume each player does not have.
probable(Card) :-
	forall(validplayer(P), assumeDoesNotHave(P,Card)).

% Logically, if we know a player does not have a card then they do not
% have the card. Or, if I am the player, I know which cards I do not
% have.
logicDoesNotHave(P,Card) :- doesnothavecard(P,Card).
logicDoesNotHave(P,Card) :- me(P), not(hascard(P,Card)).

% If we know someone does not have a card then we can assume they do not
% as well. If we assume a player does not have the card, then this
% returns true. If a player is assumed to not have both of two cards and
% they do have one of those cards then we can assume they do not have
% the other.
assumeDoesNotHave(P,Card) :- doesnothavecard(P,Card).
assumeDoesNotHave(P,Card) :- assumenothavecard(P,Card).
assumeDoesNotHave(P,Card) :-
	(doesNotHaveBoth(P,Card,OtherCard);
	 doesNotHaveBoth(P,OtherCard,Card)),
	hascard(P,OtherCard),
	assert(assumenothavecard(P,Card)).

% If we assume that a player does not have any of 3 cards and we know
% that they do have one of them, then we can assume they don't have both
% of the others.
doesNotHaveBoth(P,Card1,Card2) :-
	(assumedoesnothaveany(P,Card,Card1,Card2);
	assumedoesnothaveany(P,Card1,Card,Card2);
	assumedoesnothaveany(P,Card1,Card2,Card)),
	hascard(P,Card).

% If a player owns a card then they have the card. They can also have
% the card if they have at least one of two and we know they do not have
% the other. If we learn a player has a card, we can update assumptions
% that were incorrect about that player-card combination.
owns(P,Card) :- hascard(P,Card).
owns(P,Card) :-
	(hasAtLeastOne(P,Card,OtherCard);
	 hasAtLeastOne(P,OtherCard,Card)),
	doesnothavecard(P,OtherCard),
	assert(hascard(P,Card)), clearAssume.

% Clears assumptions. E.G. if we know X has a card, then we should not
% assume that anyone else has it also. If we know that X has a card,
% then we should not assume that X does not have that card. This
% function always returns true.
clearAssume :-
	hascard(P,Card),
	not(P1=P),
	foreach(assumehascard(P1,Card),retract(assumehascard(P1,Card))).
clearAssume :-
	hascard(P,Card),
	assumenothavecard(P,Card),
	retract(assumenothavecard(P,Card)).
clearAssume.

% We assume a player has a card if they have the card or we assume that
% they have the card. If a different player made two suggestions with 2
% of 3 cards the same. And the current player showed a card on the first
% of the suggestions, then we can assume that that player owned the card
% that changed.
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

% A player suggested a card twice if there are two entries in the
% assumedoesnothaveany database for the same two cards from the same
% player in any order.
suggestedBothTwice(P,Card,Card1,Card2) :-
	(assumedoesnothaveany(P,Card,Card1,Card2);
	assumedoesnothaveany(P,Card1,Card,Card2);
	assumedoesnothaveany(P,Card1,Card2,Card);
	assumedoesnothaveany(P,Card,Card2,Card1);
	assumedoesnothaveany(P,Card2,Card1,Card);
	assumedoesnothaveany(P,Card2,Card,Card1)),
	(assumedoesnothaveany(P,X,Card1,Card2);
	assumedoesnothaveany(P,Card1,X,Card2);
	assumedoesnothaveany(P,Card1,Card2,X);
	assumedoesnothaveany(P,X,Card2,Card1);
	assumedoesnothaveany(P,Card2,Card1,X);
	assumedoesnothaveany(P,Card2,X,Card1)),
	not(X=Card).

% A player has at least one card if they have 1 of 3 cards and we know
% they don't have one of those three. Ie. they have one of the other
% two.
hasAtLeastOne(P,Card1,Card2) :-
	(has1of3cards(P,Card,Card1,Card2);
	has1of3cards(P,Card1,Card,Card2);
	 has1of3cards(P,Card1,Card2,Card)),
	doesnothavecard(P,Card).

% Call this function to start inputting your turn information.
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

% Call this function to start inputting another player's turn
% information.
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

% Creates a suggestion and prints it.
createSuggestion :-
	suggestACombination(Card1,Card2,Card3),
	printSuggestion(Card1,Card3,Card2),!.

% Prints a suggestion in a nice format.
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

% Sly suggestions insert a card owned by the player furthest away from
% you in play order into a suggestion. If you do not have this info a
% tricky suggestion is offered instead.
createSlySuggestion :-
	myplayer(Me),
	getNextPlayer(Me,Next),
	findall(Card,hascard(Next,Card),List),
	random_member(RandCard,List),
	suggestCorrectFormat(RandCard).
createSlySuggestion :-
	writeln('Not enough info to make a sly suggestion, here is a tricky one instead.'),
	createTrickySuggestion.

% Formats a suggestion correctly.
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

% Avoid suggesting cards we know are the correct answer if possible by
% using one of our cards, if not possible, use a possible card. If we
% don't know the answer, guess a likely card. If there aren't any likely
% cards, guess a possible card.
suggestACombination(Card1,Card2,Card3) :-
	suggestSuspect(Card1),
	suggestRoom(Card2),
	suggestWeapon(Card3).

% Using the logic described for suggestACombination to suggest a
% suspect.
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


% Using the logic described for suggestACombination to suggest a
% room.
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


% Using the logic described for suggestACombination to suggest a
% weapon.
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

% Enter the next suggestion.
suggested(Person,Room,Weapon,Player) :-
	validplayer(Player),
	validroom(Room),
	validweapon(Weapon),
	validsuspect(Person),
	assert(assumedoesnothaveany(Player,Person,Room,Weapon)),
	writeln('Were any cards shown after this suggestion? yes/no'),
	read(R),
	playerShowedCard(R,Person,Room,Weapon,Player),
	checkAccusation,!.

% When a player shows a card, we gather that information and add it to
% the database. If no one showed a card then there's probably an
% accusation to be made.
playerShowedCard(no,Person,Room,Weapon,Player):-
	getNextPlayer(Player,NextPlayer),
	recordDoesNotHave(Player,NextPlayer,Person,Room,Weapon),
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

% This records information about when a player does not have a card.
recordDoesNotHave(Suggester,Suggester,_,_,_).
recordDoesNotHave(Suggester,Player,Person,Room,Weapon) :-
	getNextPlayer(Player,NextPlayer),
	assert(doesnothavecard(Player,Person)),
	assert(doesnothavecard(Player,Room)),
	assert(doesnothavecard(Player,Weapon)),
	recordDoesNotHave(Suggester,NextPlayer,Person,Room,Weapon).

% Simple modulo implementation.
modulo(M,N,Z) :- Z is mod(M,N).

% Get player that comes before you (oddly named but used for checking
% when players do not have cards).
getNextPlayer(P,Next) :-
	numofplayers(N),
	subtract(P,2,M),
	modulo(M,N,I),
	Next is I+1.

% Checks whether we can make an accusation.
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

% Checks if we know that a person was guilty.
checkPerson(Person) :-
	cardType(Person,suspect),
	possibilities(Person),
	not((cardType(OtherPerson,suspect),
	    possibilities(OtherPerson),
	    not(Person=OtherPerson))).
% Checks if we know which room it happened in.
checkRoom(Room) :-
	cardType(Room,room),
	possibilities(Room),
	not((cardType(OtherRoom,room),
	    possibilities(OtherRoom),
	    not(Room=OtherRoom))).
% Checks if we know which weapon was used.
checkWeapon(Weapon) :-
	cardType(Weapon,weapon),
	possibilities(Weapon),
	not((cardType(OtherWeapon,weapon),
	    possibilities(OtherWeapon),
	    not(Weapon=OtherWeapon))).

% Checks the person, room and weapon.
checkAccuse(Person,Room,Weapon) :-
	checkPerson(Person),
	checkRoom(Room),
	checkWeapon(Weapon).

% Prints all possible cards.
showPossible :-
	forall(possibilities(Card),writeln(Card)),!.

% Prints all likely cards.
showLikely :-
	forall(likely(Card),writeln(Card)),!.

me :- me(Name),writeln(Name).

showRooms :- forall(validroom(R), writeln(R)).

showWeapons :- forall(validweapon(W), writeln(W)).

showSuspects :- forall(validsuspect(S), writeln(S)).

showPlayers :- forall(playerlist(P),writeln(P)).

/* Can see patterns someone suggesting 1,2,3 and 1,2,4 they have 1,2 */
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


%hasshown(X) :- shownpeople(X),

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
			tab(7),writeln('My Suggestions:'),
			showMySuggestions,nl,
			tab(7),writeln('Shown Cards:'),nl,
			tab(0),writeln('To Others:'),
			showShownCards,nl,
			%tab(7),writeln('Shown Cards:'),nl,
			tab(0),writeln('To Me:'),
			tab(7),writeln('* People:'),
			showShownPeople,
			tab(7),writeln('* Rooms:'),
			showShownRooms,
			tab(7),writeln('* Weapons:'),
			showShownWeapons,nl,
			tab(0),writeln('By Me:'),
			showShownByMe,nl,nl,
			tab(7),writeln('Not Shown Cards:'),
			showNotShown,nl,
			tab(7),writeln('POSSIBILIES'),
			writeln('--------------------------'),
			tab(7),writeln('Possible People:'),
			showPossiblePeople,
			tab(7),writeln('Possible Rooms:'),
			showPossibleRooms,
			tab(7),writeln('Possible Weapons:'),
			showPossibleWeapons,
			writeln(''),
			writeln('--------------------------'),
			tab(7),writeln('You are in room:'),
			whereami,
			tab(7),writeln('Before this you were in room:'),
			wasin,!.

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
			tab(3),writeln('	issuggested:		To record a suggestion made by a player, '),
			tab(9),writeln('			type: issuggested(<person>,<room>,<weapon>,<player>).'),
			tab(9),writeln('			Eg. player 1 suggested "scarlet in the kitchen with a gun" '),
			tab(9),writeln('			would be issugested(scarlet,kitchen,gun,1).'),
			writeln(''),
			tab(3),writeln('	mysuggestion:		To your own suggestion, type: '),
			tab(9),writeln('			mysuggestion(<person>,<room>,<weapon>).'),
			tab(9),writeln('			Eg. If your suggestion is "scarlet in the kitchen with a gun" '),
			tab(9),writeln('			type mysuggestion(scarlet,kitchen,gun).'),
			writeln(''),
			tab(3),writeln('	shown:			To record a card that was shown to you, type: '),
			tab(9),writeln('			shown(<name>,<type>,<player>).'),
			tab(9),writeln('			Eg. player 1 showed "scarlet" = shown(scarlet,person,1).'),
			writeln(''),
			tab(3),writeln('	shownCard:		To manually input if an unknown card was shown to another player,'),
			tab(9),writeln('			type: shownCard(<yes/no>,<person>,<room>,<weapon>,<suggester>).'),
			tab(9),writeln('			Eg. player 1 showed a card to player 2 after a query:'),
			tab(9),writeln('			"scarlet,kitchen,gun" = shownCard(yes,scarlet,kitchen,gun,2).'),
			writeln(''),
			tab(3),writeln('	show<TYPE>:		To view all rooms/suspects/weapons used in this game, type: '),
			tab(9),writeln('			showRooms. or showSuspects. or showWeapons  respectively.'),
			writeln(''),
			tab(3),writeln('	showPlayers:		To view players participating in the game, type: showPlayers.'),
			writeln(''),
			tab(3),writeln('	showPossible<TYPE>:	To view all possible rooms/people/weapons type: '),
			tab(9),writeln('			showPossibleRooms. or showPossiblePeople. or showPossibleWeapons.'),
			writeln(''),
			tab(3),writeln('	foundall:		To manually view if the solution has been found, type: foundall.'),
			writeln(''),
			tab(3),writeln('	found<TYPE>:		To manually view if any of the components of the solution'),
			tab(9),writeln('			have been found, type: foundPerson. or foundRoom. or foundWeapon.'),
			writeln(''),
			tab(3),writeln('	me:			To find what player piece you are on the board, type: me.'),
			writeln(''),
			tab(3),writeln('	whereami:		To view what room you are in on the board, type: whereami.'),
			writeln(''),
			tab(3),writeln('	movedTo:		To move your player into a different room, type: movedTo(<room>).'),
			writeln(''),
			tab(3),writeln('	wasin:			To view last visited room(descending order of recency), type: wasin.').



/*		*/

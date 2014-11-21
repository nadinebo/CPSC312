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

test :- validateRooms([kitchen,bar,bedroom,garage,library]),
		validateWeapons([wrench,flamethrower,gun,sewingneedle,rope]),
		validateSuspects([scarlet,plum,peacock,green,mustard,white]),
		validatePlayersNumber(2),
		validateMyPerson(mustard),
		validateMyLocation(bar),
		validateMyWeapon(flamethrower),
		validateMyPlayerNumber(1),
		validateMe(green),
		removeMyRoom, 
		removeMyPerson, 
		removeMyWeapon.
		
test1 :- validateRooms([kitchen,bar,bedroom]),
		validateWeapons([wrench,knife,gun]),
		validateSuspects([scarlet,plum,peacock]),
		validatePlayersNumber(2),
		validateMyPerson(scarlet),
		validateMyLocation(bar),
		validateMyWeapon(wrench),
		validateMyPlayerNumber(1),
		validateMe(plum),
		removeMyRoom, 
		removeMyPerson, 
		removeMyWeapon.
		

clue :- setUp.

setUp :- 		validateSuspects([scarlet,plum,peacock,green,mustard,white]),
				write_ln('Please enter rooms used: '),
				read(Rooms),
				validateRooms(Rooms),nl,
				write_ln('Please enter weapons used: '),
				read(Weapons),
				validateWeapons(Weapons),nl,
				write_ln('Please enter the number of players: '),
				read(Players),
				validatePlayersNumber(Players),
				write_ln('Please enter your PERSON card: '),
				read(PCard),
				validateMyPerson(PCard),
				write_ln('Please enter your LOCATION card: '),
				read(LCard),
				validateMyLocation(LCard),
				write_ln('Please enter your WEAPON card: '),
				read(WCard),
				validateMyWeapon(WCard),
				write_ln('Please enter whose turn it is (eg. player1 etc.): '),
				read(Turn),
				validatePlayer(Turn),
				write_ln('Please enter which player you are (eg. player2): '),
				read(P),
				validateMyPlayerNumber(P),
				removeMyRoom, 
				removeMyPerson, 
				removeMyWeapon,
				write_ln('Please enter your player\'s name (your piece on the board): '),
				read(Me),
				validateMe(Me),!,
				processTurn(Turn, Me,PCard,LCard),!.


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

/*validplayer(player1).
validplayer(player2).
validplayer(player3).
validplayer(player4).
validplayer(player5).
validplayer(player6).*/

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
:- dynamic validweapon/1.
:- dynamic validroom/1.
:- dynamic suggestion/1.
:- dynamic shownrooms/1.
:- dynamic shownpeople/1.
:- dynamic shownweapons/1.
:- dynamic myplay/1.

:- dynamic possibleroom/1.
:- dynamic possibleperson/1.
:- dynamic possibleweapon/1.

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

validateMyPerson(S) :- validsuspect(S),assert(myperson(S)).

validateMyLocation(L) :- validroom(L),assert(myroom(L)).

validateMyWeapon(W) :- validweapon(W),assert(myweapon(W)).


/* My turn */

validateMe(P) :- assert(myplayer(T)).

processTurn(T,T,P,L) :- assert(myplayer(T)),suggestFirst(P,L),!.
processTurn(T,Me,P,L).
suggestFirst(P,L)
	:- write('We are just starting here! Why don\'t you try: '), 
		write(P),
		write(' in the '),write(L),
		write(' with a '),write( 'gun?'),!.
		/* select random weapon from valid ones later */


suggest(T) :- stub(T). /*check history and suggested + heuristic used here */


/* Fill this in when someone suggests */
/* Later on can keep track of whether the card was shown or not */
issuggested([Person,Room,Weapon,Player]) :- assert(suggestion([Person,Room,Weapon,Player])).
mysuggestion([Person,Room,Weapon]) :- assert(myplay([Person,Room,Weapon])).

shown(Card,room,Player) :- validroom(Card),assert(shownrooms([Card,Player])),
							validplayer(Player),removeRoomFromPossibilities(Card),
							writeln('Wasn\'t here! Do we have the answer? '),accuse,!.

shown(Card,person,Player) :- validsuspect(Card),assert(shownpeople([Card,Player])),
							validplayer(Player),removePersonFromPossibilities(Card),
							writeln('Innocent! Do we have the answer? '),accuse,!.

shown(Card,weapon,Player) :- validweapon(Card),assert(shownweapons([Card,Player])),
							validplayer(Player),removeWeaponFromPossibilities(Card),
							writeln('Clean! Do we have the answer? '),accuse,!.

showRooms :- forall(validroom(R), writeln(R)).

showWeapons :- forall(validweapon(W), writeln(W)).

showSuspects :- forall(validsuspect(S), writeln(S)).

showPlayers :- forall(validplayer(P), writeln(P)). /* TODO make it print only the numofplayers */

/* Can see patterns someone suggesting 1,2,3 and 1,2,4 they have 1,2 */
showSuggested :- forall(suggestion(S), writeln(S)).

showMySuggestions :- forall(myplay(S), writeln(S)).

showShownRooms :-	forall(shownrooms(R), writeln(R)).

showShownPeople :-	forall(shownpeople(P), writeln(P)).

showShownWeapons :-	forall(shownweapons(W), writeln(W)).

showPossibleRooms :- forall(possibleroom(R), writeln(R)).

showPossiblePeople :- forall(possibleperson(P), writeln(P)).

showPossibleWeapons :- forall(possibleweapon(W),writeln(W)).


showall :- 	tab(7),writeln('Suspects:'),
			showSuspects,
			tab(7),writeln('Rooms:'),
			showRooms,
			tab(7),writeln('Weapons:'),
			showWeapons,nl,
			tab(7),writeln('Suggested:'),
			showSuggested,
			tab(7),writeln('My Suggestions:'),
			showMySuggestions,nl,
			tab(7),writeln('Shown Cards:'),nl,
			tab(7),writeln('* People:'),
			showShownPeople,
			tab(7),writeln('* Rooms:'),
			showShownRooms,
			tab(7),writeln('* Weapons:'),
			showShownWeapons,
			tab(7),writeln('Possible People:'),
			showPossiblePeople,
			tab(7),writeln('Possible Rooms:'),
			showPossibleRooms,
			tab(7),writeln('Possible Weapons:'),
			showPossibleWeapons.

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


%roomSuggestion :- 

/*		*/
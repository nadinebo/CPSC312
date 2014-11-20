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

clue :- setUp.

setUp :- 		write_ln('Please enter rooms used: '),
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
				validateSuspect(PCard),
				write_ln('Please enter your LOCATION card: '),
				read(LCard),
				validateLocation(LCard),
				write_ln('Please enter your WEAPON card: '),
				read(WCard),
				validateWeapon(WCard),
				write_ln('Please enter whose turn it is (eg. player1 etc.): '),
				read(Turn),
				validatePlayer(Turn),
				write_ln('Please enter which player you are (eg. player2): '),
				read(Me),
				validatePlayer(Me),!.
				/*processTurn(Turn, Me,PCard,LCard),!.*/


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

validplayer(player1).
validplayer(player2).
validplayer(player3).
validplayer(player4).
validplayer(player5).
validplayer(player6).

:- dynamic myperson/1.
:- dynamic myweapon/1.
:- dynamic myroom/1.
:- dynamic myplayer/1.
:- dynamic validweapon/1.
:- dynamic validroom/1.
:- dynamic suggestion/1.
:- dynamic shownrooms/1.
:- dynamic shownpeople/1.
:- dynamic shownweapons/1.

validroom(_).

validateRooms([]).
validateRooms([H|T]) :- assert(validroom(H)), validateRooms(T).

validateWeapons([]).
validateWeapons([H|T]) :- assert(validweapon(H)), validateWeapons(T).

validateWeapon(W) :- validweapon(W),assert(myweapon(W)).

validatePlayersNumber(N) :- validplayersnum(N).

validatePlayer(P) :- validplayer(P).

validateSuspect(P) :- validsuspect(P),assert(myperson(P)).

validateLocation(L) :- validroom(L),assert(myroom(L)).

/* My turn */
/*processTurn(T,Me,P,L).*/

processTurn(T,T,P,L) :- assert(myplayer(T)),suggestFirst(P,L),!.

suggestFirst(P,L)
	:- write('We are just starting here! Why don\'t you try: '), 
		write(P),
		write(' in the '),write(L),
		write(' with a '),write( 'gun?'),!.
		/* select random weapon from valid ones later */


suggest(T) :- stub(T). /*check history */

test(R) :- assert(myroom(R)),write(myroom).

/* Fill this in when someone suggests */
issuggested([Person,Room,Weapon,Player]) :- assert(suggestion([Person,Room,Weapon,Player])).

/* Can see patterns someone suggesting 1,2,3 and 1,2,4 they have 1,2 */
%allsuggestions :- 

%show :-

shown(Card,room,Player) :- validroom(Card),assert(shownrooms(Card)),validplayer(Player),!.

shown(Card,person,Player) :- validsuspect(Card),assert(shownpeople(Card)),validplayer(Player),!.

shown(Card,weapon,Player) :- validweapon(Card),assert(shownweapons(Card)),validplayer(Player),!.




/* ASG4 - Prolog Flights
 * Matthew Lo
 * mamlo
 * 1464155
 */

/* implementing not in Prolog */
not( X ) :- X, !, fail.
not( _ ).

/* function to convert degrees to radians */
degmin_to_rad( degmin( Degrees, Minutes), Rad ) :-
   Deg is Degrees + Minutes / 60,
   Rad is (Deg * pi) / 180.

/* adopted from haversine formula pdf */
haversine_formula( Lat1, Lon1, Lat2, Lon2, D ) :-
   Dlon is Lon2 - Lon1,
   Dlat is Lat2 - Lat1,
   A is (sin( Dlat/2 )) ** 2 + cos( Lat1 )
      * cos( Lat2 ) * (sin( Dlon/2 )) ** 2,
   C is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
   D is 3961 * C.

/* convert to radians before doing haversine */
distance( Airport1, Airport2, D ) :-
   airport( Airport1, _, Lat1, Lon1 ),
   airport( Airport2, _, Lat2, Lon2 ),
   degmin_to_rad( Lon1, Lon1rad ),
   degmin_to_rad( Lat1, Lat1rad ),
   degmin_to_rad( Lon2, Lon2rad ),
   degmin_to_rad( Lat2, Lat2rad ),
   haversine_formula( Lat1rad, Lon1rad, Lat2rad, Lon2rad, D ).

/* convert hours & minutes to just hours */
time_in_hours( time( Hours, Mins ), Hoursfloat ) :-
   Hoursfloat is Hours + Mins / 60.

/* find the time traveling at 500 mph */
miles_to_hours( Miles, Hours) :-
   Hours is Miles / 500.

/* if Digits is < 10, print 0 before printing the single digit */
print_digits( Digits ) :-
   Digits < 10, 
   print( 0 ), print( Digits ).

/* else, print the two digits */
print_digits( Digits ) :-
   Digits >= 10,
   print( Digits ).

/* print out the final time */
print_final_time( Hoursfloat ) :-
   Totalmin is floor( Hoursfloat * 60 ),
   Hours is Totalmin // 60,
   Mins is Totalmin mod 60,
   print_digits( Hours ),
   print( ':' ),
   print_digits( Mins ).

/* Find paths from a departure airport to an arrival airport.
 * A flight transfer always takes 30 minutes. There are no
 * overnight trips. The complete trip must depart and arrive
 * in the same day. 
 */

/* flightpath, a recursive function that recursively finds new
 * flights until there is a flight that reaches the destination
 * modified from grapthpaths.pl */

/* if arrived at the destination, end */

flightpath( ArriveTerm, ArriveTerm, _, [ArriveTerm], _ ).

/* if there is a direct flight from this terminal to the destination, */

flightpath( DepartTerm, ArriveTerm, Listvisited, 
   [[DepartTerm, Depart, Arrive] | List], HMDeparture ) :-
   /* if there is a flight from departure to arrival terminal */
   flight( DepartTerm, ArriveTerm, HMDeparture ),
   /* if the arrival terminal hasnt already been visited */
   not( member( ArriveTerm, Listvisited ) ),
   /* find the distance between the
    * two terminals and the time traveled */
   distance( DepartTerm, ArriveTerm, DistBetw ),

   miles_to_hours( DistBetw, TravelTime ),
   /* find the departure time in hours */
   time_in_hours( HMDeparture, Depart ),
   Arrive is Depart + TravelTime,
   /* there are no overnight trips */
   /* if the time is less than 24 hours*/
   Arrive < 24.0,
   flightpath( ArriveTerm, ArriveTerm, 
      [ArriveTerm | Listvisited], List, _ ).

/* else, find next path */
flightpath( DepartTerm, ArriveTerm, Listvisited,
   [[DepartTerm, Depart, Arrive] | List], HMDeparture ) :-
   /* if there is a flight from departure to another terminal */
   flight( DepartTerm, AnotherTerm, HMDeparture ),
   /* if AnotherTerm hasn't already been visited */
   not(member( AnotherTerm, Listvisited )),

   /* find the distance between the 
    * two terminals and the time traveled */
   distance( DepartTerm, AnotherTerm, DistBetw ),
   miles_to_hours( DistBetw, TravelTime ),
   
   /* find the departure time in hours */
   time_in_hours( HMDeparture, Depart ),
   Arrive is Depart + TravelTime,
   /* there are no overnight trips*/
   /* first check if the arrival time is less than 24 hours */
   Arrive < 24.0,

   /* since we havent arrived at the destination yet,
    * this flight must be a connecting flight */
   flight( AnotherTerm, _, ConnectingHM ),
   time_in_hours( ConnectingHM, ConnectingTime ),

  /* the departure of a connecting flight must be at least 
   * 30 minutes later than the arrival of the incoming
   * flight */
   (Arrive + 0.5) =< ConnectingTime,
   /* add AnotherTerm in to our list of visited paths.
    * Also add it to the path and call flightpath from there */ 
   flightpath( AnotherTerm, ArriveTerm,
      [AnotherTerm | Listvisited], List, ConnectingHM).

writepath( [[F, FD, FA], S | []] ) :-
   airport( F, FName, _, _ ), airport( S, SName, _, _ ),
   write( ' ' ), write( 'depart '), write( F ), write( ' ' ),
   write( FName ), write( ' ' ), print_final_time( FD ), nl,
   write( ' ' ), write( 'arrive '), write( S ), write( ' ' ),
   write( SName ), write( ' ' ), print_final_time( FA ), nl.

writepath( [[F, FD, FA], [S, SD, SA] | Tail] ) :-
   airport( F, FName, _, _ ), airport( S, SName, _, _ ),
   write( ' ' ), write( 'depart '), write( F ), write( ' ' ),
   write( FName ), write( ' ' ), print_final_time( FD ), nl,
   write( ' ' ), write( 'arrive '), write( S ), write( ' ' ),
   write( SName ), write( ' ' ), print_final_time( FA ), nl,
   writepath( [[S, SD, SA] | Tail] ).

/* if trying to fly to the same airport */
fly( StartPoint, StartPoint ) :-
   write('Error: Start terminal and '),
   write('Destination terminal are the same.'),
   nl, !, fail.

/* if trying to fly to a valid destination */
fly( StartPoint, Destination ) :-
   airport( StartPoint, _, _, _ ),
   airport( Destination, _, _, _ ),
   flightpath( StartPoint, Destination, [StartPoint], List, _ ),
   !, nl,
   writepath( List ),
   true.

/* else if airport exists but no flight exists */

fly( StartPoint, Destination ) :-
   airport( StartPoint, _, _, _ ),
   airport( Destination, _, _, _),
   write( 'Error: Either there is no existing flight from '),
   write( StartPoint ), write(' to '),
   write( Destination ),
   write( ' or the flight cannot be accomplished in 24 hours'), nl,
   !, fail.

/* else if trying to fly to invalid destination */
fly( _, _ ) :-
   write('Error: Trying to fly to invalid destination'), nl,
   !, fail.

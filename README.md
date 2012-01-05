Prolog Project: Cab Driver
==========================

General
-------
This is the project for the 1st Master course Declarative Programming on the Vrije Universiteit Brussel.

Description
-----------
You are the owner of the world famous taxi company “El Caberino”. As the crisis also hit your company you are looking to lower your costs by improving the scheduling of your taxis. 

You know in advance all the clients your taxis have to pick up for a day, and their locations and destinations. You have a number of taxis at your disposal, all located in the parking lot of the company. You want to get a list of actions each taxidriver has to do during the day. The goal is to make these as efficient as possible, so taxis do not waste too much fuel. You also suspect you hired too many employees, and using fewer taxis would reduce the costs as well. By the end of the day all the taxis have to return to the parking lot. 

You have a file with all the needed data (city.pl). This data consists of: 
 taxi(Id)
 customer(CustomerID, EarliestTimeOfPickup, LatestTimeOfPickup, NodeId, Destination)
 node(Id, X, Y)
 edge(FromNodeId, ToNodeId, Distance)

The city has a simple grid-like layout, and the parking lot is situated in the middle of the city. 

Customers specify a timespan in which they can be pickup up. The NodeId specifies the node on which they are waiting. Destination is another node-identifier that describes the customer his destination. 

Time is described by an integer between 0 and 1440 (there are 1440 minutes in one day). The distance is denoted by the time the taxi needs to travel that edge. All the taxis drive equally fast (and do not drive faster than they are allowed to).

Functional requirements
-----------------------
Basic Program
 The basic program returns, given the data above, a list of actions for each taxi. These actions ensure the following: 
 Each customer is picked up at the correct time.
 A taxi can only contain 4 customers.
 Each taxi starts and ends his day at the parking lot
 A day consists out of 1440 minutes, and a taxi has to be returned before the day is over.
Extensions
 Meeting the conditions for the basic program suffices to pass the practical part of the course. Students who want a higher grade can implement several extensions. Note that we just provide some ideas here, but you are free to come up with other ideas. 
 Add a human language interface that can be used to configure and query the system.
 Use the map of a real city, for example by using data from OpenStreetMap
 Make the traveltime of roads dynamic, for example due to increased traffic.
 Increase the difficulty of the program by either generating more customers, a larger city, etc.
 Allow customers to be added dynamicly.
 
Non-functional requirements
---------------------------
The program and the database have to be written in SWI-prolog and should run on the computer rooms of IG. For those of you that make their project at home and/or with a different prolog, make sure that your code is fully functional on the target system. Your code must work when run using SWI-prolog in the computer rooms! 

With respect to the program's design: the more modular, reusable and extensible your different program modules (search algorithm, constraints, etc.) are, the better. 

Make sure that your source files are standard unix text files, e.g. lines are separated by newline characters only. Comment your source code.

Deadline
--------
The firm deadline for this project is 13 January 2012 at midnight . The defense will follow in one of the following weeks. You should send me the following deliverables by email: 
 Your source code files: well structured and with the necessary documentation;
 A report in PDF that briefly describes your design and the functionality of your program: what are your major datastructures? how does your scheduling algorithm work? etc.
 A small manual and an example run of your program: the idea is that I should be able to test your program by myself, without having to contact you for additional information.
Everything must be sent by email to the following address any time before the deadline: jvallejo ][AT][ vub ][DOT][ ac ][DOT][ be.

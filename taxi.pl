% Contains taxi related functions
			
% Get a list of all the possible taxis
%	- Taxis = list of taxi ID's
getAllTaxis(Taxis):-
	findall(Taxi,
			(taxi(TaxiID),
			 Taxi = TaxiID),
			Taxis).
	
% Get list of all the taxijob-ID's
%	-Jobs = list of taixjobs-ID's
getAllTaxiJobs(Jobs):-
	findall(Job,
		(taxiJob(Taxi, _, _, _),
		 Job = Taxi),
		 Jobs).	

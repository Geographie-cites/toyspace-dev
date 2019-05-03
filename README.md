# toyspace

RAW DATA :

	+citiesShape (shapefile of cities to be mapped)
	+tabFlows (table of flow between cities, containing : origin, destination, transport mode and SPC)
	+roadNetwork (a network of road used to compute a distance matrix between cities)

	+railRoads (railRoad network to be mapped)
	+mainRoad (mainRoad network to be mapped)
	+station (train station to be mapped)



MAKING DATA :

	For each region :
		+popTab (sums of every flows, origin (population), destination (workers), internal (population))
		+popTabAgr (Aggregation of popTab for metropolis cities)
		+tabFlowsAgr (Aggregation of tabFlows for metropolis cities)
		+citiesShapeAgr (Aggregation of citiesShape for metropolis cities)
		+matFlow (flow Matrix)
		+coordCom (Cities Coordinate)
		+list potential (proportion of SPC)
		
		+candidate (1 | 0) (citiesShape$cand) :
			+station (railroadStation in cities (finger_city))
			+stationAndPole (employment pole and railroadStation in cities (tod_city & polycentric_city))
			+metro (metropolis in cities (cbd_city))
			+sub (subUrb and countryside in cities (cbd_city))
	
	Function : 
		+nystuen_dacey(){} (for macro flow tab)
		+excess_commuting(){}
		+mob_indic(){} (for index tab)
		+potential_palette(){} (for structure tab)
		+potential_contour(){} (for structure tab)
		+get_links(){} (for micro flow tab)
		
		
		
SCENARIOS :

	+Relocate populations and activities :
		+Finger plan City
			finger_city(pol,id,cand,tabFlows,idOri,idDes,idFlows){
				relocate_one(pol,id,cand){}
			}
		+Transport-oriented development City
			tod_city(pol,id,cand,tabFlows,idOri,idDes,idFlows){
				relocate_one(pol,id,cand){}
			}
		+Polycentric City
			polycentric_city(pol,id,cand,tabFlows,idOri,idDes,idFlows){
				relocate_one(pol,id,cand){}
			}
		+CBD-Model-City
			cbd_city(pol,id,cand,tabFlows,idOri,idDes,idFlows){
				routing(roadNetwork,pol,idpol){
					diagDist(){}
				}
				relocate_one(pol,id,cand){}
				proportionalSPC(){}
				proportionalTransport(){}
			}

	+Relocate equipment :
		+Near the residents
		+Near jobs
		+Balancing residents-jobs

	+Act on residential and professional mobility :
		+Job exchange
		+Housing exchange
		+Exchange without constraint

	+Act on the mode of transport :
		+Zero car
			reassignTransport(){}
		+All car
			reassignTransport(){}
		+Zero public transport
			reassignTransport(){}
		+All public transport
			reassignTransport(){}
		+Zero active mobility
			reassignTransport(){}
		+All active mobility



AMEGINAT-R :
	
	+Scenarios

	+Index
		+data:
		matFlow
		tabFlows
		citiesShape	
		railRoads
		mainRoad
		station
		
		+using:
		mob_indic(){}	
	
	+Structure
		+data:	
		list potential	
		citiesShape
		railRoads
		mainRoad
		station
		
		+using:
		potential_palette(){}
		potential_contour(){}
	
	+Micro Flow
		+data:
		tabFlows
		citiesShape	
		railRoads
		mainRoad
		station
		
		+using:
		get_links(){}
	
	+Macro Flow
		+data:
		popTab
		popTabAgr
		tabFlows
		tabFlowsAgr
		citiesShape
		citiesShapeAgr	
		railRoads
		mainRoad
		station
		
		+using:
		nystuen_dacey(){}

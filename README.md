# Play-By-Play Reader

## Huh?
For the 2019 NBA Hackathon one of the technical questions was to make a script to take play by play data and return the   
Offensive and Defensive Ratings for all players who played in the 2018 playoffs. The pdf can really explain it better than I could.

## Requirements
* R (I know, why did I not use python)
* A Play-by-Play which is formated the same as the Play by Play in the data folder (Play_by_Play.txt)
  * Basically files that are formatted exactly like the files in the data folder, I doubt you have some of those just lying around.

## How to Use 
Run the R Script after placing the play by play and game lineup document in `./data/`. A csv file called **Results.csv**   
will be created with the Offensive and Defensive Ratings of all players and what game those ratings were in. The  
Event_Codes.txt file shows how the "Event_Msg_Type" and "Action_Type" corelate to real life events. Just clone/fork the repo and run the R script.

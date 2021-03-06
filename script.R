# Import dplyr library for data wrangling
library(dplyr)

# Get the Data
game_lineup <-
  read.csv(
    './data/Game_Lineup.txt',
    sep = '\t',
    stringsAsFactors = FALSE
  )
play_by_play <-
  read.csv(
    './data/Play_by_Play.txt',
    sep = '\t',
    stringsAsFactors = FALSE
  )

# An array of all the game ids
all_games <- unique(play_by_play$Game_id)

# Function to add Possessions to both teams
# If the players were on offense on time of possessoin it is considered an offensive possession
# If the players were on defense on time of possession it is considered a defensive possession
add_poss <-
  function(one_quarter,
           player_id,
           team_id,
           quarter_lineup,
           add,
           row,
           event,
           quarter_play_by_play) {
    if (player_id != 0) {
      temp <-
        quarter_lineup$Team_id[quarter_lineup$Person_id == player_id]
      if (length(temp) > 0) {
        team_id <- temp
      }
    }
    one_quarter$Off_Poss[one_quarter$Person_id %in% quarter_lineup$Person_id &
                           one_quarter$Team_id == team_id] <-
      one_quarter$Off_Poss[one_quarter$Person_id %in% quarter_lineup$Person_id &
                             one_quarter$Team_id == team_id] + add
    one_quarter$Def_Poss[one_quarter$Person_id %in% quarter_lineup$Person_id &
                           one_quarter$Team_id != team_id] <-
      one_quarter$Def_Poss[one_quarter$Person_id %in% quarter_lineup$Person_id &
                             one_quarter$Team_id != team_id] + add
    return (one_quarter)
  }

# Function to add points
# Function adds points to team who scored as Team Points and team who gave up points as Opp Points
add_points <-
  function(one_quarter,
           player_id,
           quarter_lineup,
           option1,
           row,
           event,
           quarter_play_by_play) {
    team_id <-
      quarter_lineup$Team_id[quarter_lineup$Person_id == player_id]
    # Add Team Points
    one_quarter$Team_PTS[one_quarter$Person_id %in% quarter_lineup$Person_id &
                           one_quarter$Team_id == team_id] <-
      one_quarter$Team_PTS[one_quarter$Person_id %in% quarter_lineup$Person_id &
                             one_quarter$Team_id == team_id] + option1
    # Add Opponent Points
    one_quarter$Opp_PTS[one_quarter$Person_id %in% quarter_lineup$Person_id &
                          one_quarter$Team_id != team_id] <-
      one_quarter$Opp_PTS[one_quarter$Person_id %in% quarter_lineup$Person_id &
                            one_quarter$Team_id != team_id] + option1
    return (one_quarter)
  }

# Returns a dataframe with the 10 players (5 from each team) that are currently playing
subs <-
  function(quarter_play_by_play,
           subee,
           suber,
           game_lineup,
           quarter_lineup) {
    # Sub out players in game lineup dataset
    quarter_lineup$Person_id[quarter_lineup$Person_id == subee] <-
      suber
    return (quarter_lineup)
  }

# Final DataFrame to return
final_data <-
  data.frame(
    Game_ID = character(),
    Player_ID = character(),
    OffRtg = double(),
    DefRtg = double(),
    stringsAsFactors = FALSE
  )

# Loop Through Each Game in Game Array
for (i in 1:length(all_games)) {
  # Creates a dataframe for one game that calculates Team & Opponent Points and
  # Offensive & Defensive Possessions for all players in game
  one_game <- game_lineup %>%
    filter(Game_id == all_games[i] & Period == 0) %>%
    select(Game_id, Person_id, Team_id)
  one_game$Team_PTS <- 0
  one_game$Opp_PTS <- 0
  one_game$Off_Poss <- 0
  one_game$Def_Poss <- 0
  
  # Subsets the play by play dataframe to feature only plays from the current game
  one_game_play_by_play <- play_by_play %>%
    filter(Game_id == all_games[i]) %>%
    arrange(Period, desc(PC_Time), WC_Time, Event_Num)
  
  # Loop through every period
  for (j in 1:max(one_game_play_by_play$Period)) {
    # Subset dataframes futher to calculate everything by quarter
    
    # Play by play for the quarter
    quarter_play_by_play <- one_game_play_by_play %>%
      filter(Period == j)
    # Dataframe to store the 10 players on the court
    # Initial Values are the lineups from beginning of quarter
    quarter_lineup <- game_lineup %>%
      filter(Game_id == all_games[i] & Period == j) %>%
      select(Person_id, Team_id)
    # Identical Dataframe to one game but is used to calculate one quarter
    one_quarter <- game_lineup %>%
      filter(Game_id == all_games[i] & Period == 0) %>%
      select(Game_id, Person_id, Team_id)
    one_quarter$Team_PTS <- 0
    one_quarter$Opp_PTS <- 0
    one_quarter$Off_Poss <- 0
    one_quarter$Def_Poss <- 0
    
    # Vector holding 2 teams that play game
    teams <- unique(quarter_lineup$Team_id)
    
    # Skips lines only when substitution happens mid free throws
    skip <- 0
    
    # Loop through the quarter play by play, row by row/play by play
    for (row in 1:nrow(quarter_play_by_play)) {
      msg <- quarter_play_by_play[row, 'Event_Msg_Type']
      team_id <- quarter_play_by_play[row, 'Team_id']
      option1 <- quarter_play_by_play[row, 'Option1']
      action <- quarter_play_by_play[row, 'Action_Type']
      
      # All Action Codes for free throws that are the last free throw
      final_ft <- c(12, 15, 16, 17, 19, 20, 22, 26, 29)
      technical_ft <- c(16, 17, 19, 20, 22, 26, 29)
      if (skip == 0) {
        # If a player made a shot
        if (msg == 1) {
          # Add points for correct team
          one_quarter <-
            add_points(
              one_quarter,
              quarter_play_by_play[row, 'Person1'],
              quarter_lineup,
              option1,
              row,
              msg,
              quarter_play_by_play
            )
          one_quarter <-
            add_poss(
              one_quarter,
              quarter_play_by_play[row, 'Person1'],
              0,
              quarter_lineup,
              1,
              row,
              msg,
              quarter_play_by_play
            )
          # If the shot was missed
        } else if (msg == 2) {
          count <- 0
          # Loops until finds the rebound associated with the shot in order to find
          # if the rebound would be offensive or defensive
          while (quarter_play_by_play[row + count, 'Event_Msg_Type'] != 4) {
            count <- count + 1
          }
          # Gets the player ID of who rebounded the shot
          pid <- quarter_play_by_play[row + count, 'Person1']
          # Gets team ID of the player who took the shot
          tid <-
            quarter_lineup$Team_id[quarter_lineup$Person_id == pid]
          #
          if (length(tid) != 0) {
            if (tid != quarter_play_by_play[row, 'Team_id']) {
              one_quarter <-
                add_poss(
                  one_quarter,
                  0,
                  team_id,
                  quarter_lineup,
                  1,
                  row,
                  msg,
                  quarter_play_by_play
                )
            }
            #
          } else {
            count <- 2
            while (quarter_play_by_play[row + count, 'Event_Msg_Type'] == 8) {
              count <- count + 1
            }
            if (quarter_play_by_play[row, 'Team_id'] != quarter_play_by_play[row + count, 'Team_id']) {
              one_quarter <-
                add_poss(
                  one_quarter,
                  0,
                  team_id,
                  quarter_lineup,
                  1,
                  row,
                  msg,
                  quarter_play_by_play
                )
            }
          }
          # If there was a free throw attempt
        } else if (msg == 3 &&
                   quarter_play_by_play[row, 'Action_Type'] == 10) {
          if (quarter_play_by_play[row - 1, 'Event_Msg_Type'] != 8) {
            if (quarter_play_by_play[row, 'Option1'] == 1) {
              one_quarter <-
                add_points(
                  one_quarter,
                  quarter_play_by_play[row, 'Person1'],
                  quarter_lineup,
                  option1,
                  row,
                  msg,
                  quarter_play_by_play
                )
            } else {
              one_quarter <-
                add_poss(
                  one_quarter,
                  quarter_play_by_play[row, 'Person1'],
                  0,
                  quarter_lineup,
                  -1,
                  row,
                  msg,
                  quarter_play_by_play
                )
            }
          } else {
            count <- 1
            while (quarter_play_by_play[row - count, 'Event_Msg_Type'] == 8) {
              count <- count + 1
            }
            for (player in 1:(count - 1)) {
              quarter_lineup <-
                subs(
                  quarter_play_by_play,
                  quarter_play_by_play[row - player, 'Person2'],
                  quarter_play_by_play[row - player, 'Person1'],
                  game_lineup,
                  quarter_lineup
                )
            }
            if (quarter_play_by_play[row, 'Option1'] == 1) {
              one_quarter <-
                add_points(
                  one_quarter,
                  quarter_play_by_play[row, 'Person1'],
                  quarter_lineup,
                  option1,
                  row,
                  msg,
                  quarter_play_by_play
                )
            } else {
              one_quarter <-
                add_poss(
                  one_quarter,
                  quarter_play_by_play[row, 'Person1'],
                  0,
                  quarter_lineup,
                  -1,
                  row,
                  msg,
                  quarter_play_by_play
                )
            }
            for (player in seq(count - 1, 1,-1)) {
              quarter_lineup <-
                subs(
                  quarter_play_by_play,
                  quarter_play_by_play[row - player, 'Person1'],
                  quarter_play_by_play[row - player, 'Person2'],
                  game_lineup,
                  quarter_lineup
                )
            }
            
          }
        } else if (msg == 3 &&
                   quarter_play_by_play[row, 'Action_Type'] != 10) {
          # Find foul associated with shot to find all subs
          foul <- 1
          while (quarter_play_by_play[row - foul, 'Event_Msg_Type'] != 6) {
            foul <- foul + 1
          }
          # Reverse subs so points happen to right people
          for (hi in 1:(foul - 1)) {
            if (quarter_play_by_play[row - hi, 'Event_Msg_Type'] == 8) {
              quarter_lineup <-
                subs(
                  quarter_play_by_play,
                  quarter_play_by_play[row - hi, 'Person2'],
                  quarter_play_by_play[row - hi, 'Person1'],
                  game_lineup,
                  quarter_lineup
                )
            }
          }
          # Free throw was made
          if (option1 == 1) {
            # Record each free throw that was made (1 means made anything else is missed)
            one_quarter <-
              add_points(
                one_quarter,
                quarter_play_by_play[row, 'Person1'],
                quarter_lineup,
                1,
                row,
                msg,
                quarter_play_by_play
              )
          }
          
          count <- 0
          # Loop until finds the final free throw in sequence.
          # Adds count for each line that is not the final free throw.
          # In order to calculate points for players who were on the floor at the time of first
          # free throw, free throws are all counted first then anything in between will be taken
          # into account.
          while ((!(quarter_play_by_play[row + count, 'Action_Type'] %in% final_ft) ||
                  (quarter_play_by_play[row + count, 'Event_Msg_Type'] != 3)) &&
                 !(action %in% final_ft)) {
            # If there is another free throw that is not the final one add if it was made
            if (quarter_play_by_play[row + count + 1, 'Event_Msg_Type'] == 3) {
              # Add free throw
              if (quarter_play_by_play[row + count + 1, 'Option1'] == 1) {
                # Record each free throw that was made (1 means made anything else is missed)
                one_quarter <-
                  add_points(
                    one_quarter,
                    quarter_play_by_play[row, 'Person1'],
                    quarter_lineup,
                    1,
                    row,
                    msg,
                    quarter_play_by_play
                  )
              }
            }
            # Increment count for each line that was not the final free throw
            count <- count + 1
          }
          # Add possession if last free throw was made
          if (!(quarter_play_by_play[row + count, 'Action_Type'] %in% technical_ft)) {
            if (quarter_play_by_play[row + count, 'Option1'] == 1) {
              one_quarter <-
                add_poss(
                  one_quarter,
                  quarter_play_by_play[row, 'Person1'],
                  0,
                  quarter_lineup,
                  1,
                  row,
                  msg,
                  quarter_play_by_play
                )
            } else {
              temp1 <- 1
              while (quarter_play_by_play[row + count + temp1, 'Event_Msg_Type'] != 4) {
                temp1 <- temp1 + 1
              }
              # Gets the player ID of who rebounded the shot
              pid <-
                quarter_play_by_play[row + count + temp1, 'Person1']
              # Gets team ID of the player who took the shot
              tid <-
                quarter_lineup$Team_id[quarter_lineup$Person_id == pid]
              if (length(tid) != 0) {
                if (tid != quarter_play_by_play[row, 'Team_id']) {
                  one_quarter <-
                    add_poss(
                      one_quarter,
                      0,
                      team_id,
                      quarter_lineup,
                      1,
                      row,
                      msg,
                      quarter_play_by_play
                    )
                }
              } else {
                temp <- 1
                while (quarter_play_by_play[row + count + temp, 'Event_Msg_Type'] == 8) {
                  temp <- temp + 1
                }
                if (quarter_play_by_play[row, 'Team_id'] != quarter_play_by_play[row + count + temp, 'Team_id']) {
                  one_quarter <-
                    add_poss(
                      one_quarter,
                      0,
                      team_id,
                      quarter_lineup,
                      1,
                      row,
                      msg,
                      quarter_play_by_play
                    )
                }
              }
            }
          }
          # Put back pre frethrow subs
          for (hi in seq(foul - 1, 0,-1)) {
            if (quarter_play_by_play[row - hi, 'Event_Msg_Type'] == 8) {
              quarter_lineup <-
                subs(
                  quarter_play_by_play,
                  quarter_play_by_play[row - hi, 'Person1'],
                  quarter_play_by_play[row - hi, 'Person2'],
                  game_lineup,
                  quarter_lineup
                )
            }
          }
          # Loop through all the lines that were in between Free Throws.
          # Only subs can happen in between free throws therefore skips any thing else (Timeout, Stoppage, etc)
          for (k in 0:count) {
            # Update who is on the floor with subs
            # Even if subs were substituted mid free throws they are only updated after the,
            if (quarter_play_by_play[row + k, 'Event_Msg_Type'] == 8) {
              quarter_lineup <-
                subs(
                  quarter_play_by_play,
                  quarter_play_by_play[row + k, 'Person1'],
                  quarter_play_by_play[row + k, 'Person2'],
                  game_lineup,
                  quarter_lineup
                )
            }
          }
          # Set skip to count in order to skip the lines in between free throws as they were
          # already checked
          skip <- count
          # If there was a turnover
        } else if (msg == 5) {
          # Add Possessions to Every Player except if Action code was 0 indicating no turnover
          if (action != 0) {
            one_quarter <-
              add_poss(
                one_quarter,
                quarter_play_by_play[row, 'Person1'],
                team_id,
                quarter_lineup,
                1,
                row,
                msg,
                quarter_play_by_play
              )
          }
          # End of Period
        } else if (msg == 13) {
          # Period ended, add possession for everyone
          one_quarter <-
            add_poss(one_quarter,
                     0,
                     team_id,
                     quarter_lineup,
                     1,
                     row,
                     msg,
                     quarter_play_by_play)
          # If there is a sub
        } else if (msg == 8) {
          # Use sub function to sub out players
          quarter_lineup <-
            subs(
              quarter_play_by_play,
              quarter_play_by_play[row, 'Person1'],
              quarter_play_by_play[row, 'Person2'],
              game_lineup,
              quarter_lineup
            )
        }
        # If Skip != 0, lines need to be skipped in order to not double count free throws
      } else {
        # Decrement Skip
        skip <- skip - 1
      }
    }
    # Add each quarter's calculations to the one game dataframe
    one_game$Team_PTS <- one_game$Team_PTS + one_quarter$Team_PTS
    one_game$Opp_PTS <- one_game$Opp_PTS + one_quarter$Opp_PTS
    one_game$Off_Poss <- one_game$Off_Poss + one_quarter$Off_Poss
    one_game$Def_Poss <- one_game$Def_Poss + one_quarter$Def_Poss
  }
  # Add Offensive Rating and Defensive Rating to the Game Dataframe
  one_game <- one_game %>%
    mutate(OffRtg = round(((
      Team_PTS / Off_Poss
    ) * 100), 1),
    DefRtg = round(((Opp_PTS / Def_Poss) * 100), 1))
  # Add the game with all players from the game to the final csv file with only
  # Game ID, Player ID, Offensive Rating, and Defensive Rating
  final_data <-
    rbind(
      final_data,
      data.frame(
        "Game_ID" = one_game$Game_id,
        "Player_ID" = one_game$Person_id,
        "OffRtg" = one_game$OffRtg,
        "DefRtg" = one_game$DefRtg
      )
    )
}

write.csv(final_data, 'Results.csv', row.names = FALSE)
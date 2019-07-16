library(dplyr)

# Get the Data
game_lineup <-
  read.csv('./data/Game_Lineup.txt',
           sep = '\t',
           stringsAsFactors = FALSE)
play_by_play <-
  read.csv('./data/Play_by_Play.txt',
           sep = '\t',
           stringsAsFactors = FALSE)
#event_codes <-
#  read.csv(
#    './data/Event_Codes.txt',
#    sep = '\t',
#    stringsAsFactors = FALSE
#  )

all_games <- unique(play_by_play$Game_id)

add_poss <- function(one_quarter, team_id, quarter_lineup) {
#  one_quarter$Tot_Poss[one_quarter$Person_id %in% quarter_lineup$Person_id] <-
#    one_quarter$Tot_Poss[one_quarter$Person_id %in% quarter_lineup$Person_id] + 1
  one_quarter$Off_Poss[one_quarter$Person_id %in% quarter_lineup$Person_id &
                         one_quarter$Team_id == team_id] <- 
    one_quarter$Off_Poss[one_quarter$Person_id %in% quarter_lineup$Person_id &
                           one_quarter$Team_id == team_id] + 1
  one_quarter$Def_Poss[one_quarter$Person_id %in% quarter_lineup$Person_id &
                         one_quarter$Team_id != team_id] <- 
    one_quarter$Def_Poss[one_quarter$Person_id %in% quarter_lineup$Person_id &
                           one_quarter$Team_id != team_id] + 1
  return (one_quarter)
}

add_points <-
  function(one_quarter,
           team_id,
           quarter_lineup,
           option1) {
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

subs <-
  function(quarter_play_by_play,
           row,
           game_lineup,
           quarter_lineup) {
    # Sub out players in game lineup dataset
    subee <- quarter_play_by_play[row, 'Person1']
    suber <- quarter_play_by_play[row, 'Person2']
    quarter_lineup$Person_id[quarter_lineup$Person_id == subee] <-
      suber
    return (quarter_lineup)
  }

final_data <-
  data.frame(
    Game_ID = character(),
    Player_ID = character(),
    OffRtg = double(),
    DefRtg = double(),
    stringsAsFactors = FALSE
  )

for (i in 1:length(all_games)) {
  one_game <- game_lineup %>%
    filter(Game_id == all_games[i] & Period == 0) %>%
    select(Game_id, Person_id, Team_id)
  one_game$Team_PTS <- 0
  one_game$Opp_PTS <- 0
  one_game$Off_Poss <- 0
  one_game$Def_Poss <- 0
  
  one_game_play_by_play <- play_by_play %>%
    filter(Game_id == all_games[i]) %>%
    arrange(Period, desc(PC_Time), WC_Time, Event_Num)
  
  for (j in 2:2){#1:max(one_game_play_by_play$Period)) {
    quarter_play_by_play <- one_game_play_by_play %>%
      filter(Period == j)
    quarter_lineup <- game_lineup %>%
      filter(Game_id == all_games[i] & Period == j) %>%
      select(Person_id, Team_id)
    one_quarter <- game_lineup %>%
      filter(Game_id == all_games[i] & Period == 0) %>%
      select(Game_id, Person_id, Team_id)
    one_quarter$Team_PTS <- 0
    one_quarter$Opp_PTS <- 0
    one_quarter$Off_Poss <- 0
    one_quarter$Def_Poss <- 0
    teams <- unique(quarter_lineup$Team_id)
    
    # Skips lines only when substitution happens mid free throws
    skip <- 0
    
    for (row in 1:nrow(quarter_play_by_play)) {
      msg <- quarter_play_by_play[row, 'Event_Msg_Type']
      team_id <- quarter_play_by_play[row, 'Team_id']
      option1 <- quarter_play_by_play[row, 'Option1']
      action <- quarter_play_by_play[row, 'Action_Type']
      
      final_ft <- c(10, 12, 15, 16, 17, 19, 20, 22, 26, 29)
      if (skip == 0) {
        if (msg == 1) {
          # Add points for correct team
          one_quarter <-
            add_points(one_quarter, team_id, quarter_lineup, option1)
          # Add possession
          one_quarter <- add_poss(one_quarter, team_id, quarter_lineup)
        } else if (msg == 2) {
          # Find if FG Rebound was Defensive, then add Possession if is
          # Find if FT Rebound was Defensive, then add Possession if is
          count <- 0
          while (quarter_play_by_play[row + count, 'Event_Msg_Type'] != 4) {
            count <- count + 1
          }
          pid <- quarter_play_by_play[row + count, 'Person1']
          tid <-
            quarter_lineup$Team_id[quarter_lineup$Person_id == pid]
          if (length(tid) != 0) {
            if (tid != quarter_play_by_play[row, 'Team_id']) {
              one_quarter <- add_poss(one_quarter, team_id, quarter_lineup)
            }
          }
        } else if (msg == 3) {
          if (option1 == 1) {
            # Record each free throw that was made (1 means made anything else is missed)
            one_quarter <-
              add_points(one_quarter, team_id, quarter_lineup, 1)
          }
          count <- 0
          while ((!(quarter_play_by_play[row + count, 'Action_Type'] %in% final_ft) ||
                  (quarter_play_by_play[row + count, 'Event_Msg_Type'] != 3)) &&
                 !(action %in% final_ft)) {
            if (quarter_play_by_play[row + count + 1, 'Event_Msg_Type'] == 3) {
              if (quarter_play_by_play[row + count + 1, 'Option1'] == 1) {
                # Record each free throw that was made (1 means made anything else is missed)
                one_quarter <-
                  add_points(one_quarter, team_id, quarter_lineup, 1)
              }
            }
            count <- count + 1
          }
          # Add possession if last free throw was made
          if (quarter_play_by_play[row + count, 'Option1'] == 1 &&
              quarter_play_by_play[row - 1, 'Event_Msg_Type'] != 1) {
            one_quarter <- add_poss(one_quarter, team_id, quarter_lineup)
          }
          for (k in 0:count) {
            if (quarter_play_by_play[row + k, 'Event_Msg_Type'] == 8) {
              quarter_lineup <-
                subs(quarter_play_by_play,
                     row + k,
                     game_lineup,
                     quarter_lineup)
            }
          }
          skip <- count
        } else if (msg == 5) {
          # Add Possessions to Every Player
          if (action != 0) {
            one_quarter <- add_poss(one_quarter, team_id, quarter_lineup)
          }
        } else if (msg == 13) {
          # Period ended, add possession for everyone
          one_quarter <- add_poss(one_quarter, teams[1], quarter_lineup)
          one_quarter <- add_poss(one_quarter, teams[2], quarter_lineup)
        } else if (msg == 8) {
          quarter_lineup <-
            subs(quarter_play_by_play,
                 row,
                 game_lineup,
                 quarter_lineup)
        }
      } else {
        skip <- skip - 1
      }
    }
    #View(one_quarter)
    one_game$Team_PTS <- one_game$Team_PTS + one_quarter$Team_PTS
    one_game$Opp_PTS <- one_game$Opp_PTS + one_quarter$Opp_PTS
    one_game$Off_Poss <- one_game$Off_Poss + one_quarter$Off_Poss
    one_game$Def_Poss <- one_game$Def_Poss + one_quarter$Def_Poss
  }
  one_game <- one_game %>%
    mutate(OffRtg = round(((Team_PTS / Off_Poss) * 100), 1),
           DefRtg = round(((Opp_PTS / Def_Poss) * 100), 1))
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
final_data <- final_data %>% tidyr::drop_na()
#final_data$DefRtg <-
#  ifelse(is.nan(final_data$DefRtg), NA, final_data$DefRtg)
  
write.csv(final_data,'./Answer.csv', row.names=FALSE)
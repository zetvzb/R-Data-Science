#2019 March Madness Prediction Model

##############################################################################################################################
#------------------------------------------------------ Data Load -----------------------------------------------------------#
#PreProcessing Detailed Datasets from Kaggle Competition as Sponsored by Google
#Create a row that relates to each Team with detailed season stats
##############################################################################################################################
rm(list=ls())
#Team Statistics Datasets
reg_season_stats <- read.csv("datafiles/RegularSeasonDetailedResults.csv", stringsAsFactors = FALSE)
tourney_stats <- read.csv("datafiles/NCAATourneyDetailedResults.csv", stringsAsFactors = FALSE)
teams <- read.csv("datafiles/Teams.csv", stringsAsFactors = FALSE)

#Bracketology Datasets
tourney_compact_df <- read.csv("datafiles/NCAATourneyCompactResults.csv", stringsAsFactors = FALSE)
tourney_detailed_df <- read.csv("datafiles/NCAATourneyDetailedResults.csv", stringsAsFactors = FALSE)
tourney_seedRoundSlots_df <- read.csv("datafiles/NCAATourneySeedRoundSlots.csv", stringsAsFactors = FALSE)
tourney_seeds <- read.csv("datafiles/NCAATourneySeeds.csv", stringsAsFactors = FALSE)
###############################################################################################################################
#---------------------------------------------------- Setup Libraries --------------------------------------------------------#
###############################################################################################################################
library(tidyverse)
library(gridExtra)
library(knitr)
library(magrittr)
library(XML)
library(RCurl)
library(dplyr)
library(data.table)
library(stringr)
library(randomForest)
library(tidyr)
################################################################################################################################
#Calculate Possessions Per Game Statistics

# regular season
reg_season_stats <- reg_season_stats %>%
  mutate(WPoss = WFGA + (WFTA * 0.475) + WTO - WOR,
         LPoss = LFGA + (LFTA * 0.475) + LTO - LOR)

# Tourney
tourney_stats <- tourney_stats %>%
  mutate(WPoss = WFGA + (WFTA * 0.475) + WTO - WOR,
         LPoss = LFGA + (LFTA * 0.475) + LTO - LOR)

#################################################################################################################################
#----------------------------------------------------------- Feature Engineering -----------------------------------------------#
# Calculate TEAM season TOTALS                                                                                                  #
# Methodology inspired from Max Phillips Kernel on Kaggle                                                                       #
# Transforms Individual Game data for each Team to Season Totals. Creates a Tidy Dataset                                        #
                                                                                                                                #
# First I will define a function that will take the detailed results data and reshape it so that it results in a dataframe where# 
# each observation is a team for that season and their totals statistics for that season. It will also calculate the statistics #
# allowed for the season. This function only requires pass regular season stats and references a Teams Dataset.                 #
#################################################################################################################################
reshape_detailed_results <- function(detailed_dataset) {
  
  season_team_stats_tot <- rbind(
    detailed_dataset %>%
      select(Season, DayNum, TeamID=WTeamID, Score=WScore, OScore=LScore, WLoc, NumOT, Poss=WPoss, FGM=WFGM, FGA=WFGA, FGM3=WFGM3,  FGA3=WFGA3, FTM=WFTM, FTA=WFTA, OR=WOR,
             DR=WDR, Ast=WAst, TO=WTO, Stl=WStl, Blk=WBlk, PF=WPF, OPoss=LPoss, OFGM=LFGM, OFGA=LFGA, OFGM3=LFGM3, OFGA3=LFGA3, OFTM=LFTM, OFTA=LFTA, O_OR=LOR, ODR=LDR,
             OAst=LAst, OTO=LTO, OStl=LStl, OBlk=LBlk, OPF=LPF) %>%
      mutate(Winner=1),
    
    detailed_dataset %>%
      select(Season, DayNum, TeamID=LTeamID, Score=LScore, OScore=WScore, WLoc, NumOT, Poss=LPoss, FGM=LFGM, FGA=LFGA, FGM3=LFGM3,  FGA3=LFGA3, FTM=LFTM, FTA=LFTA, OR=LOR,
             DR=LDR, Ast=LAst, TO=LTO, Stl=LStl, Blk=LBlk, PF=LPF, OPoss=WPoss, OFGM=WFGM, OFGA=WFGA, OFGM3=WFGM3, OFGA3=WFGA3, OFTM=WFTM, OFTA=WFTA, O_OR=WOR, ODR=WDR,
             OAst=WAst, OTO=WTO, OStl=WStl, OBlk=WBlk, OPF=WPF) %>%
      mutate(Winner=0)) %>%
    left_join(teams, by= "TeamID") %>%
    group_by(Season, TeamID, TeamName) %>%
    summarise(GP = n(),
              wins = sum(Winner),
              TotPoints = sum(Score),
              TotPointsAllow = sum(OScore),
              NumOT = sum(NumOT),
              TotPos = sum(Poss),
              TotFGM = sum(FGM),
              TotFGA = sum(FGA),
              TotFGM3 = sum(FGM3),
              TotFGA3 = sum(FGA3),
              TotFTM = sum(FTM),
              TotFTA = sum(FTA),
              TotOR = sum(OR),
              TotDR = sum(DR),
              TotAst = sum(Ast),
              TotTO = sum(TO),
              TotStl = sum(Stl),
              TotBlk = sum(Blk),
              TotPF = sum(PF),
              TotPossAllow = sum(OPoss),
              TotFGMAllow = sum(OFGM),
              TotFGAAllow = sum(OFGA),
              TotFGM3Allow = sum(OFGM3),
              TotFGA3Allow = sum(OFGA3),
              TotFTMAllow = sum(OFTM),
              TotFTAAllow = sum(OFTA),
              TotORAllow = sum(O_OR),
              TotDRAllow = sum(ODR),
              TotAstAllow = sum(OAst),
              TotTOAllow = sum(OTO),
              TotStlAllow = sum(OStl),
              TotBlkAllow = sum(OBlk),
              TotPFAllow = sum(OPF)) %>% ungroup()
  
}

#Store Results to Dataframe from Function 
#Takes out Day Feature and Aggregates at Season Level
season_team_stats_tot <- reshape_detailed_results(reg_season_stats)

#Add in Team Winning Percentage as a feature 
season_team_stats_tot$WinPerc <- season_team_stats_tot$wins / season_team_stats_tot$GP

##################################################################################################################################
#------------------------- Create a dataframe of season averages for each team --------------------------------------------------#
#Now Create dataset that does the same process as above, but as team game averages instead of totals.                            #
# Next I will define a function that takes in the totals dataframe from the above function and calculate a season averages dataset.
# this function also only requires one parameter to be passed to it, the totals dataframe.                                       #
##################################################################################################################################
calculate_detailed_averages <- function(totals_dataframe) {
  
  averages <- totals_dataframe
  
  cols <- names(averages[,c(6:36)])
  
  for (eachcol in cols) {
    averages[,eachcol] <- round(averages[,eachcol] / averages$GP,2)
    
  }
  
  averages <- averages %>%
    rename(AvgPoints = TotPoints, AvgPointsAllow=TotPointsAllow, AvgOT=NumOT, AvgPoss=TotPos, AvgFGM=TotFGM,  AvgFGA=TotFGA, AvgFGM3=TotFGM3, AvgFGA3=TotFGA3, AvgFTM=TotFTM,
           AvgFTA=TotFTA, AvgOR=TotOR, Avg_DR=TotDR, AvgAst=TotAst, AvgTO=TotTO, AvgStl=TotStl, Avg_Blk=TotBlk, AvgPF=TotPF, AvgPossAllow=TotPossAllow, AvgFGMAllow=TotFGMAllow, 
           AvgFGAAllow=TotFGAAllow,  AvgFGM3Allow=TotFGM3Allow, AvgFGA3Allow=TotFGA3Allow,  AvgFTMAllow=TotFTMAllow, AvgFTAAllow=TotFTAAllow, 
           AvgORAllow=TotORAllow, AvgDRAllow=TotDRAllow,  AvgAstAllow=TotAstAllow, AvgTOAllow=TotTOAllow, AvgStlAllow=TotStlAllow,  AvgBlkAllow=TotBlkAllow,
           AvgPFAllow=TotPFAllow) %>%
    mutate(PointsPerPoss = AvgPoints / AvgPoss,
           PointsPerPossAllow = AvgPointsAllow / AvgPossAllow)
  
  return(averages)
  
}

#Store Results to Dataframe from Function 
season_team_stats_averages <- calculate_detailed_averages(season_team_stats_tot)

##################################################################################################################################
#------------------------------------------------- Advanced Stats ---------------------------------------------------------------#
#In this section, I will analyse the impact of advanced stats on a teams performance.                                            #
#The first subsection will deal with per-game advanced stats, while the second will                                              # 
#look at season-aggregated advanced stats.                                                                                       #
                                                                                                                                 #
#As a lot of you are probably aware, since the popularisation of sabermetrics and integration of                                 #
#advanced analytics into other sports, there are better ways of measuring a team's expected success.                             #
#These per game advanced analytics are displayed below. These metrics all show a considerable difference                         #  
#when comparing wins and losses in regular season play between 2003 and 2018.                                                    #
                                                                                                                                 #    
#The calculations of the displayed advanced stats are as follows:                                                                #
#Possessions = 0.96 x (FGA + Turnovers + (0.475 x FTA) - Offensive Rebounds                                                      #
#Offensive Rating = 100 x (Score / Possessions)                                                                                  #
#Defensive Rating =  100 x (Opponent's Score / Possessions)                                                                      #
#Strength of Schedule = 100 x (Offensive Rating - Defensive Rating)                                                              #  
#Performance Impact Estimator = Score + FGM + FTM - FGA - FTA + Defensive Rebounds + (0.5 * Offensive Rebounds) + Assists +      #
#Steals + (0.5 * Blocks) - PF - Turnovers                                                                                        #  
#Team Impact Estimator = PIE / (PIE + Opponent's PIE)                                                                            #
#Assist ratio  = 100 * Assists / (FGA + (0.475 * FTA) + Asistst + Turnovers))                                                    #
#Turnover ratio = 100 * Turnovers / (FGA + (0.475 * FTA) + Assists + Turnovers)                                                  #
#True Shooting% = 100 * Team Points / (2 * (FGA + (0.475 * FTA)))                                                                #
#Effective FG% = (FGM + 0.5 * Threes Made) / FGA                                                                                 #  
#Free Throw rate = FTA / FGA                                                                                                     #  
#Offensive Rebound = Offensive Rebounds / (Offensive Rebounds + Opponent's Defensive Rebounds)                                   #   
#Defensive Rebound = Defensive Rebounds / (Defensive Rebounds + Opponent's Offensive Rebounds)                                   #
#Total Rebound = (Defensive Rebounds + Offensive Rebounds) / (Defensive Rebounds + Offensive Rebounds +                          #
#Opponent's Defensive Rebounds + Opponent's Offensive Rebounds)                                                                  #
##################################################################################################################################
                                                                      
advanced_stats_per_game <- rbind(
  reg_season_stats %>%
    select(Season, DayNum, TeamID=WTeamID, Score=WScore, OScore=LScore, Poss=WPoss, OPoss=LPoss, WLoc, NumOT, FGM=WFGM, FGA=WFGA, FGM3=WFGM3,  FGA3=WFGA3, FTM=WFTM, FTA=WFTA, OR=WOR, DR=WDR, Ast=WAst, TO=WTO, Stl=WStl, Blk=WBlk, PF=WPF, OFGM=LFGM, OFGA=LFGA, OFGM3=LFGM3, OFGA3=LFGA3, OFTM=LFTM, OFTA=LFTA, O_OR=LOR, ODR=LDR, OAst=LAst, OTO=LTO, OStl=LStl, OBlk=LBlk, OPF=LPF) %>%
    mutate(Winner=1),
  
  reg_season_stats %>%
    select(Season, DayNum, TeamID=LTeamID, Score=LScore, OScore=WScore, Poss=LPoss, OPoss=WPoss, WLoc, NumOT, FGM=LFGM, FGA=LFGA, FGM3=LFGM3,  FGA3=LFGA3, FTM=LFTM, FTA=LFTA, OR=LOR, DR=LDR, Ast=LAst, TO=LTO, Stl=LStl, Blk=LBlk, PF=LPF, OFGM=WFGM, OFGA=WFGA, OFGM3=WFGM3, OFGA3=WFGA3, OFTM=WFTM, OFTA=WFTA, O_OR=WOR, ODR=WDR, OAst=WAst, OTO=WTO, OStl=WStl, OBlk=WBlk, OPF=WPF) %>%
    mutate(Winner=0)) %>%
  mutate(PtsDiff = Score - OScore)


advanced_stats_per_game <- advanced_stats_per_game %>%
  mutate(OffRtg = 100 * (Score / Poss),
         DefRtg = 100 * (OScore / OPoss),
         SoS = OffRtg - DefRtg,
         Pie = Score + FGM + FTM - FGA - FTA + DR + (0.5 * OR) + Ast + Stl + (0.5 * Blk) - PF - TO,
         OPie = OScore + OFGM + OFTM - OFGA - OFTA + ODR + (0.5 * O_OR) + OAst + OStl + (0.5 * OBlk) - OPF - OTO,
         Tie = Pie / (Pie + OPie) * 100,
         AstRatio = 100 * Ast / (FGA + (0.475 * FTA) + Ast + TO),
         TORatio = 100 * TO / (FGA + (0.475 * FTA) + Ast + TO),
         TSPerc = 100 * Score / (2 * (FGA + (0.475 * FTA))),
         EFGPerc = 100* (FGM + 0.5 * FGM3) / FGA,
         FTRate = FTA / FGA,
         OffRebPerc = OR / (OR + ODR),
         DefRebPerc = DR / (DR + O_OR),
         TotRebPerc = (DR + OR) / (DR + OR + ODR + O_OR)) %>%
  select(Season, DayNum, TeamID, Score, OScore, WLoc, NumOT, Winner, Poss, OffRtg, DefRtg, SoS, Pie, OPie, Tie, AstRatio, TORatio, TSPerc, EFGPerc, FTRate, OffRebPerc, DefRebPerc, TotRebPerc)

#Now Aggregate Advanced Statistics By Season
advanced_stats_season <- season_team_stats_tot %>%
  mutate(WinPerc = wins / GP,
         OffRtg = 100 * (TotPoints / TotPos),
         DefRtg = 100 * (TotPointsAllow / TotPossAllow),
         SoS = OffRtg - DefRtg,
         Pie = TotPoints + TotFGM + TotFTM - TotFGA - TotFTA + TotDR + (0.5 * TotOR) + TotAst + TotStl + (0.5 * TotBlk) - TotPF - TotTO,
         OPie = TotPointsAllow + TotFGMAllow + TotFTMAllow - TotFGAAllow - TotFTAAllow + TotDRAllow + (0.5 * TotORAllow) + TotAstAllow + TotStlAllow + (0.5 * TotBlkAllow) - TotPFAllow - TotTOAllow,
         Tie = Pie / (Pie + OPie) * 100,
         AstRatio = 100 * TotAst / (TotFGA + (0.475 * TotFTA) + TotAst + TotTO),
         TORatio = 100 * TotTO / (TotFGA + (0.475 * TotFTA) + TotAst + TotTO),
         TSPerc = 100 * TotPoints / (2 * (TotFGA + (0.475 * TotFTA))),
         FTRate = TotFTA / TotFGA,
         ThreesShare = TotFGA3 / TotFGA,
         OffRebPerc = TotOR / (TotOR + TotDRAllow),
         DefRebPerc = TotDR / (TotDR + TotORAllow),
         TotRebPerc = (TotDR + TotOR) / (TotDR + TotOR + TotDRAllow + TotORAllow)) %>%
  select(Season, TeamID, TotPoints, TotPointsAllow, OffRtg, DefRtg, SoS, Pie, OPie, Tie, AstRatio, TORatio, TSPerc, FTRate, ThreesShare, OffRebPerc, DefRebPerc, TotRebPerc) %>%
  left_join(teams %>% select(TeamID, TeamName), by = "TeamID")

################################################################################################################################################
#--------------------------------------------------------- Massey Ordinals Rankings -----------------------------------------------------------#
#Using Massey Ordinals Data to use Rankings as a feature in the model.                                                                         #
#Specifically chose Ken Pomeroy and Jeff Sagarin Rankings as they are the most famous.                                                         #
#Select the last rankings of each season for each team. Do not load entire Massey Ordinals dataset, it is a billion records.                   #
                                                                                                                                               #
## Rankings and winning percentages?                                                                                                           #
#As can be seen, the better Pomeroy ranked team won on average 71.9% of the time in tourney games during 2003-2018.                            #
#The 2008 season saw the highest percentage of better ranked teams winning at 81% of games,                                                    #
#while 2014 was the worst seasons for better ranked teams, with them winning only 65% of the time.                                             #
                                                                                                                                               #
#Sagarin rankings were fairly similar, with an average performance of 71.6%, just below Pomeroy's rankings.                                    #
################################################################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pomeroy Rankings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
massey_pom_end <- read.csv("datafiles/MasseyOrdinals_thru_2019_day_128.csv", stringsAsFactors = FALSE) %>% # read in data
  filter(SystemName == "POM") %>% # filter out only Pomeroy rankings
  group_by(Season, TeamID) %>%
  filter(RankingDayNum == max(RankingDayNum)) %>% ungroup() %>% # select the latest date of the season
  select(Season, TeamID, OrdinalRank)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Now Sagarin Rankings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
massey_sagarin_end <- read.csv("datafiles/MasseyOrdinals_thru_2019_day_128.csv", stringsAsFactors = FALSE) %>% # read in data
  filter(SystemName == "SAG") %>% # filter out only Sagarin rankings
  group_by(Season, TeamID) %>%
  filter(RankingDayNum == max(RankingDayNum)) %>% ungroup() %>% # select the latest date of the season
  select(Season, TeamID, OrdinalRank)
#Merge Pomeroy and Sagarin Rankings
Ordinals_rankings = merge(massey_pom_end, massey_sagarin_end, by=c("Season", "TeamID"))
Ordinals_rankings <- Ordinals_rankings %>%
  rename(Pomeroy =OrdinalRank.x , 
         Sagarin =OrdinalRank.y)
###################################################################################################################################################
#----------------------------------------------------------- Feature Selection -------------------------------------------------------------------#
#First Merge the Seasonal Stat Averages by Team with the Advanced Stats by Team                                                                   #
#Second Create a Field called "SeasID" that is a combination of Season and TeamID. This is a unique identifier for each team by season            #
#Third Define the Test set as Season, Team 1, Team 2, and Outcome (1=Win, 0 = Loss). Outcome is if Team 1 beat Team 2.                            #
#Munge Team Seeds to Give Seeds by Season by Team                                                                                                 #
#Then Joing Regular Season Stats with Team Seeds and All Aggregated Stats Data                                                                    #
#Clean up Rows with Missing Data, Remove them                                                                                                     #
#Clean up dataset Feature Names with Convention "Team x_name_of_Stat"                                                                             #
###################################################################################################################################################
combined_stats_season <- merge(season_team_stats_averages, advanced_stats_season, by=c("Season", "TeamID"))[,-56]
combined_stats_season <- merge(combined_stats_season,Ordinals_rankings, by=c("Season", "TeamID"))

key_features <- combined_stats_season %>%
  #select(Season, TeamID, WinRate, AvgPtDiff, Ortg, Drtg, NetRtg) %>%
  mutate(SeasID = paste(Season, TeamID, sep = "_"))

if(sum(tourney_compact_df$WTeamID == tourney_compact_df$LTeamID) > 0) {
  print("Same winning and losing team, check data")
}

train <- tourney_compact_df %>% 
  select(Season, WTeamID, LTeamID) %>% 
  mutate(team_id_diff = WTeamID - LTeamID,
         Team1 = if_else(team_id_diff < 0, WTeamID, LTeamID),
         Team2 = if_else(team_id_diff > 0, WTeamID, LTeamID),
         result = if_else(WTeamID == Team1, 1, 0)) %>% 
  #               filter((Team1 - Team2) > 0) %>%
  select(Season, Team1, Team2, result)

#convert seed to integers
tourney_seeds$Seed <- as.integer(str_extract_all(tourney_seeds$Seed, "[0-9]+"))

team1_seeds <- tourney_seeds %>% set_colnames(c("Season", "T1Seed", "Team1ID"))
team2_seeds <- tourney_seeds %>% set_colnames(c("Season", "T2Seed", "Team2ID"))

#join regular season stats to corresponding tourney teams seeds
train <- train %>% 
  left_join(team1_seeds, by = c("Season", "Team1"="Team1ID")) %>%    
  left_join(team2_seeds, by = c("Season", "Team2"="Team2ID")) %>% 
  mutate(team1_seed_str = if_else(T1Seed < 9, 1, 0),
         team2_seed_str = if_else(T2Seed < 9, 1, 0),
         seed_diff = T1Seed - T2Seed,
         SeasID1 = paste(Season, Team1, sep = "_"),
         SeasID2 = paste(Season, Team2, sep = "_")) %>%
  left_join(key_features, by = c("SeasID1" = "SeasID")) %>%
  left_join(key_features, by = c("SeasID2" = "SeasID"))

tail(train)

# remove rows with NAs
train <- na.omit(train)

names((train))

#clean up train data frame
train <- train %>%
  select(-SeasID1, -SeasID2, -Season.y, -TeamID.x, -Season, -TeamID.y, -TeamName.x.x, -TeamName.x.y,-T1Seed, -T2Seed) %>%
  rename(Season = Season.x,
         Team1_WinRate = WinPerc.x, Team2_WinRate = WinPerc.y,
         Team1_Wins = wins.x, Team2_Wins = wins.y, 
         Team1_AstRatio = AstRatio.x, Team2_AstRatio = AstRatio.y, 
         Team1_AvgBlk = Avg_Blk.x, Team2_AvgBlk = Avg_Blk.y, 
         Team1_Avg_DR = Avg_DR.x, Team2_Avg_DR = Avg_DR.y, 
         Team1_AvgAst = AvgAst.x, Team2_AvgAst = AvgAst.y, 
         Team1_AvgAstAllow = AvgAstAllow.x, Team2_AvgAstAllow = AvgAstAllow.y, 
         Team1_AvgBlkAllow = AvgBlkAllow.x, Team2_AvgBlkAllow = AvgBlkAllow.y, 
         Team1_AvgDRAllow = AvgDRAllow.x, Team2_AvgDRAllow = AvgDRAllow.y,
         Team1_Avg_FGA = AvgFGA.x, Team2_Avg_FGA = AvgFGA.y, 
         Team1_Avg_FGA3 = AvgFGA3.x, Team2_Avg_FGA3 = AvgFGA3.y, 
         Team1_Threes_Share = ThreesShare.x, Team2_Threes_Share = ThreesShare.y, 
         Team1_SoS = SoS.x, Team2_SoS = SoS.y, 
         Team1_Tie = Tie.x, Team2_Tie = Tie.y, 
         Team1_TORatio = TORatio.x, Team2_TORatio = TORatio.y, 
         Team1_TotPoints = TotPoints.x, Team2_TotPoints = TotPoints.y,
         Team1_TotPointsAllow = TotPointsAllow.x, Team2_TotPointsAllow = TotPointsAllow.y, 
         Team1_TotRebPerc = TotRebPerc.x, Team2_TotRebPerc = TotRebPerc.y, 
         Team1_TSPerc = TSPerc.x, Team2_TSPerc = TSPerc.y, 
         Team1_AvgFGA3Allow = AvgFGA3Allow.x, Team2_AvgFGA3Allow = AvgFGA3Allow.y, 
         Team1_AvgFGA_Allow = AvgFGAAllow.x, Team2_AvgFGA_Allow = AvgFGAAllow.y, 
         Team1_AvgFGM = AvgFGM.x, Team2_AvgFGM = AvgFGM.y, 
         Team1_AvgFGM3 = AvgFGM3.x, Team2_AvgFGM3 = AvgFGM3.y,
         Team1_PointsPerPossAllow = PointsPerPossAllow.x, Team2_PointsPerPossAllow = PointsPerPossAllow.y, 
         Team1_PointsPerPoss = PointsPerPoss.x, Team2_PointsPerPoss = PointsPerPoss.y, 
         Team1_Pie = Pie.x, Team2_Pie = Pie.y, 
         Team1_OPie = Pie.x, Team2_OPie = Pie.y, 
         Team1_OffRtg = OffRtg.x, Team2_OffRtg = OffRtg.y, 
         Team1_GP = GP.x,Team2_GP = GP.y, 
         Team1_FTRate = FTRate.x, Team2_FTRate = FTRate.y, 
         Team1_DefRtg = DefRtg.x, Team2_DefRtg = DefRtg.y,
         Team1_DefRebPerc = DefRebPerc.x, Team2_DefRebPerc = DefRebPerc.y, 
         Team1_AvgTOAllow = AvgTOAllow.x, Team2_AvgTOAllow = AvgTOAllow.y, 
         Team1_AvgTO = AvgTO.x, Team2_AvgTO = AvgTO.y, 
         Team1_AvgStlAllow = AvgStlAllow.x, Team2_AvgStlAllow = AvgStlAllow.y, 
         Team1_AvgStl = AvgStl.x, Team2_AvgStl = AvgStl.y, 
         Team1_AvgPossAllow = AvgPossAllow.x, Team2_AvgPossAllow = AvgPossAllow.y, 
         Team1_AvgPoss = AvgPoss.x, Team2_AvgPoss = AvgPoss.y, 
         Team1_AvgPointsAllow = AvgPointsAllow.x, Team2_AvgPointsAllow = AvgPointsAllow.y, 
         Team1_AvgPoints = AvgPoints.x, Team2_AvgPoints = AvgPoints.y, 
         Team1_AvgPFAllow = AvgPFAllow.x, Team2_AvgPFAllow = AvgPFAllow.y, 
         Team1_AvgPF = AvgPF.x, Team2_AvgPF = AvgPF.y, 
         Team1_AvgOT = AvgOT.x, Team2_AvgOT = AvgOT.y, 
         Team1_AvgORAllow = AvgORAllow.x, Team2_AvgORAllow = AvgORAllow.y, 
         Team1_AvgOR = AvgOR.x, Team2_AvgOR = AvgOR.y, 
         Team1_AvgFTMAllow = AvgFTMAllow.x, Team2_AvgFTMAllow = AvgFTMAllow.y, 
         Team1_AvgFTM = AvgFTM.x, Team2_AvgFTM = AvgFTM.y, 
         Team1_AvgFTAAllow = AvgFTAAllow.x, Team2_AvgFTAAllow = AvgFTAAllow.y, 
         Team1_AvgFTA = AvgFTA.x, Team2_AvgFTA = AvgFTA.y, 
         Team1_AvgFGMAllow = AvgFGMAllow.x, Team2_AvgFGMAllow = AvgFGMAllow.y, 
         Team1_AvgFGM3Allow = AvgFGM3Allow.x, Team2_AvgFGM3Allow = AvgFGM3Allow.y, 
         Team1_Pomeroy = Pomeroy.x, Team2_Pomeroy = Pomeroy.y, 
         Team1_Sagarin = Sagarin.x, Team2_Sagarin = Sagarin.y
         ) 

# subset train FOR STAGE 1 SUBMISSION PICK YOUR TRAINING SET YEARS
train <- subset(train, Season < 2019)

################################################################################################################################################
#--------------------------------------------------------------- Train Model -----------------------------------------------------------------#
################################################################################################################################################
fitRF <- randomForest(result ~., 
                      data = train,
                      ntree = 500)  

train$PredRF <- predict(fitRF, train)

mean(train$result == train$PredRF)

plot(fitRF)
varImpPlot(fitRF)
################################################################################################################################################
#----------------------------------------------------------------- Test Set -------------------------------------------------------------------#
################################################################################################################################################
#create test df
test <- read.csv("SampleSubmissionStage2.csv", stringsAsFactors = FALSE) %>% 
  select(ID) %>% 
  separate(ID, sep = "_", into = c("Season", "Team1", "Team2"), convert = TRUE) %>%
  left_join(team1_seeds, by = c("Season", "Team1"="Team1ID")) %>%
  left_join(team2_seeds, by = c("Season", "Team2"="Team2ID")) %>% 
  mutate(team1_seed_str = if_else(T1Seed < 9, 1, 0),
         team2_seed_str = if_else(T2Seed < 9, 1, 0),
         seed_diff = T1Seed - T2Seed) %>% 
  mutate(SeasID1 = paste(Season, Team1, sep = "_"),
         SeasID2 = paste(Season, Team2, sep = "_")) %>%
  left_join(key_features, by = c("SeasID1" = "SeasID")) %>%
  left_join(key_features, by = c("SeasID2" = "SeasID"))

names((test))

#clean up test df
test <- test %>%
  select(-SeasID1, -SeasID2, -Season.y, -TeamID.x, -Season, -TeamID.y, -TeamName.x.x, -TeamName.x.y, -T1Seed, -T2Seed) %>%
  rename(Season = Season.x,
         Team1_WinRate = WinPerc.x,Team2_WinRate = WinPerc.y,
         Team1_Wins = wins.x, Team2_Wins = wins.y, 
         Team1_AstRatio = AstRatio.x, Team2_AstRatio = AstRatio.y, 
         Team1_AvgBlk = Avg_Blk.x, Team2_AvgBlk = Avg_Blk.y, 
         Team1_Avg_DR = Avg_DR.x, Team2_Avg_DR = Avg_DR.y, 
         Team1_AvgAst = AvgAst.x, Team2_AvgAst = AvgAst.y, 
         Team1_AvgAstAllow = AvgAstAllow.x, Team2_AvgAstAllow = AvgAstAllow.y, 
         Team1_AvgBlkAllow = AvgBlkAllow.x, Team2_AvgBlkAllow = AvgBlkAllow.y, 
         Team1_AvgDRAllow = AvgDRAllow.x, Team2_AvgDRAllow = AvgDRAllow.y,
         Team1_Avg_FGA = AvgFGA.x, Team2_Avg_FGA = AvgFGA.y, 
         Team1_Avg_FGA3 = AvgFGA3.x, Team2_Avg_FGA3 = AvgFGA3.y, 
         Team1_Threes_Share = ThreesShare.x, Team2_Threes_Share = ThreesShare.y, 
         Team1_SoS = SoS.x, Team2_SoS = SoS.y, 
         Team1_Tie = Tie.x, Team2_Tie = Tie.y, 
         Team1_TORatio = TORatio.x, Team2_TORatio = TORatio.y, 
         Team1_TotPoints = TotPoints.x, Team2_TotPoints = TotPoints.y,
         Team1_TotPointsAllow = TotPointsAllow.x, Team2_TotPointsAllow = TotPointsAllow.y, 
         Team1_TotRebPerc = TotRebPerc.x, Team2_TotRebPerc = TotRebPerc.y, 
         Team1_TSPerc = TSPerc.x, Team2_TSPerc = TSPerc.y, 
         Team1_AvgFGA3Allow = AvgFGA3Allow.x, Team2_AvgFGA3Allow = AvgFGA3Allow.y, 
         Team1_AvgFGA_Allow = AvgFGAAllow.x, Team2_AvgFGA_Allow = AvgFGAAllow.y, 
         Team1_AvgFGM = AvgFGM.x, Team2_AvgFGM = AvgFGM.y, 
         Team1_AvgFGM3 = AvgFGM3.x, Team2_AvgFGM3 = AvgFGM3.y,
         Team1_PointsPerPossAllow = PointsPerPossAllow.x, Team2_PointsPerPossAllow = PointsPerPossAllow.y, 
         Team1_PointsPerPoss = PointsPerPoss.x, Team2_PointsPerPoss = PointsPerPoss.y, 
         Team1_Pie = Pie.x, Team2_Pie = Pie.y, 
         Team1_OPie = Pie.x, Team2_OPie = Pie.y, 
         Team1_OffRtg = OffRtg.x, Team2_OffRtg = OffRtg.y, 
         Team1_GP = GP.x,Team2_GP = GP.y, 
         Team1_FTRate = FTRate.x, Team2_FTRate = FTRate.y, 
         Team1_DefRtg = DefRtg.x, Team2_DefRtg = DefRtg.y,
         Team1_DefRebPerc = DefRebPerc.x, Team2_DefRebPerc = DefRebPerc.y, 
         Team1_AvgTOAllow = AvgTOAllow.x, Team2_AvgTOAllow = AvgTOAllow.y, 
         Team1_AvgTO = AvgTO.x, Team2_AvgTO = AvgTO.y, 
         Team1_AvgStlAllow = AvgStlAllow.x, Team2_AvgStlAllow = AvgStlAllow.y, 
         Team1_AvgStl = AvgStl.x, Team2_AvgStl = AvgStl.y, 
         Team1_AvgPossAllow = AvgPossAllow.x, Team2_AvgPossAllow = AvgPossAllow.y, 
         Team1_AvgPoss = AvgPoss.x, Team2_AvgPoss = AvgPoss.y, 
         Team1_AvgPointsAllow = AvgPointsAllow.x, Team2_AvgPointsAllow = AvgPointsAllow.y, 
         Team1_AvgPoints = AvgPoints.x, Team2_AvgPoints = AvgPoints.y, 
         Team1_AvgPFAllow = AvgPFAllow.x, Team2_AvgPFAllow = AvgPFAllow.y, 
         Team1_AvgPF = AvgPF.x, Team2_AvgPF = AvgPF.y, 
         Team1_AvgOT = AvgOT.x, Team2_AvgOT = AvgOT.y, 
         Team1_AvgORAllow = AvgORAllow.x, Team2_AvgORAllow = AvgORAllow.y, 
         Team1_AvgOR = AvgOR.x, Team2_AvgOR = AvgOR.y, 
         Team1_AvgFTMAllow = AvgFTMAllow.x, Team2_AvgFTMAllow = AvgFTMAllow.y, 
         Team1_AvgFTM = AvgFTM.x, Team2_AvgFTM = AvgFTM.y, 
         Team1_AvgFTAAllow = AvgFTAAllow.x, Team2_AvgFTAAllow = AvgFTAAllow.y, 
         Team1_AvgFTA = AvgFTA.x, Team2_AvgFTA = AvgFTA.y, 
         Team1_AvgFGMAllow = AvgFGMAllow.x, Team2_AvgFGMAllow = AvgFGMAllow.y, 
         Team1_AvgFGM3Allow = AvgFGM3Allow.x, Team2_AvgFGM3Allow = AvgFGM3Allow.y,
         Team1_Pomeroy = Pomeroy.x, Team2_Pomeroy = Pomeroy.y, 
         Team1_Sagarin = Sagarin.x, Team2_Sagarin = Sagarin.y
  ) 
################################################################################################################################################
#---------------------------------------------------------------Predict------------------------------------------------------------------------#
################################################################################################################################################
test$Pred <- predict(fitRF, test)

#write to file
submitR1 <- test %>% 
  select(Season, Team1, Team2, Pred) %>%
  unite("ID", Season, Team1, Team2, sep = "_") %>%
  write.csv("submitR4.csv", row.names = FALSE)






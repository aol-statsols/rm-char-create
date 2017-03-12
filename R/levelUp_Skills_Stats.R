#' A LEVEL UP FUNCTION
#'
#' FUNCTION TO DEAL WITH LEVELING UP PAST LEVEL TO THE MAX LEVEL THE CHARACTER REACHES.
#' @param char.Level STRING CONTAINING CHARACTER'S LEVEL
#' @param level_Up.Result.List EITHER A LIST CONTAINING ALL RESULTS FOR LEVELING UP FOR ***STATS*** WHICH HAVE BEEN PREGENERATED OR -1
#' @param stat.Tmp VECTOR OF CHARACTER'S TEMPORARY STATS
#' @param stat.Pot VECTOR OF CHARACTER'S POTENTIAL STATS
#' @param char.Age NUMERIC CONTAINING THE CHARACTER'S CURRENT AGE
#' @param char.Age_Die NUMERIC CONTAINING THE SIZE (d4, d6, etc.) OF THE CHARACTER'S AGE DIE, USED TO INCREASE THE AGE FOR EACH LEVEL GREATER THAN 1 THE CHARACTER STARTS AT.
#' @param char.Age.List LIST OF ALL AGES THE CHARACTER HAS REACHED EACH LEVEL SO FAR.
#' @param char.Alive = TRUE IS THE CHARACTER STILL ALIVE            
#' @param char.Class STRING CONTAINING THE CHARACTER'S PROFESSION    
#' @param species.Middle_Aged NUMERIC WITH THE AGE AT WHICH THE CHARACTER'S SPECIES REACHES MIDDLE AGE
#' @param species.Old NUMERIC WITH THE AGE AT WHICH THE CHARACTER'S SPECIES REACHES OLD AGE
#' @param species.Venerable NUMERIC WITH THE AGE AT WHICH THE CHARACTER'S SPECIES REACHES VENERABLE AGE
#' @param age_Crisis.Middle_Aged HAS THE CHARACTER REACHED MIDDLE AGE YET
#' @param age_Crisis.Old HAS THE CHARACTER REACHED OLD AGE YET
#' @param age_Crisis.Venerable HAS THE CHARACTER REACHED VENERABLE AGE YET
#' @param skills.Increase.Record.DF DATAFRAME OF SKILLS WHICH CAN BE INCREASED
#' @param background_Options.Benefits LIST CONTAINING ALL BACKGROUND BENEFITS CHOSEN OR ROLLED
#' @param dev_Points.Level_1 DEVELPOMENT POINTS FOR LEVEL 1
#' @param level_1_Skills.Increase.Record.DF DATAFRAME OF SKILL INCREASES FROM LEVEL 1
#' @param level_1_Skills.Increase.Record.List RESULT OF LEVELING UP AT LEVEL 1
#' @param level_1_Skills.Increase.Record.DP_Spent DEVELOPMENT POINTS SPENT DURING LEVEL 1.
#' @param level_Up.Repeated.Result.List = -1 EITHER A LIST CONTAINING ALL RESULTS FOR LEVELING UP FOR ***SKILLS*** WHICH HAVE BEEN PREGENERATED OR -1
#' @param skills.Each.Level.List LIST CONTAINING DATAFRAMES FOR THE SKILLS LEVELED UP EACH LEVEL
#' @concept level skills stats
#' @export
#' @examples
#' levelUp_Skills_Stats()

levelUp_Skills_Stats <-
function( char.Level = -1, level_Up.Result.List = -1, stat.Tmp = -1, stat.Pot = -1, char.Age = -1, char.Age_Die = -1, char.Age.List = -1, char.Alive = TRUE, char.Class = -1, 
                                    species.Middle_Aged = -1, species.Old = -1, species.Venerable = -1, age_Crisis.Middle_Aged = -1, age_Crisis.Old = -1, age_Crisis.Venerable = -1,
                                    skills.Increase.Record.DF = -1, background_Options.Benefits = -1,
                                    dev_Points.Level_1 = -1, level_1_Skills.Increase.Record.DF = -1, level_1_Skills.Increase.Record.List = -1, level_1_Skills.Increase.Record.DP_Spent = -1,
                                    level_Up.Repeated.Result.List = -1, skills.Each.Level.List = -1
                                        ){

if(unlist(char.Level)[1] == -1 & unlist(level_Up.Result.List)[1] == -1 & unlist(stat.Tmp)[1] == -1 & unlist(stat.Pot)[1] == -1 & unlist(char.Age)[1] == -1 & 
	unlist(char.Age_Die)[1] == -1 & unlist(char.Age.List)[1] == -1 & unlist(char.Alive)[1] == TRUE & unlist(char.Class)[1] == -1 &
	unlist(species.Middle_Aged)[1] == -1 & unlist(species.Old)[1] == -1 & unlist(species.Venerable)[1] == -1 &
	unlist(age_Crisis.Middle_Aged)[1] == -1 & unlist(age_Crisis.Old)[1] == -1 & unlist(age_Crisis.Venerable)[1] == -1 &
    unlist(skills.Increase.Record.DF)[1] == -1 & unlist(background_Options.Benefits)[1] == -1 &
    unlist(dev_Points.Level_1)[1] == -1 & unlist(level_1_Skills.Increase.Record.DF)[1] == -1 & unlist(level_1_Skills.Increase.Record.List)[1] == -1 &
	unlist(level_1_Skills.Increase.Record.DP_Spent)[1] == -1 &
    unlist(level_Up.Repeated.Result.List)[1] == -1 & unlist(skills.Each.Level.List)[1] == -1) return(-1)                                
#returns:                                   LIST CONTAINING 4 OBJECTS. 1) skills.Each.Level.Fully.Assigned. 2) stat.List. 3) skills.Each.Level.List. 4) level_Up.Repeated.Result.List.
    
    stats = stat_Name_DF[,1]
    stat = statVal_And_Bonus_And_DP[,1]
    bonus <- statVal_And_Bonus_And_DP[,2]
    dev_Points <- statVal_And_Bonus_And_DP[,3]
    
    if( char.Level > 1 ){
        if ( unlist(level_Up.Result.List)[1] ==-1 ){
            level_Up.Result.List <- level_Up_Stat_Changes(stat.Tmp,stat.Pot,char.Age,char.Age_Die,species.Middle_Aged,species.Old,species.Venerable,age_Crisis.Middle_Aged,age_Crisis.Old,age_Crisis.Venerable)
        }
        
        stat.Tmp <- level_Up.Result.List[[1]]; stat.Pot <- level_Up.Result.List[[2]]; stat.Bonus <- level_Up.Result.List[[3]]; stat.Dev <- level_Up.Result.List[[4]]; dev_Points.Level_2 <- floor(sum(stat.Dev));
        char.Age <- level_Up.Result.List[[5]]; age_Crisis.Middle_Aged <- level_Up.Result.List[[6]]; age_Crisis.Old <- level_Up.Result.List[[7]]; age_Crisis.Venerable <- level_Up.Result.List[[8]];
        stat.List <- list( data.frame(Stat = stats, Stat.Tmp = stat.Tmp, Stat.Potential = stat.Pot, Stat.Bonus = stat.Bonus, Stat.Dev = stat.Dev) )
        if(any(stat.Pot <= 0)){
            char.Alive = FALSE
        }
        
        if ( any(stat.Tmp > stat.Pot) ) stat.Tmp[stat.Tmp > stat.Pot] = stat.Pot[stat.Tmp > stat.Pot] 
        
        char.Age.List[[2]] = char.Age
        dev_Points.Total = dev_Points.Level_2
        
        #ASSIGNS SKILLS TO INCREASE EACH LEVEL AFTER THE FIRST. GIVES THE PLAYER A CHANCE TO CHANGE THE ASSIGNMENT IF THEY ARE UNHAPPY.
        satisfied = FALSE
        skills.Increase.Record.DF[,"TotalRanks"] = level_1_Skills.Increase.Record.DF[,"TotalRanks"]
        
        if( unlist(skills.Each.Level.List)[1] == -1 ){
            skills.Each.Level.List <- choose_Skills_To_Increase_Each_Level(skills.Increase.Record.DF, dev_Points.Total, char.Class, background_Options.Benefits) 
        } else{
            satisfied = FALSE
        }
        
        while( !satisfied ){
            cat("Your current skill list will increase like this each level:\n\n")
            print(skills.Each.Level.List[[1]])
            prom <- "\n\nAre you satisfied with the skills above?\n\n"
            ans <- read_Yes_Or_No(prom)
            
            if( ans == "No" ){
                skills.Each.Level.List <- choose_Skills_To_Increase_Each_Level(skills.Increase.Record.DF, dev_Points.Total, char.Class, background_Options.Benefits)
            } else{
                satisfied = TRUE
            }
        }
        skills.Each.Level <- skills.Each.Level.List[[1]]
        dev_Points.Spent <- dev_Points.Total - skills.Each.Level.List[[2]] #DEVELOPMENT POINTS LEFT OVER EACH LEVEL. MUST PRINT OUT.
        if( char.Level > 2 ){
        
            #WORKS OUT THE STATS, BONUSES, DEV POINTS, AGE INCREASE, ANY AGING CRISES, ANY DEATHS ETC FOR EACH CHARACTER CREATED. STORES THEM IN stat.List.
            if( unlist(level_Up.Repeated.Result.List)[1] == -1 ){
                stat.List[(length(stat.List)+1)] <- list( data.frame(Stats = stats, Stat.Tmp = stat.Tmp, Stat.Potential = stat.Pot, Stat.Bonus = stat.Bonus, Stat.Dev = stat.Dev) )
                
                dev_Points.Level <- NULL; 
                for (i.Level in 1:( char.Level - 2)){
                    level_Up.Result.List <- level_Up_Stat_Changes(stat.Tmp,stat.Pot,char.Age,char.Age_Die,species.Middle_Aged,species.Old,species.Venerable,age_Crisis.Middle_Aged,age_Crisis.Old,age_Crisis.Venerable)
                    
                    stat.Tmp <- level_Up.Result.List[[1]]; stat.Pot <- level_Up.Result.List[[2]]; stat.Bonus <- level_Up.Result.List[[3]]; stat.Dev <- level_Up.Result.List[[4]]; dev_Points.Level[i.Level] <- floor(sum(stat.Dev));
                    char.Age <- level_Up.Result.List[[5]]; age_Crisis.Middle_Aged <- level_Up.Result.List[[6]]; age_Crisis.Old <- level_Up.Result.List[[7]]; age_Crisis.Venerable <- level_Up.Result.List[[8]];
                    
                    if(any(stat.Pot <= 0)){
                        char.Alive = FALSE
                        break;
                    }
          
                    if (any(stat.Tmp > stat.Pot) ) stat.Tmp[stat.Tmp > stat.Pot] = stat.Pot[stat.Tmp > stat.Pot] 
                    stat.List[(length(stat.List)+1)] <- list( data.frame(Stat = stats, Stat.Tmp = stat.Tmp, Stat.Potential = stat.Pot, Stat.Bonus = stat.Bonus, Stat.Dev = stat.Dev) )
                   
                    char.Age.List[[(length(char.Age.List) + 1)]] = char.Age
                    level_Up.Repeated.Result.List[[i.Level]] <- list( level_Up.Result.List )
                 }
                
                if( !(length(dev_Points.Level_1)) ) dev_Points.Level_1 = 0
                dev_Points.Level <- c( dev_Points.Level_1, dev_Points.Level_2, dev_Points.Level )
            } else{
                dev_Points.Level <- NULL; 
                stat.List[(length(stat.List)+1)] <- list( data.frame(Stats = stats, Stat.Tmp = stat.Tmp, Stat.Potential = stat.Pot, Stat.Bonus = stat.Bonus, Stat.Dev = stat.Dev) )
               
                
                for (i.Level in 1:length(level_Up.Repeated.Result.List)){
                    level_Up.Result.List <- level_Up.Repeated.Result.List[[i.Level]]
                    
                    stat.Tmp <- level_Up.Result.List[[1]][[1]]; stat.Pot <- level_Up.Result.List[[1]][[2]]; stat.Bonus <- level_Up.Result.List[[1]][[3]]; stat.Dev <- level_Up.Result.List[[1]][[4]]; dev_Points.Level[i.Level] <- floor(sum(stat.Dev));
                    char.Age <- level_Up.Result.List[[1]][[5]]; age_Crisis.Middle_Aged <- level_Up.Result.List[[1]][[6]]; age_Crisis.Old <- level_Up.Result.List[[1]][[7]]; age_Crisis.Venerable <- level_Up.Result.List[[1]][[8]];
                    
                    stat.List[(length(stat.List)+1)] <- list( data.frame(Stat = stats, Stat.Tmp = stat.Tmp, Stat.Potential = stat.Pot, Stat.Bonus = stat.Bonus, Stat.Dev = stat.Dev) )
                    if(any(stat.Pot <= 0)){
                        char.Alive = FALSE
                        break;
                    }
                    
                    if (any(stat.Tmp > stat.Pot) ) stat.Tmp[stat.Tmp > stat.Pot] = stat.Pot[stat.Tmp > stat.Pot] 
                    stat.List[(length(stat.List)+1)] <- list( data.frame(Stats = stats, Stat.Tmp = stat.Tmp, Stat.Potential = stat.Pot, Stat.Bonus = stat.Bonus, Stat.Dev = stat.Dev) )
               
                    char.Age.List[[(i.Level + 2)]] = char.Age
                    level_Up.Repeated.Result.List[[i.Level]] = list( level_Up.Result.List )
                 }
                
                if( !(length(dev_Points.Level_1)) ) dev_Points.Level_1 = 0
                dev_Points.Level <- c( dev_Points.Level_1, dev_Points.Level_2, dev_Points.Level )
            }
            
        } else{
            dev_Points.Level = c( dev_Points.Level_1, dev_Points.Level_2 )
            level_Up.Repeated.Result.List = -1
        }
        
    } else{
stat.Bonus <- 1:10
stat.Dev <- NULL

for(i.Stat in 1:10){
stat.Bonus[i.Stat] = min(bonus[stat.Tmp[i.Stat] <= stat])
stat.Dev[i.Stat] = ifelse( i.Stat < 6, min(dev_Points[stat.Tmp[i.Stat] <= stat]), 0 )
}
        stat.List <- list( data.frame(Stat = stats, Stat.Tmp = stat.Tmp, Stat.Potential = stat.Pot, Stat.Bonus = stat.Bonus, Stat.Dev = stat.Dev) )
        dev_Points.Level = dev_Points.Level_1
        skills.Each.Level.List = -1
        level_Up.Repeated.Result.List = -1
    }
    if( char.Level > 2 ){
        dev_Points.Spent.Level <- c( level_1_Skills.Increase.Record.DP_Spent,rep(dev_Points.Spent,(length(dev_Points.Level)-1)) )
    
        skills.Each.Level.Fully.Assigned <- assign_Leftover_DP(char.Class,dev_Points.Level,dev_Points.Spent.Level,skills.Each.Level,background_Options.Benefits)
        skills.Each.Level.Fully.Assigned[[1]] <- level_1_Skills.Increase.Record.List
        
    } else if( char.Level == 2 ){
        dev_Points.Level = c( dev_Points.Level_1, dev_Points.Level_2)
        dev_Points.Spent.Level <- c( level_1_Skills.Increase.Record.DP_Spent,dev_Points.Spent )
    
        skills.Each.Level.Fully.Assigned <- assign_Leftover_DP(char.Class,dev_Points.Level,dev_Points.Spent.Level,skills.Each.Level,background_Options.Benefits)
        skills.Each.Level.Fully.Assigned[[1]] <- level_1_Skills.Increase.Record.List
    } else{
        dev_Points.Level = dev_Points.Level_1
        dev_Points.Spent.Level <- level_1_Skills.Increase.Record.DP_Spent
    
        skills.Each.Level.Fully.Assigned <- NULL
        skills.Each.Level.Fully.Assigned[[1]] <- level_1_Skills.Increase.Record.List
    }   
    skills.Each.Level.Fully.Assigned[[length(skills.Each.Level.Fully.Assigned)]][[1]][,"NumTimesIncreased"] = 
        skills.Each.Level.Fully.Assigned[[length(skills.Each.Level.Fully.Assigned)]][[1]][,"TotalRanks"]
    return( list( skills.Each.Level.Fully.Assigned, stat.List, skills.Each.Level.List, level_Up.Repeated.Result.List) )
}

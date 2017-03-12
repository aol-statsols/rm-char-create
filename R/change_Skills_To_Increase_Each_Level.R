#' A HEIGHT AND WEIGHT FUNCTION
#'
#' FUNCTION TO CHOOSE WHICH SKILLS TO INCREASE EACH LEVEL UNTIL YOU HAVE <10 DP REMAINING (TO GIVE A BUFFER)
#' @param skills.Increase.Record.DF A DATAFRAME CONTAINING WHICH SKILLS HAVE BEEN INCREASED AND HOW OFTEN.
#' @param dev_Points.Total AN INTEGER WHICH IS HOW MANY DEV POINTS THEY HAVE THAT LEVEL
#' @param char.Class A STRING WHICH IS WHAT CLASS THEY ARE (DETERMINES SKILL FACTS)
#' @param background_Options.Benefits LIST CONTAINING ALL BACKGROUND OPTION BENEFITS A PLAYER HAS TAKEN
#' @param skills_Groups GROUPS OF SKILLS WHICH SHARE HALF RANKS WITH EACH OTHER
#' @concept skills level
#' @export
#' @examples
#' change_Skills_To_Increase_Each_Level()

change_Skills_To_Increase_Each_Level <-
function(skills.Increase.Record.DF = -1, dev_Points.Total = -1, char.Class = -1, background_Options.Benefits = -1
                                                    , skills_Groups = -1){ #
                    
if(unlist(skills.Increase.Record.DF)[1] == -1 & unlist(dev_Points.Total)[1] == -1 & unlist(char.Class)[1] == -1 & 
	unlist(background_Options.Benefits)[1] == -1 & unlist(skills_Groups)[1] == -1) return(-1)                         
#returns:   A LIST OF TWO OBJECTS: 1) THE COMPLETED DATAFRAME OF SKILL INCREASES. 2) THE TOTAL DEVELOPMENT POINTS REMAINING.
skill.Num <- nrow(skills.Increase.Record.DF)
skill.Names <- skills.Increase.Record.DF[,1]
    skill.Chosen.Name <- NULL
continue = TRUE
    
while( dev_Points.Total < 0 ){
    
        skills.Increase.Record.DF.OUT <- skills_DF_OUT_CREATE(skills.Increase.Record.DF, FALSE)
print(skills.Increase.Record.DF.OUT)
        cat("\n\n               
            * * * * * * * * * * * * * * * * * * * * * * * * * * * *
            *                                                     *
            *    YOU HAVE TO DECREASE DEVELOPMENT POINTS SPENT    *
            *                                                     *
            * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n\n")
        cat("\n\nEnter the name of the skill you wish to remove a rank from:\n\nOR\n\nChoose a number between 1 and ", skill.Num,"\n\n",sep="")
cat("\n\nYou have ", -dev_Points.Total, " Development Points left to remove.")
if (interactive() ){ skill <- readline()} else{ skill <- readLines("stdin",1)  }

if( is.na( suppressWarnings( as.numeric( skill ) ) ) ) {

if( skill %in% skill.Names ){

                if( skill == "Control" & !( "Lycanthropy" %in% unlist(background_Options.Benefits) ) ){
                    cat(paste0( "\nYou can only choose that skill if you are a Lycanthrope. Please choose another.\n" ))
                    if (interactive() ){ readline()} else{ readLines("stdin",1)  }
                    return( change_Skills_To_Increase_Each_Level(skills.Increase.Record.DF, dev_Points.Total, char.Class, background_Options.Benefits) )
                }

skillRow = skills.Increase.Record.DF[skills.Increase.Record.DF[,1] == skill,]
if( 0 < skillRow["NumTimesIncreased"] & skillRow["NumTimesIncreased"] <= skillRow["NumTimesCanIncrease"] ){
if( skillRow["NumTimesCanIncrease"] == 55 ){
                        skillRow["NumTimesIncreased"] = skillRow["NumTimesIncreased"] - 1
                        dev_Points.Total = unlist( dev_Points.Total + skillRow["Skills.Cost.1"] ) 
                        skills.Increase.Record.DF[skills.Increase.Record.DF[,1] == skill,] = skillRow
                    }else{
                        skill.Cost = unlist( ifelse( skillRow["NumTimesIncreased"] == 1, skillRow["Skills.Cost.1"], 
                                        ifelse( skillRow["NumTimesIncreased"] == 2, skillRow["Skills.Cost.2"], 
                                        ifelse( skillRow["NumTimesIncreased"] == 3, skillRow["Skills.Cost.3"]  ) ) ) )
                        skillRow["NumTimesIncreased"] = skillRow["NumTimesIncreased"] - 1
                        dev_Points.Total = unlist( dev_Points.Total + skill.Cost )
                        skills.Increase.Record.DF[skills.Increase.Record.DF[,1] == skill,] = skillRow
                    }
} else{
cat( "\nYou must have already increased \"", skill, "\" at least once to remove a rank.\n\nChoose something else to decrease.\n\nPress enter to continue.\n", sep="" )
if (interactive() ){ readline()} else{ readLines("stdin",1)  }
return( change_Skills_To_Increase_Each_Level(skills.Increase.Record.DF, dev_Points.Total, char.Class,background_Options.Benefits) )
}
                
}else{
cat(paste0( "\nThe name must be from\n", paste0(skill.Names,collapse=", "), "\n\nPress enter to continue.\n" ))
if (interactive() ){ readline()} else{ readLines("stdin",1)  }
return( change_Skills_To_Increase_Each_Level(skills.Increase.Record.DF, dev_Points.Total, char.Class,background_Options.Benefits) )
}

} else{
            skill <- as.numeric(skill)
if( 1 <= skill && skill <= skill.Num ){

                skill.Chosen.Name <- skill.Names[skill]

                if( skill.Chosen.Name == "Control" & !( "Lycanthropy" %in% unlist(background_Options.Benefits) ) ){
                    cat(paste0( "\nYou can only choose that skill if you are a Lycanthrope. Please choose another.\n" ))
                    if (interactive() ){ readline()} else{ readLines("stdin",1)  }
                    return( change_Skills_To_Increase_Each_Level(skills.Increase.Record.DF, dev_Points.Total, char.Class, background_Options.Benefits) )
                }
                
skillRow = skills.Increase.Record.DF[skill,]
if( 0 < skillRow["NumTimesIncreased"] & skillRow["NumTimesIncreased"] <= skillRow["NumTimesCanIncrease"] ){
if( skillRow["NumTimesCanIncrease"] == 55 ){
                        skillRow["NumTimesIncreased"] = skillRow["NumTimesIncreased"] - 1
                        dev_Points.Total = unlist( dev_Points.Total + skillRow["Skills.Cost.1"] )
                        skills.Increase.Record.DF[skill,] = skillRow
                    }else{
                        skill.Cost = unlist( ifelse( skillRow["NumTimesIncreased"] == 1, skillRow["Skills.Cost.1"], 
                                        ifelse( skillRow["NumTimesIncreased"] == 2, skillRow["Skills.Cost.2"], 
                                        ifelse( skillRow["NumTimesIncreased"] == 3, skillRow["Skills.Cost.3"]  ) ) ) )
                                        
                        skillRow["NumTimesIncreased"] = skillRow["NumTimesIncreased"] - 1
                        dev_Points.Total = unlist( dev_Points.Total + skill.Cost )
                        skills.Increase.Record.DF[skill,] = skillRow
                    }
} else{
cat( "\nYou must have already increased \"", skill.Chosen.Name, "\" at least once to remove a rank.\n\nChoose something else to decrease.\n\nPress enter to continue.\n", sep="" )
if (interactive() ){ readline()} else{ readLines("stdin",1)  }
return( change_Skills_To_Increase_Each_Level(skills.Increase.Record.DF, dev_Points.Total, char.Class,background_Options.Benefits) )
}

} else{
cat(paste0( "\nThe number must be from\n\t1 - ",skill.Num, "\n\nPress enter to continue.\n" ))
if (interactive() ){ readline()} else{ readLines("stdin",1)  }
return( change_Skills_To_Increase_Each_Level(skills.Increase.Record.DF, dev_Points.Total, char.Class,background_Options.Benefits) )
}

        }

}

    if( dev_Points.Total == 0 ){
        cat("\nYou have successfully adjusted your development points for this level.\n\nPress Enter to continue.\n")
        if (interactive() ){ readline()} else{ readLines("stdin",1)  }
        return( list(skills.Increase.Record.DF, dev_Points.Total) )
    } else{
        cat("\nYou have", dev_Points.Total, "development points left - get ready to spend them.\n\nPress Enter to continue.\n")
        if (interactive() ){ readline()} else{ readLines("stdin",1)  }
        return( choose_Skills_To_Increase_Each_Level(skills.Increase.Record.DF, dev_Points.Total, char.Class, background_Options.Benefits) )
    }
    
}

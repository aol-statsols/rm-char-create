#' A SKILL FUNCTION
#'
#' FUNCTION TO CHOOSE WHICH SKILLS TO INCREASE EACH LEVEL UNTIL YOU HAVE <10 DP REMAINING (TO GIVE A BUFFER)
#' @param skills.Increase.Record.DF A DATAFRAME CONTAINING WHICH SKILLS HAVE BEEN INCREASED AND HOW OFTEN.
#' @param dev_Points.Total AN INTEGER WHICH IS HOW MANY DEV POINTS THEY HAVE THAT LEVEL
#' @param char.Class A STRING WHICH IS WHAT CLASS THEY ARE (DETERMINES SKILL FACTS)
#' @param background_Options.Benefits LIST CONTAINING ALL BACKGROUND OPTION BENEFITS A PLAYER HAS TAKEN
#' @param skills_Groups GROUPS OF SKILLS THAT SHARE HALF RANKS
#' @concept skills level 
#' @export
#' @examples
#' choose_Skills_To_Increase_Each_Level()

choose_Skills_To_Increase_Each_Level <-
function(skills.Increase.Record.DF = -1, dev_Points.Total = -1, char.Class = -1, background_Options.Benefits = -1
                                                    , skills_Groups = -1){ 

if(unlist(skills.Increase.Record.DF)[1] == -1 & unlist(dev_Points.Total)[1] == -1 & unlist(char.Class)[1] == -1 & unlist(background_Options.Benefits)[1] == -1 & unlist(skills_Groups)[1] == -1) return(-1)    
#returns: A LIST OF TWO OBJECTS 1) THE COMPLETED DATAFRAME OF SKILL INCREASES. 2) THE TOTAL DEVELOPMENT POINTS REMAINING.
    skill.Num <- nrow(skills.Increase.Record.DF)
skill.Names <- skills.Increase.Record.DF[,1]
    skill.Chosen.Name <- NULL
continue = TRUE
    
while( dev_Points.Total > 0 && continue == TRUE ){
    
        skills.Increase.Record.DF.OUT <- skills_DF_OUT_CREATE(skills.Increase.Record.DF)
print(skills.Increase.Record.DF.OUT)
        cat("\n\nEnter the name of the skill you wish to increase:\n\nOR\n\nChoose a number between 1 and ", skill.Num,":\n\nOR\n\nEnter STOP to exit:",sep="")
cat("\n\nYou have ", dev_Points.Total, " Development Points left to spend.")
if (interactive() ){ skill <- readline()} else{ skill <- readLines("stdin",1)  }

if( is.na( suppressWarnings( as.numeric( skill ) ) ) ) {

            if( skill == "STOP" || skill == "stop" || skill == "S" || skill == "s" ){
break;
            } else if( test_Skill_From_DF_Character(skill, skill.Names, skills.Increase.Record.DF, background_Options.Benefits, dev_Points.Total) ){
                skillRow = skills.Increase.Record.DF[skills.Increase.Record.DF[,1] == skill,]
                skill.Cost = unlist( ifelse( skillRow["NumTimesCanIncrease"] == 55, skillRow["Skills.Cost.1"],
                    ifelse( skillRow["NumTimesIncreased"] == 0, skillRow["Skills.Cost.1"], 
                    ifelse( skillRow["NumTimesIncreased"] == 1, skillRow["Skills.Cost.2"], 
                    ifelse( skillRow["NumTimesIncreased"] == 2, skillRow["Skills.Cost.3"]  ) ) ) ) )
                dev_Points.Total = unlist( dev_Points.Total - skill.Cost ) 
                skills.Increase.Record.DF[skills.Increase.Record.DF[,1] == skill,"NumTimesIncreased"] = skillRow["NumTimesIncreased"] + 1
            } else{
                return( choose_Skills_To_Increase_Each_Level(skills.Increase.Record.DF, dev_Points.Total, char.Class, background_Options.Benefits) )
            }


} else{
            
            if( test_Skill_From_DF_Numeric(skill, skill.Names, skills.Increase.Record.DF, background_Options.Benefits, dev_Points.Total) ){
                skillRow = skills.Increase.Record.DF[ as.numeric(skill),]
                skill.Cost = unlist( ifelse( skillRow["NumTimesCanIncrease"] == 55, skillRow["Skills.Cost.1"],
                    ifelse( skillRow["NumTimesIncreased"] == 0, skillRow["Skills.Cost.1"], 
                    ifelse( skillRow["NumTimesIncreased"] == 1, skillRow["Skills.Cost.2"], 
                    ifelse( skillRow["NumTimesIncreased"] == 2, skillRow["Skills.Cost.3"]  ) ) ) ) )
                dev_Points.Total = unlist( dev_Points.Total - skill.Cost ) 
                skills.Increase.Record.DF[as.numeric(skill),"NumTimesIncreased"] = skillRow["NumTimesIncreased"] + 1
            } else{
return( choose_Skills_To_Increase_Each_Level(skills.Increase.Record.DF, dev_Points.Total, char.Class, background_Options.Benefits) )
}
            
        }
}

if( dev_Points.Total > 0 ) cat("\nYou have", dev_Points.Total, "development points left - a buffer for when you get older.\n\nCustomise your character with them!\n\nOr don't.\n\nI'm a string, not a cop.\n\n")
return( list( skills.Increase.Record.DF, dev_Points.Total ) )
    
}

#' A LEVEL FUNCTION
#'
#' PLAYER ASSIGNS SKILLS FROM HOBBY POINTS AND DEVELOPMENT POINTS FOR LEVEL 0
#' @param skills.Increase.Record.DF A DATAFRAME CONTAINING WHICH SKILLS HAVE BEEN INCREASED AND HOW OFTEN.
#' @param char.Class A STRING WHICH IS WHAT CLASS THEY ARE (DETERMINES SKILL FACTS)
#' @param hobby.Points AN INTEGER WHICH IS HOW MANY HOBBY POINTS THEY HAVE.
#' @param background_Options.Benefits LIST CONTAINING ALL BACKGROUND OPTION BENEFITS A PLAYER HAS TAKEN
#' @param background.Social.Skills.Normal DATAFRAME OF THE SKILLS ASSIGNED AUTOMATICALLY FROM SOCIETY
#' @param background.Social.Skills.ANY DATAFRAME OF THE SKILLS ASSIGNED BY THE PLAYER FROM SOCIETY
#' @param background.Occupation.Skills.Normal DATAFRAME OF THE SKILLS ASSIGNED AUTOMATICALLY FROM PARENTS' OCCUPATION
#' @param background.Occupation.Skills.ANY DATAFRAME OF THE SKILLS ASSIGNED BY THE PLAYER FROM PARENTS' OCCUPATION
#' @param background.Skills.Assign TRUE/FALSE - HAVE THE BACKGROUND SKILLS ALREADY BEEN ASSIGNED
#' @param skills_Groups GROUPS OF SKILLS THAT SHARE HALF RANKS
#' @concept skills level hobby hobbies background
#' @export
#' @examples
#' level_0_Skills()

level_0_Skills <-
function(skills.Increase.Record.DF = -1,  char.Class = -1,  hobby.Points = -1,  background_Options.Benefits = -1,  background.Social.Skills.Normal = -1,  background.Social.Skills.ANY = -1,  
                            background.Occupation.Skills.Normal = -1,  background.Occupation.Skills.ANY = -1,  background.Skills.Assign = TRUE, 
                            skills_Groups = -1){ 

if(unlist(skills.Increase.Record.DF)[1] == -1 & unlist(char.Class)[1] == -1 & unlist(hobby.Points)[1] == -1 & unlist(background_Options.Benefits)[1] == -1 & unlist(background.Social.Skills.Normal)[1] == -1 &
    unlist(background.Social.Skills.ANY)[1] == -1 & unlist(background.Occupation.Skills.Normal)[1] == -1 & unlist(background.Occupation.Skills.ANY)[1] == -1 & unlist(background.Skills.Assign)[1] == TRUE &
    unlist(skills_Groups)[1] == -1) return(-1)   
#returns:   A LIST OF ONE OBJECT: 1) THE COMPLETED DATAFRAME OF SKILL INCREASES.
skill.Num <- nrow(skills.Increase.Record.DF)
skill.Names <- skills.Increase.Record.DF[,1]
    skill.Chosen.Name <- NULL
continue = TRUE
    
    if( background.Skills.Assign == TRUE ){
        #ADD ON SKILLS FROM OCCUPATION
        skills.Increase.Record.DF[ skills.Increase.Record.DF[,"Skills.Name"] %in% background.Occupation.Skills.Normal[,1],"NumTimesIncreased"] = skills.Increase.Record.DF[ skills.Increase.Record.DF[,"Skills.Name"] %in% background.Occupation.Skills.Normal[,1],"NumTimesIncreased"] + background.Occupation.Skills.Normal[,2]
        #ADDS ON SKILLS FROM SOCIETY
        if( nrow( background.Social.Skills.Normal ) > 0 ){
            for(i in 1:nrow(background.Social.Skills.Normal)){
                bSSN <- background.Social.Skills.Normal[i,]
                skills.Increase.Record.DF[ skills.Increase.Record.DF[,"Skills.Name"] == bSSN[,1],"NumTimesIncreased" ] = 
                    skills.Increase.Record.DF[ skills.Increase.Record.DF[,"Skills.Name"] == bSSN[,1],"NumTimesIncreased" ] + bSSN[,2]
            }
        }
        
        ANY.skills <- assign_ANY_Skills(background.Occupation.Skills.ANY, background.Social.Skills.ANY, data.frame(), skills.Increase.Record.DF)
                                    
        skills.Increase.Record.DF[,"NumTimesIncreased"] <- ANY.skills[,"NumTimesIncreased"]
        background.Skills.Assign = FALSE
    }
while( hobby.Points > 0 && continue == TRUE ){
        
        skills.Increase.Record.DF.OUT <- skills_DF_OUT_CREATE(skills.Increase.Record.DF)
print(skills.Increase.Record.DF.OUT[,-2])
        cat("\n\nYou have ", hobby.Points, " Hobby Points left to spend.\n\nEnter the name of the skill you wish to increase:\n\tOR\nChoose a number between 1 and ", skill.Num,":\n\tOR\nEnter STOP to exit:\n\n",sep="")
if (interactive() ){ skill <- readline()} else{ skill <- readLines("stdin",1)  }

if( is.na( suppressWarnings( as.numeric( skill ) ) ) ) {

if( skill %in% skill.Names ){

                if( grepl("Directed Spells", skill) ){
                    canIncrease <- check_Directed_Spells_Available( skill, skills.Increase.Record.DF )
                    if( !canIncrease ){
                        return( level_0_Skills(skills.Increase.Record.DF, char.Class, hobby.Points, background_Options.Benefits, background.Social.Skills.Normal, background.Social.Skills.ANY, background.Occupation.Skills.Normal, background.Occupation.Skills.ANY, background.Skills.Assign) )
                    }
                }
                
                if( skill == "Control" & !( "Lycanthropy" %in% unlist(background_Options.Benefits) ) ){
                    cat(paste0( "\nYou can only choose that skill if you are a Lycanthrope. Please choose another.\n" ))
                    if (interactive() ){ readline()} else{ readLines("stdin",1)  }
                    return( level_0_Skills(skills.Increase.Record.DF, char.Class, hobby.Points, background_Options.Benefits, background.Social.Skills.Normal, background.Social.Skills.ANY, background.Occupation.Skills.Normal, background.Occupation.Skills.ANY, background.Skills.Assign) )
                }
                
skillRow = skills.Increase.Record.DF[skills.Increase.Record.DF[,1] == skill,]
if( skillRow["NumTimesIncreased"] < skillRow["NumTimesCanIncrease"] ){
                    skillRow["NumTimesIncreased"] = skillRow["NumTimesIncreased"]  + 1
                    hobby.Points = hobby.Points - 1
                    skills.Increase.Record.DF[skills.Increase.Record.DF[,1] == skill,] = skillRow
} else{
cat( "\nYou have already increased \"", skill, "\" as many times you can in a level.\n\nChoose something else to increase.\n\nPress enter to continue.\n", sep="" )
if (interactive() ){ readline()} else{ readLines("stdin",1)  }
return( level_0_Skills(skills.Increase.Record.DF, char.Class, hobby.Points, background_Options.Benefits, background.Social.Skills.Normal, background.Social.Skills.ANY, background.Occupation.Skills.Normal, background.Occupation.Skills.ANY, background.Skills.Assign) )
}

} else if( skill == "STOP" || skill == "stop" || skill == "S" || skill == "s" ){
break;
            }  else{
cat(paste0( "\nThe name must be from\n", paste0(skill.Names,collapse=", "), "\n\nPress enter to continue.\n" ))
if (interactive() ){ readline()} else{ readLines("stdin",1)  }
return( level_0_Skills(skills.Increase.Record.DF, char.Class, hobby.Points, background_Options.Benefits, background.Social.Skills.Normal, background.Social.Skills.ANY, background.Occupation.Skills.Normal, background.Occupation.Skills.ANY, background.Skills.Assign) )
}
} else{
            skill <- as.numeric(skill)
if( 1 <= skill && skill <= skill.Num ){

                skill.Chosen.Name <- skill.Names[skill]
                
                if( grepl("Directed Spells", skill.Chosen.Name) ){
                    canIncrease <- check_Directed_Spells_Available( skill.Chosen.Name, skills.Increase.Record.DF )
                    if( !canIncrease ){
                        return( level_0_Skills(skills.Increase.Record.DF, char.Class, hobby.Points, background_Options.Benefits, background.Social.Skills.Normal, background.Social.Skills.ANY, background.Occupation.Skills.Normal, background.Occupation.Skills.ANY, background.Skills.Assign) )
                    }
                }
                
                if( skill.Chosen.Name == "Control" & !( "Lycanthropy" %in% unlist(background_Options.Benefits) ) ){
                    cat(paste0( "\nYou can only choose that skill if you are a Lycanthrope. Please choose another.\n" ))
                    if (interactive() ){ readline()} else{ readLines("stdin",1)  }
                    return( level_0_Skills(skills.Increase.Record.DF, char.Class, hobby.Points, background_Options.Benefits, background.Social.Skills.Normal, background.Social.Skills.ANY, background.Occupation.Skills.Normal, background.Occupation.Skills.ANY, background.Skills.Assign) )
                }
                
skillRow = skills.Increase.Record.DF[skill,]
if( skillRow["NumTimesIncreased"] < skillRow["NumTimesCanIncrease"] ){
                    skillRow["NumTimesIncreased"] = skillRow["NumTimesIncreased"]  + 1
                    hobby.Points = hobby.Points - 1
                    skills.Increase.Record.DF[skill,] = skillRow
} else{
cat( "\nYou have already increased \"", skill.Chosen.Name, "\" as many times you can in a level.\n\nChoose something else to increase.\n\nPress enter to continue.\n", sep="" )
if (interactive() ){ readline()} else{ readLines("stdin",1)  }
return( level_0_Skills(skills.Increase.Record.DF, char.Class, hobby.Points, background_Options.Benefits, background.Social.Skills.Normal, background.Social.Skills.ANY, background.Occupation.Skills.Normal, background.Occupation.Skills.ANY, background.Skills.Assign) )
}

} else{
cat(paste0( "\nThe number must be between 1 and \n", skill.Num, "\n\nPress enter to continue.\n" ))
if (interactive() ){ readline()} else{ readLines("stdin",1)  }
return( level_0_Skills(skills.Increase.Record.DF, char.Class, hobby.Points, background_Options.Benefits, background.Social.Skills.Normal, background.Social.Skills.ANY, background.Occupation.Skills.Normal, background.Occupation.Skills.ANY, background.Skills.Assign) )
}

        }
        
}
    
cat( "\nYou have spent all your points for level 0.\n" )
    if (interactive() ){ readline("Press enter to continue.")} else{ cat("Press enter to continue."); readLines("stdin",1)  }
return( list( skills.Increase.Record.DF ) )
    print("Eh?")
}

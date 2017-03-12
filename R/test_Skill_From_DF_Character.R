#' A SKILL INCREASE FUNCTION
#'
#' FUNCTION TO CHECK IF CHOSEN SKILL CAN BE INCREASED
#' @param skill SKILL YOU WANT TO INCREASE
#' @param skill.Names NAMES OF ALL SKILLS  
#' @param skills.Increase.Record.DF DATA FRAME OF THE SKILLS, SKILL COSTS, NUMBER OF TIMES YOU CAN INCREASE, NUMBER OF TIMES YOU HAVE INCREASED
#' @param background_Options.Benefits LIST CONTAINING ALL BACKGROUND OPTION BENEFITS A PLAYER HAS TAKEN
#' @param dev_Points.Total NUMERIC VARIABLE WITH TOTAL DEVELOPMENT POINTS PLAYER HAS LEFT FOR ASSIGNING SKILLS
#' @concept test skill
#' @export
#' @examples
#' test_Skill_From_DF_Character()

test_Skill_From_DF_Character <-
function(skill = -1,  skill.Names = -1,  skills.Increase.Record.DF = -1,  background_Options.Benefits = -1,  dev_Points.Total = -1){    

if(unlist(skill)[1] == -1 & unlist(skill.Names)[1] == -1 & unlist(skills.Increase.Record.DF)[1] == -1 &
	unlist(background_Options.Benefits)[1] == -1 & unlist(dev_Points.Total)[1] == -1) return(-1)
#returns:                       TRUE/FALSE DEPENDING ON WHETHER OR NOT YOU CAN INCREASE THE SKILL    
    if( skill %in% skill.Names ){
                    
        if( grepl("Directed Spells", skill) ){
            canIncrease <- check_Directed_Spells_Available( skill, skills.Increase.Record.DF )
            if( !canIncrease ){
                return( FALSE )
            }
        }
        
        if( skill == "Control" & !( "Lycanthropy" %in% unlist(background_Options.Benefits) ) ){
            cat(paste0( "\nYou can only choose that skill if you are a Lycanthrope. Please choose another.\n" ))
            if (interactive() ){ readline()} else{ readLines("stdin",1)  }
            return( FALSE )
        }
        
        skillRow = skills.Increase.Record.DF[skills.Increase.Record.DF[,1] == skill,]
        if( skillRow["NumTimesIncreased"] < skillRow["NumTimesCanIncrease"] ){
            if( skillRow["NumTimesCanIncrease"] == 55 ){
                if( dev_Points.Total >= skillRow["Skills.Cost.1"] ){
                    return( TRUE )
                } else{
                    cat( "\nYou don't have enought Development Points to increase \"", skill, "\"\n\nChoose something else to increase.\n\nYou have ",dev_Points.Total," left to spend\n\nPress enter to continue.\n", sep="" )
                    if (interactive() ){ readline()} else{ readLines("stdin",1)  }
                    return( FALSE )
                }
            }else{
                skill.Cost = unlist( ifelse( skillRow["NumTimesIncreased"] == 0, skillRow["Skills.Cost.1"], 
                                ifelse( skillRow["NumTimesIncreased"] == 1, skillRow["Skills.Cost.2"], 
                                ifelse( skillRow["NumTimesIncreased"] == 2, skillRow["Skills.Cost.3"]  ) ) ) )
                if( dev_Points.Total >= skill.Cost ){
                    return( TRUE )
                } else{
                    cat( "\nYou don't have enought Development Points to increase \"", skill, "\"\n\nChoose something else to increase.\n\nYou have ",dev_Points.Total," left to spend\n\nPress enter to continue.\n", sep="" )
                    if (interactive() ){ readline()} else{ readLines("stdin",1)  }
                    return( FALSE )
                }
            }
        } else{
            cat( "\nYou have already increased \"", skill, "\" as many times you can in a level.\n\nChoose something else to increase.\n\nPress enter to continue.\n", sep="" )
            if (interactive() ){ readline()} else{ readLines("stdin",1)  }
            return( FALSE )
        }
    } else{
        cat(paste0( "\nThe name must be from\n", paste0(skill.Names,collapse=", "), "\n\nPress enter to continue.\n" ))
        if (interactive() ){ readline()} else{ readLines("stdin",1)  }
        return( FALSE )
    }
    
}

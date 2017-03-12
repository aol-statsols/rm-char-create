#' A SKILL FUNCTION
#'
#' FUNCTION TO LET THE USER CHOOSE WHICH SKILL THEY WANT TO INCREASE WITH BACKGROUND POINTS
#' @param skills.DF DATA FRAME OF THE SKILLS, SKILL COSTS, NUMBER OF TIMES YOU CAN INCREASE, NUMBER OF TIMES YOU HAVE INCREASED
#' @param background_Options.Benefits LIST CONTAINING ALL BACKGROUND OPTION BENEFITS A PLAYER HAS TAKEN
#' @concept skills level 
#' @export
#' @examples
#' choose_Skill_From_DF()

choose_Skill_From_DF <-
function(skills.DF = -1, background_Options.Benefits = -1){ 

if( length(background_Options.Benefits) == 0 & unlist(skills.DF)[1] == -1) return(-1)
if( length(background_Options.Benefits) > 0 ) if(unlist(skills.DF)[1] == -1 & unlist(background_Options.Benefits)[1] == -1) return(-1)
#returns EITHER THE SKILL NAME OR FORCES THE PLAYER TO CHOOSE ANOTHER IF THEY CAN'T TAKE THAT ONE
    skill.Num = nrow(skills.DF)
    skill.Names <- skills.DF[,2]
    print(skills.DF)
    cat("\n\nEnter the name of the skill you wish to increase:\n\nOR\n\nChoose a number between 1 and ", skill.Num,":\n\nOR\n\nEnter STOP to exit:",sep="")

    skill <- if (interactive() ){ readline()} else{ readLines("stdin",1)  }
    
    if( is.na( suppressWarnings( as.numeric( skill ) ) ) ) {
        
        if( grepl("Directed Spells", skill) ){
            canIncrease <- check_Directed_Spells_Available( skill, skills.DF )
            if( !canIncrease ){
                choose_Skill_From_DF(skills.DF, background_Options.Benefits)
            }
        }

        if( skill %in% skill.Names ){
            
            if( skill == "Control" & !( "Lycanthropy" %in% unlist(background_Options.Benefits) ) ){
                cat(paste0( "\nYou can only choose that skill if you are a Lycanthrope. Please choose another.\n" ))
                if (interactive() ){readline()} else{ readLines("stdin",1)  }
                return( choose_Skill_From_DF(skills.DF, background_Options.Benefits) )
            }
            
            return(skill)
                   
        }else{
            cat(paste0( "\nThe name must be from\n", paste0(skill.Names,collapse=", "), "\n\nPress enter to continue.\n" ))
            if (interactive() ){ readline()} else{ readLines("stdin",1)  }
            return( choose_Skill_From_DF(skills.DF, background_Options.Benefits) )
        }
    
    } else{
        skill <- as.numeric(skill)
        if( 1 <= skill && skill <= skill.Num ){
            
            if( grepl("Directed Spells", skill) ){
                canIncrease <- check_Directed_Spells_Available( skill, skills.DF )
                if( !canIncrease ){
                    choose_Skill_From_DF(skills.DF, background_Options.Benefits)
                }
            }
            
            if( skill.Names[skill] == "Control" & !( "Lycanthropy" %in% unlist(background_Options.Benefits) ) ){
                cat(paste0( "\nYou can only choose that skill if you are a Lycanthrope. Please choose another.\n" ))
                if (interactive() ){ readline()} else{ readLines("stdin",1)  }
                return( choose_Skill_From_DF(skills.DF, background_Options.Benefits) )
            }
            
            return(skill.Names[skill])
                  
        } else{
            cat(paste0( "\nThe number must be between \n\t1 and ", skill.Num, "\n\nPress enter to continue.\n" ))
            if (interactive() ){ readline()} else{ readLines("stdin",1)  }
            return( choose_Skill_From_DF(skills.DF, background_Options.Benefits) )
        }
    
    }
    
}

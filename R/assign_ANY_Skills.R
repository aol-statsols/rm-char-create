#' A SKILL FUNCTION
#'
#' FUNCTION WHICH ASSIGNS SKILLS THAT COME FROM "ANY" OR "UP TO"
#' @param background.Occupation.Skills.ANY DATAFRAME OF THE SKILLS ASSIGNED BY THE PLAYER FROM PARENTS' OCCUPATION. Defaults to empty data frame.
#' @param background.Social.Skills.ANY DATAFRAME OF THE SKILLS ASSIGNED BY THE PLAYER FROM PARENTS' SOCIETY AND PLACE WITHIN IT. Defaults to empty data frame.
#' @param tp.Skills.ANY DATAFRAME OF THE SKILLS ASSIGNED BY THE PLAYER FROM THE TRAINING PACKAGE. Defaults to empty data frame.
#' @param skills.Increase.Record.DF DATAFRAME OF SKILLS WHICH CAN BE INCREASED
#' @concept skill increases
#' @export
#' @examples
#' assign_ANY_Skills()

assign_ANY_Skills <-
function(background.Occupation.Skills.ANY = data.frame(), background.Social.Skills.ANY = data.frame(), 
                                tp.Skills.ANY = data.frame(), skills.Increase.Record.DF = data.frame()){ 

if( nrow(background.Occupation.Skills.ANY) == 0 & nrow(background.Social.Skills.ANY) == 0 & nrow(tp.Skills.ANY) == 0 & nrow(skills.Increase.Record.DF) == 0 ){
    return(-1)
}            
                    print(1)
skills.ANY.Output <- NULL
if( nrow( background.Occupation.Skills.ANY ) ){
skills.ANY.Output <- rbind( skills.ANY.Output, background.Occupation.Skills.ANY )
}
if( nrow ( background.Social.Skills.ANY ) ){
if( !(is.null(skills.ANY.Output)) ) colnames( skills.ANY.Output ) <- colnames(background.Social.Skills.ANY)
for( i in 1:nrow(background.Social.Skills.ANY ) ){
if( background.Social.Skills.ANY[i,1] %in% skills.ANY.Output[,1] ){
skills.ANY.Output[i,2] = skills.ANY.Output[i,2] + background.Social.Skills.ANY[i,2] 
} else{
skills.ANY.Output <- rbind( skills.ANY.Output, background.Social.Skills.ANY[i,]  )
}
}
}
if( nrow ( tp.Skills.ANY ) ){
        tp.Skills.ANY[,1] <- gsub("OWN REALM", "ANY", tp.Skills.ANY[,1])
if( !(is.null(skills.ANY.Output)) ) colnames( skills.ANY.Output ) <- c( "Skills.Name","Skills.Ranks" )
for( i in 1:nrow(tp.Skills.ANY) ){
if( tp.Skills.ANY[i,1] %in% skills.ANY.Output[,1] ){
skills.ANY.Output[skills.ANY.Output[,1] == tp.Skills.ANY[i,1],2] = skills.ANY.Output[skills.ANY.Output[,1] == tp.Skills.ANY[i,1],2] + tp.Skills.ANY[i,2] 
} else{
skills.ANY.Output <- rbind( skills.ANY.Output, tp.Skills.ANY[i,]  )
}
}
}

colnames( skills.ANY.Output ) <- c( "Skill.Name","Skill.Rank" )
skills.Increase.Record.DF.Subset <- NULL

    skills.Increased.UpTo <- NULL
            
for( i in 1:nrow(skills.ANY.Output)) {
        
        if( grepl( " OR ", skills.ANY.Output[i,1] ) ){
            individual.ANY <- unlist( strsplit( skills.ANY.Output[i,1], " OR " ) ) 
            player.Choice <- choose_Generic( "skill list you wish to increase", individual.ANY )
            skills.ANY.Output[i,1] <- player.Choice
        }
        
        if(grepl( ": ANY.*",skills.ANY.Output[i,1] ) ){
            #print(skills.ANY.Output[i,1])
            skills.Increase.Record.DF.Subset <- skills.Increase.Record.DF[ grepl( gsub( ": ANY.*", "", skills.ANY.Output[i,1] ), skills.Increase.Record.DF[,"Skills.Name"] ), c(1,5,6) ]
            rownames(skills.Increase.Record.DF.Subset) = NULL
            if( (as.character(skills.Increase.Record.DF.Subset[1,1]) == "Weapons: Melee: One Handed Edged") && 
                (as.character(skills.Increase.Record.DF.Subset[nrow(skills.Increase.Record.DF.Subset),1]) == "Craft: Weapons") ){
                    skills.Increase.Record.DF.Subset <- skills.Increase.Record.DF.Subset[-(nrow(skills.Increase.Record.DF.Subset)),]
            }
            skill.Num <- nrow(skills.Increase.Record.DF.Subset)
            skill.Names <- skills.Increase.Record.DF.Subset[,1]
            
            while( skills.ANY.Output[i,2] > 0 ){
                print(skills.Increase.Record.DF.Subset)
                cat("\n\nYou have", skills.ANY.Output[i,2], "ANY Points left to spend.\n")
                cat("\n\nEnter the name of the skill you wish to increase:\n\nOR\n\nChoose a number between 1 and ", skill.Num,":\t",sep="")
                if (interactive() ){ skill <- readline()} else{ skill <- readLines("stdin",1)  }
                
                if( is.na( suppressWarnings( as.numeric( skill ) ) ) ) {
                    
                    if( skill %in% skill.Names ){
                        
                        skillRow = skills.Increase.Record.DF.Subset[skills.Increase.Record.DF.Subset[,1] == skill,]
                        if(skillRow["NumTimesIncreased"] < skillRow["NumTimesCanIncrease"] ){
                            skillRow["NumTimesIncreased"] = skillRow["NumTimesIncreased"]  + 1
                            skills.ANY.Output[i,2] = unlist( skills.ANY.Output[i,2] - 1 ) 
                            skills.Increase.Record.DF.Subset[skills.Increase.Record.DF.Subset[,1] == skill,] = skillRow
                        } else{
                            cat(paste0( "\nYou have already increased that skill as many times as you can. \nChoose a different skill.\n\n" ))
                            if (interactive() ){ readline("Press enter to continue.")} else{ cat("Press enter to continue."); readLines("stdin",1)  }
                        }
                        
                    }else{
                        cat(paste0( "\nThe name must be from\n", paste0(skill.Names,collapse=", "), "\n\nPress enter to continue.\n" ))
                        if (interactive() ){ readline()} else{ readLines("stdin",1)  }
                    }
                
                } else{
                    skill <- as.numeric(skill)
                    if( 1 <= skill && skill <= skill.Num ){
                        
                        skill.Chosen.Name <- skill.Names[skill]
                        skillRow = skills.Increase.Record.DF.Subset[skill,]
                       
                        if(skillRow["NumTimesIncreased"] < skillRow["NumTimesCanIncrease"] ){
                            skillRow["NumTimesIncreased"] = skillRow["NumTimesIncreased"]  + 1
                            skills.ANY.Output[i,2] = unlist( skills.ANY.Output[i,2] - 1 )
                            skills.Increase.Record.DF.Subset[skill,] = skillRow
                        } else{
                            cat(paste0( "\nYou have already increased that skill as many times as you can. \nChoose a different skill.\n\n" ))
                            if (interactive() ){ readline("Press enter to continue.")} else{ cat("Press enter to continue."); readLines("stdin",1)  }
                        }
                    
                    } else{
                        cat(paste0( "\nThe number must be between\n1 and ", skill.Num, "\nPress enter to continue.\n" ))
                        if (interactive() ){ readline()} else{ readLines("stdin",1)  }
                    }
                
                }
            }
            
            skills.Increase.Record.DF[ grepl( gsub( ": ANY.*", "", skills.ANY.Output[i,1] ), skills.Increase.Record.DF[,"Skills.Name"] ), c(1,5,6) ] <- skills.Increase.Record.DF.Subset
} else if( grepl( "Up to",skills.ANY.Output[i,1] ) ){
            skills.Increase.Record.DF.Subset <- skills.Increase.Record.DF[ grepl( gsub( ": Up to.*", "", skills.ANY.Output[i,1] ), skills.Increase.Record.DF[,"Skills.Name"] ), c(1,5,6) ]
            rownames(skills.Increase.Record.DF.Subset) = NULL
            skill.Num <- nrow(skills.Increase.Record.DF.Subset)
            skill.Names <- skills.Increase.Record.DF.Subset[,1]
            
            skillsCanIncrease <- as.numeric( gsub( "(.*: Up to )([0-9])( .*)", "\\2", skills.ANY.Output[i,1] ) )
            
            skills.Increased.UpTo.This_Time <- NULL
  
            while( skills.ANY.Output[i,2] > 0 ){
                print(skills.Increase.Record.DF.Subset)
                cat("\n\nYou have", skills.ANY.Output[i,2], "Points left to spend, on up to", skillsCanIncrease," skills.")
                cat("\n\nEnter the name of the skill you wish to increase:\n\nOR\n\nChoose a number between 1 and ", skill.Num,":\t",sep="")
                if (interactive() ){ skill <- readline()} else{ skill <- readLines("stdin",1)  }
                
                if( is.na( suppressWarnings( as.numeric( skill ) ) ) ) {
                    
                    if( skill %in% skill.Names ){

if( skill %in% skills.Increased.UpTo & !(skill %in% skills.Increased.UpTo.This_Time) ){
cat(paste0( "\nYou already increased that skill with other UP TO points. Try again.\n\nPress enter to continue.\n" ))
if (interactive() ){ readline()} else{ readLines("stdin",1)  }
} else{
if( !(skill %in% skills.Increased.UpTo.This_Time) ) skills.Increased.UpTo.This_Time <- c(skills.Increased.UpTo.This_Time, skill)
if( length( skills.Increased.UpTo.This_Time ) <= skillsCanIncrease ){ 
skillRow = skills.Increase.Record.DF.Subset[skills.Increase.Record.DF.Subset[,1] == skill,]
                                if(skillRow["NumTimesIncreased"] < skillRow["NumTimesCanIncrease"] ){
                                    skillRow["NumTimesIncreased"] = skillRow["NumTimesIncreased"]  + 1
                                    skills.ANY.Output[i,2] = unlist( skills.ANY.Output[i,2] - 1 ) 
                                    skills.Increase.Record.DF.Subset[skills.Increase.Record.DF.Subset[,1] == skill,] = skillRow
                                } else{
                                    cat(paste0( "\nYou have already increased that skill as many times as you can. \nChoose a different skill.\n\n" ))
                                    if (interactive() ){ readline("Press enter to continue.")} else{ cat("Press enter to continue."); readLines("stdin",1)  }
                                }
}
else{
skills.Increased.UpTo.This_Time <- skills.Increased.UpTo.This_Time[-length(skills.Increased.UpTo.This_Time)]
cat(paste0( "\nThe skill must be from\n\n", paste0(skills.Increased.UpTo.This_Time,collapse=", "), "\n\nPress enter to continue.\n" ))
if (interactive() ){ readline()} else{ readLines("stdin",1)  }
}
}
                    }else{
                        cat(paste0( "\nThe name must be from\n", paste0(skill.Names,collapse=", "), "\n\nPress enter to continue.\n" ))
                        if (interactive() ){ readline()} else{ readLines("stdin",1)  }
                    }
                
                } else{
                    skill <- as.numeric(skill)
                    if( 1 <= skill && skill <= skill.Num ){
                        
skill.Chosen.Name <- skill.Names[skill]

if( skill.Chosen.Name %in% skills.Increased.UpTo & !(skill.Chosen.Name %in% skills.Increased.UpTo.This_Time) ){
cat(paste0( "\nYou already increased that skill with other UP TO points. Try again.\n\nPress enter to continue.\n" ))
if (interactive() ){ readline()} else{ readLines("stdin",1)  }
} else{
if( !(skill.Chosen.Name %in% skills.Increased.UpTo.This_Time) ) skills.Increased.UpTo.This_Time <- c(skills.Increased.UpTo.This_Time, skill.Names[skill])
if( length( skills.Increased.UpTo.This_Time ) <= skillsCanIncrease ){ 
                                
skillRow = skills.Increase.Record.DF.Subset[skill,]
                                if(skillRow["NumTimesIncreased"] < skillRow["NumTimesCanIncrease"] ){
                                    skillRow["NumTimesIncreased"] = skillRow["NumTimesIncreased"]  + 1
                                    skills.ANY.Output[i,2] = unlist( skills.ANY.Output[i,2] - 1 )
                                    skills.Increase.Record.DF.Subset[skill,] = skillRow
                                } else{
                                    cat(paste0( "\nYou have already increased that skill as many times as you can. \nChoose a different skill.\n\n" ))
                                    if (interactive() ){ readline("Press enter to continue.")} else{ cat("Press enter to continue."); readLines("stdin",1)  }
                                }
}
else{
skills.Increased.UpTo.This_Time <- skills.Increased.UpTo.This_Time[-length(skills.Increased.UpTo.This_Time)]
cat(paste0( "\nThe skill must be from\n\n", paste0(skills.Increased.UpTo.This_Time,collapse=", "), "\n\nPress enter to continue.\n" ))
if (interactive() ){ readline()} else{ readLines("stdin",1)  }
}
}

                    } else{
                        cat(paste0( "\nThe number must be between\n1 and ", skill.Num, "\nPress enter to continue.\n" ))
                        if (interactive() ){ readline()} else{ readLines("stdin",1)  }
                    }
                
                }
            }

skills.Increased.UpTo <- c( skills.Increased.UpTo, skills.Increased.UpTo.This_Time )
            
            skills.Increase.Record.DF[ grepl( gsub( ": Up to.*", "", skills.ANY.Output[i,1] ), skills.Increase.Record.DF[,"Skills.Name"] ), c(1,5,6) ] <- skills.Increase.Record.DF.Subset
}
}

return(skills.Increase.Record.DF)
}

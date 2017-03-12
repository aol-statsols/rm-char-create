#' A SKILL FUNCTION
#'
#' ASSIGNS LEFTOVER DEVELOPMENT POINTS FROM EACH LEVEL
#' @param char.Class STRING CONTAINING CHARACTER'S PROFESSION
#' @param dev_Points.Level DEVELPOMENT POINTS FOR EACH LEVEL
#' @param dev_Points.Spent.Level DEVELPOMENT POINTS SPENT EACH LEVEL (MUST BE LESS THAN ABOVE)
#' @param skills.Each.Level SKILLS INCREASED EACH LEVEL
#' @param background_Options.Benefits LIST CONTAINING ALL BACKGROUND OPTION BENEFITS A PLAYER HAS TAKEN
#' @concept skill increases
#' @export
#' @examples
#' assign_Leftover_DP()

assign_Leftover_DP <-
function(char.Class = NULL,dev_Points.Level = NULL,dev_Points.Spent.Level = NULL,skills.Each.Level = NULL,background_Options.Benefits = NULL){ 

if( is.null(char.Class) & is.null(dev_Points.Level) & is.null(dev_Points.Spent.Level) & is.null(skills.Each.Level) & is.null(background_Options.Benefits) ) return(-1)    
#returns:    
    dp.Leftover <- dev_Points.Level - dev_Points.Spent.Level
    skills.Increase.Record.DF.New <- list( list( skills.Each.Level, dp.Leftover) ) 
    
    for( i in 1:(length(dp.Leftover) - 1) ){
        cat("\nYou are now going to finish leveling skills for level ", (i+1), ".\n\n", sep="")
        if (interactive() ){ readline(prompt=paste0("Press Enter to continue"))} else{ cat("Press Enter to continue"); readLines("stdin",1)  }
        if( dp.Leftover[(i+1)] > 0 ){
            skills.Increase.Record.DF.New[[(length(skills.Increase.Record.DF.New) + 1)]] <- choose_Skills_To_Increase_Each_Level(skills.Each.Level, dp.Leftover[(i+1)], char.Class, background_Options.Benefits)
        } else if( dp.Leftover[(i+1)] < 0 ){
            skills.Increase.Record.DF.New[[(length(skills.Increase.Record.DF.New) + 1)]] <- change_Skills_To_Increase_Each_Level(skills.Each.Level,dp.Leftover[(i+1)],char.Class, background_Options.Benefits)
            skills.Each.Level <- skills.Increase.Record.DF.New[[(length(skills.Increase.Record.DF.New))]][[1]]
            dp.Leftover[i:length(dp.Leftover)] = skills.Increase.Record.DF.New[[(length(skills.Increase.Record.DF.New))]][[2]]
        } else{
            skills.Increase.Record.DF.New[[(length(skills.Increase.Record.DF.New) + 1)]] <- skills.Increase.Record.DF.New[[(length(skills.Increase.Record.DF.New))]]
            cat("\nYou already spent all your development points for this level.\n\nPress enter to see your skill record.\n", sep="")
            if (interactive() ){ readline(prompt=paste0("Press Enter to continue"))} else{ cat("Press Enter to continue"); readLines("stdin",1)  }
            tmp <- skills.Increase.Record.DF.New[[(length(skills.Increase.Record.DF.New))]][[1]]
            tmp[,"TotalRanks"] = tmp[,"TotalRanks"] + tmp[,"NumTimesIncreased"]
            print(tmp)
            if (interactive() ){ readline(prompt=paste0("Press Enter to continue"))} else{ cat("Press Enter to continue"); readLines("stdin",1)  }
        }
        skills.Increase.Record.DF.New[[(length(skills.Increase.Record.DF.New))]][[1]][,"TotalRanks"] = skills.Increase.Record.DF.New[[(length(skills.Increase.Record.DF.New))]][[1]][,"TotalRanks"] +
                                                                                                        skills.Increase.Record.DF.New[[(length(skills.Increase.Record.DF.New))]][[1]][,"NumTimesIncreased"]
        
        skills.Each.Level[,"TotalRanks"] <- skills.Each.Level[,"TotalRanks"] + skills.Increase.Record.DF.New[[(length(skills.Increase.Record.DF.New))]][[1]][,"NumTimesIncreased"]
    }
    
    return(skills.Increase.Record.DF.New)
}

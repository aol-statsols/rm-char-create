#' A BACKGROUND OPTION FUNCTION
#'
#' LETS THE PLAYER CHOOSE THEIR BACKGROUND OPTIONS AND GENERATES THE BENEFITS
#' @param background_Options.Benefits.ROLLING LIST CONTAINING ALL BACKGROUND OPTION BENEFITS A PLAYER HAS TAKEN WHICH REQUIRED ROLLING
#' @param background_Options.Benefits.NO.ROLLING LIST CONTAINING ALL BACKGROUND OPTION BENEFITS A PLAYER HAS TAKEN WHICH DID NOT REQUIRE ROLLING
#' @param background.Option.Points NUMERIC VARIABLE CONTAINING THE NUMBER OF BACKGROUND OPTION POINTS A CHARACTER HAS
#' @param stats.Tmp.Pot MATRIX OF TEMPORARY AND POTENTIAL STATS
#' @param stats VECTOR OF ALL 10 STATS
#' @param background_Options DF OF BACKGROUND OPTIONS AVAILABLE
#' @param skills.Secondary.DF DATAFRAME OF THE SECONDARY SKILLS
#' @param skills.Primary.DF DATAFRAME OF THE PRIMARY SKILLS
#' @param skills.Increase.Record.DF DATAFRAME OF SKILLS YOU CAN INCREASE WHICH WILL GENERATE ONES USED TO RECORD ANY INCREASES
#' @param background_Special_Tables_List TABLE CONTAINING ALL SPECIAL "ROLL ON" TABLE THE CHARACTER COULD USE
#' @param flaws_Table TABLE CONTAINING ALL FLAWS THE CHARACTER COULD TAKE
#' @param background.Skills.Increase.Record.DF DATAFRAME OF SKILLS YOU CAN INCREASE WHICH WILL GENERATE ONES USED TO RECORD ANY INCREASES AT LEVEL 0
#' @concept  background options
#' @export
#' @examples
#' background_Options_Assignment_NO_ROLLING()

background_Options_Assignment_NO_ROLLING <-
function( background_Options.Benefits.ROLLING = -1, background_Options.Benefits.NO.ROLLING = -1, background.Option.Points = -1, stats.Tmp.Pot = -1, stats = -1, background_Options = -1,
            skills.Primary.DF = -1, skills.Secondary.DF = -1, skills.Increase.Record.DF = -1, background_Special_Tables_List = -1, 
            flaws_Table = -1, background.Skills.Increase.Record.DF = -1){ 
                                                                                            
if(unlist(background_Options.Benefits.ROLLING)[1] == -1 & unlist(background_Options.Benefits.NO.ROLLING)[1] == -1 & unlist(background.Option.Points)[1] == -1 &
	unlist(stats.Tmp.Pot)[1] == -1 & unlist(stats)[1] == -1 & unlist(background_Options)[1] == -1 &
	unlist(skills.Primary.DF)[1] == -1 & unlist(skills.Secondary.DF)[1] == -1 & unlist(skills.Increase.Record.DF)[1] == -1 & unlist(background_Special_Tables_List)[1] == -1 &
	unlist(flaws_Table)[1] == -1 & unlist(background.Skills.Increase.Record.DF)[1] == -1) return(-1)
#return:                                    LIST CONTAINING FIVE OBJECTS. 1) LIST OF ALL BENEFITS CHOSEN. 2) DATAFRAME OF ALL FLAWS TAKEN. 3) STAT.TMP. 4) STAT.POT. 5) background.Skills.Increase.Record.DF
    
   background.Option.Points = ifelse( unlist(background_Options.Benefits.ROLLING)[[1]] == -1, background.Option.Points, 
background.Option.Points - length(background_Options.Benefits.ROLLING[[1]]) )
    
    
    if( unlist(background_Options.Benefits.NO.ROLLING)[1] == -1 && background.Option.Points > 0){
        cat( paste0( "\nYou have ", background.Option.Points, " background option points remaining.\n" ) )
        if (interactive() ){ readline(prompt=paste0("Press enter to continue"))} else{ cat("Press enter to continue"); readLines("stdin",1)  }
        
        background_Options.Selected <- list()
        stat.Tmp <- stats.Tmp.Pot[,1]
        stat.Pot <- stats.Tmp.Pot[,2]
        flaws.Name <- NULL
        flaws.Description <- NULL
        
        for(i.bop in 1:background.Option.Points){
            
            flaw.Taken <- FALSE
            
            print( data.frame( Stats = stats, Tmp = stats.Tmp.Pot[,1], Pot = stats.Tmp.Pot[,2]) )
    
            result <- background_Options_Choice_NO_ROLLING(background_Options[1:11,],skills.Secondary.DF,skills.Primary.DF,
															background_Special_Tables_List,flaws_Table,
															stat.Tmp,stat.Pot,skills.Increase.Record.DF,
															background_Options.Selected)
            if( unlist(result[[1]])[1] == "FLAW" ){
                flaws.Name[(length(flaws.Name) + 1)] = result[[2]][2]
                flaws.Description[(length(flaws.Description) + 1)] = result[[2]][3]
                flaw.Taken = TRUE
                while( flaw.Taken == TRUE ){
                    result <- background_Options_Choice_NO_ROLLING(background_Options[1:11],skills.Secondary.DF,skills.Primary.DF,background_Special_Tables_List,flaws_Table,stat.Tmp,stat.Pot,skills.Increase.Record.DF,background_Options.Selected)
                    if( unlist(result[[1]])[1] == "FLAW" ){
                        flaws.Name[(length(flaws.Name) + 1)] = result[[2]][2]
                        flaws.Description[(length(flaws.Description) + 1)] = result[[2]][3]
                        flaw.Taken = TRUE
                    } else{
                        flaw.Taken = FALSE
                    }
                }
            }
              
            tmp <- stats.Tmp.Pot
            if( result[[2]] ==  6 |  result[[2]] ==  7 ){ #IF A STAT WAS INCREASED BY 2, OR THREE STATS WERE INCREASED BY 1, FIND OUT WHICH AND INCREASE THE TMP AND POT VALUES FOR IT.
                tmp <- tmp + result[[1]] - stats.Tmp.Pot
            }
            
            for(i in 1:nrow(tmp) ){
                if( tmp[i,1] > 101 ) tmp[i,1] = 101
                if( tmp[i,2] > 101 ) tmp[i,2] = 101
            }
            stat.Tmp <- tmp[,1]
            stat.Pot <- tmp[,2]
            stats.Tmp.Pot <- cbind(stat.Tmp,stat.Pot)
            
            if( result[[2]] == 1 |  result[[2]] == 2 ){ #IF ANY PRIMARY OR SECONDARY SKILLS WERE INCREASED, FIND OUT WHICH ONES AND RECORD IT IN background.Skills.Increase.Record.DF 
                background.Skills.Increase.Record.DF[,"NumTimesIncreased"] = background.Skills.Increase.Record.DF[,"NumTimesIncreased"] + result[[1]][,"NumTimesIncreased"] 
            }
            
            background_Options.Selected[[(length(background_Options.Selected)+1)]] = result
            
        }

        
        if( !(length( background_Options.Selected )) ) background_Options.Selected = -1

        return( list(background_Options.Selected, data.frame(Name = flaws.Name, Description = flaws.Description,stringsAsFactors = FALSE), stat.Tmp, stat.Pot, background.Skills.Increase.Record.DF ) )
    } else{
        return( background_Options.Benefits.NO.ROLLING )
    }
}

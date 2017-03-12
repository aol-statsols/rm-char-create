#' A BACKGROUND OPTIONS FUNCTION
#'
#' FUNCTION TO DEAL WITH BACKGROUND OPTION CHOICES
#' @param background_Options WHICH BACKGROUND OPTIONS ARE AVAILABLE
#' @param skills.Secondary.DF DATAFRAME OF THE SECONDARY SKILLS
#' @param skills.Primary.DF DATAFRAME OF THE PRIMARY SKILLS
#' @param background_Special_Tables_List TABLE CONTAINING ALL SPECIAL "ROLL ON" TABLE THE CHARACTER COULD USE
#' @param flaws_Table TABLE CONTAINING ALL FLAWS THE CHARACTER COULD TAKE
#' @param stat.Tmp CHARACTER'S TEMPORARY STATS
#' @param stat.Pot CHARACTER'S POTENTIAL STATS
#' @param skills.Increase.Record.DF DATAFRAME OF SKILLS YOU CAN INCREASE WHICH WILL GENERATE ONES USED TO RECORD ANY INCREASES
#' @param background_Options.Benefits LIST CONTAINING ALL BACKGROUND OPTION BENEFITS A PLAYER HAS TAKEN
#' @concept  skill increases
#' @export
#' @examples
#' background_Options_Choice_NO_ROLLING()

background_Options_Choice_NO_ROLLING <- function(background_Options = -1,skills.Secondary.DF = -1,skills.Primary.DF = -1,
                                        background_Special_Tables_List = -1,flaws_Table = -1,
										stat.Tmp = -1,stat.Pot = -1,skills.Increase.Record.DF = -1, 
										background_Options.Benefits = -1){ 
                                       
if(length(background_Options.Benefits) == 0 & unlist(skills.Secondary.DF)[1] == -1 & unlist(skills.Primary.DF)[1] == -1 &
	unlist(background_Special_Tables_List)[1] == -1 & unlist(flaws_Table)[1] == -1 &
    unlist(stat.Tmp)[1] == -1 & unlist(stat.Pot)[1] == -1 & unlist(skills.Increase.Record.DF)[1] == -1) return(-1)
	
if(length(background_Options.Benefits) > 0 ) if(unlist(background_Options)[1] == -1 & unlist(skills.Secondary.DF)[1] == -1 & unlist(skills.Primary.DF)[1] == -1 &
	unlist(background_Special_Tables_List)[1] == -1 & unlist(flaws_Table)[1] == -1 &
    unlist(stat.Tmp)[1] == -1 & unlist(stat.Pot)[1] == -1 & unlist(skills.Increase.Record.DF)[1] == -1 & unlist(background_Options.Benefits)[1] == -1) return(-1) 
#returns:   LIST CONTAINING 2 OBJECTS: 1) EITHER THE STRING "FLAW" IF USER CHOSE FLAW, OR THE RESULT OF THE BACKGROUND OPTION POINT. 2) THE FLAW CHOSEN OR THE NUMBER OF THE BACKGROUND OPTION CHOSEN
    cat("Choose one of the following background options:\n\n")
    print(background_Options)
    ans = choose_Generic("background option",background_Options[,2])
    ans = ifelse ( is.na( suppressWarnings( as.numeric( ans ) ) ), which(background_Options[,2] == ans) , as.numeric( ans ) )
    stats = stat_Name_DF[,1]
    
    
    if( ans == 1 ){
        skill = choose_Skill_From_DF(skills.Secondary.DF, background_Options.Benefits)
        skills.Increase.Record.DF[skills.Increase.Record.DF[,1] == skill,"NumTimesIncreased"] = 5
        return(list(skills.Increase.Record.DF,ans))
    } else if( ans == 2 ){
        skill = choose_Skill_From_DF(skills.Primary.DF, background_Options.Benefits)
        skills.Increase.Record.DF[skills.Increase.Record.DF[,1] == skill,"NumTimesIncreased"] = 2
        return(list(skills.Increase.Record.DF,ans))
    } else if( ans == 3 ){
        return(list("+10 Non-Magical Item",ans))
    } else if( ans == 4 ){
        return(list("+1 Spell Adder", ans))
    } else if( ans == 5 ){
        return(list("Language at 8 ranks written/spoken",ans))
    } else if( ans == 6 ){
        
        stat.Abb <- choose_Generic("stat",stats)
        
        if( stat.Tmp[stats %in% stat.Abb] + 2 > 101 |  stat.Pot[stats %in% stat.Abb] + 2 > 101 ){
            cat("You are trying to increase ", stat.Abb, ".\nThe temporary statistic would be: ", stat.Tmp[stats %in% stat.Abb] + 2, ".\nThe potential statistic would be: ", stat.Pot[stats %in% stat.Abb] + 2, ".\nThese can only be increased to a max of 101, are you sure you want to continue?\n\n", sep="")
            if( read_Yes_Or_No( "Continue with this selection?" ) == "No" ){
                return( background_Options_Choice_NO_ROLLING(background_Options,skills.Secondary.DF,skills.Primary.DF,background_Special_Tables_List,flaws_Table,stat.Tmp,stat.Pot,skills.Increase.Record.DF, background_Options.Benefits) )
            }
        }
        
        stat.Tmp[stats %in% stat.Abb] = stat.Tmp[stats %in% stat.Abb] + 2
        stat.Pot[stats %in% stat.Abb] = stat.Pot[stats %in% stat.Abb] + 2
        
        return(list( cbind(stat.Tmp,stat.Pot), ans))
    } else if( ans == 7 ){
    
        stat.Abb <- NULL
        stats.Tmp <- stats
        for(i in 1:3){
            stat.Abb[i] <- choose_Generic("stat", stats.Tmp)
            stats.Tmp <- stats.Tmp[stats.Tmp != stat.Abb[i]]
        }
        
        if( any(stat.Tmp[stats %in% stat.Abb] + 1 > 101) |  any(stat.Pot[stats %in% stat.Abb] + 1 > 101 )){
            for( i in 1:3 ){
                cat("You are trying to increase ", stat.Abb[i], ".\nThe temporary statistic would be: ", stat.Tmp[stats %in% stat.Abb[i]] + 1, ".\nThe potential statistic would be: ", stat.Pot[stats %in% stat.Abb[i]] + 1, ".\nThese can only be increased to a max of 101, are you sure you want to continue?\n\n", sep="")
            }
            if( read_Yes_Or_No( "Continue with this selection?" ) == "No" ){
                return( background_Options_Choice_NO_ROLLING(background_Options,skills.Secondary.DF,skills.Primary.DF,background_Special_Tables_List,flaws_Table,stat.Tmp,stat.Pot,skills.Increase.Record.DF, background_Options.Benefits) )
            }
        }
        
        stat.Tmp[stats %in% stat.Abb] = stat.Tmp[stats %in% stat.Abb] + 1
        stat.Pot[stats %in% stat.Abb] = stat.Pot[stats %in% stat.Abb] + 1
        
        return(list( cbind(stat.Tmp,stat.Pot), ans))
    } else if( ans == 8 ){
        return(list("Kung Fu",ans))
    } else if( ans == 9 ){
        return(list("Ambidextrous",ans))
    } else if( ans == 10 ){
        return(list("Choose Background",ans))
    } else if( ans == 11 ){
        return(list("Goal",ans))
    } else {
        
        cat("Please choose a number between 1 and 11 \n\n", sep="")
        if (interactive() ){ readline("Press enter to continue...")} else{ cat("Press enter to continue..."); readLines("stdin",1)  }
        return( background_Options_Choice_NO_ROLLING(background_Options,skills.Secondary.DF,skills.Primary.DF,background_Special_Tables_List,flaws_Table,stat.Tmp,stat.Pot,skills.Increase.Record.DF, background_Options.Benefits) )
               
    }
}

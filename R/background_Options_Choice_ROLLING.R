#' A BACKGROUND OPTIONS FUNCTION
#'
#' FUNCTION TO DEAL WITH INDIVIDUAL BACKGROUND OPTION CHOICES THAT REQUIRE ROLLING
#' @param background_Options WHICH BACKGROUND OPTIONS ARE AVAILABLE
#' @param background_Special_Tables_List TABLE CONTAINING ALL SPECIAL "ROLL ON" TABLE THE CHARACTER COULD USE
#' @param flaws_Table TABLE CONTAINING ALL FLAWS THE CHARACTER COULD TAKE
#' @param background_Options.Benefits LIST CONTAINING ALL BACKGROUND OPTION BENEFITS A PLAYER HAS TAKEN
#' @concept background options
#' @export
#' @examples
#' background_Options_Choice_NO_ROLLING()

background_Options_Choice_ROLLING <-
function(background_Options = -1,background_Special_Tables_List = -1,flaws_Table = -1,
                                                background_Options.Benefits = -1){ 
                                  
if(unlist(background_Options)[1] == -1 & unlist(background_Special_Tables_List)[1] == -1 & unlist(flaws_Table)[1] == -1 & length(background_Options.Benefits) == 0) return(-1)

if( length(background_Options.Benefits) > 0 ) if(unlist(background_Options)[1] == -1 & unlist(background_Special_Tables_List)[1] == -1 & unlist(flaws_Table)[1] == -1 & unlist(background_Options.Benefits)[1] == -1) return(-1)
#returns:   LIST CONTAINING 2 OBJECTS: 1) EITHER THE STRING "FLAW" IF USER CHOSE FLAW, OR THE RESULT OF THE BACKGROUND OPTION POINT. 2) THE FLAW CHOSEN OR THE NUMBER OF THE BACKGROUND OPTION CHOSEN
    cat("Choose if you want to use the background options which involve rolling.\nThese are chosen before stat assignment as they may affect it.\n\nChoose one of the following background options:\n\n")
#    print(background_Options)
    ans = choose_Generic("background option",background_Options[,2])
    ans = ifelse ( is.na( suppressWarnings( as.numeric( ans ) ) ), which(background_Options[,2] == ans) , as.numeric( ans ) )
    
    if( ans == 1 ){
        tab <- background_Special_Tables_List[[1]]
        roll <- sample(100,1,replace = TRUE)
        res <- tab[min(which(tab[,1] >= roll)),2]
           
        cat("Your special item is:\n\t",res,sep="")
        choose.Again.Answer = read_Yes_Or_No("\n\nDo you accept this? Enter Yes (Y) to continue, \nor\n enter No (N) to take a flaw to choose a different (or the same!) Background Option?")
        if( choose.Again.Answer == "No"){
            roll <- sample(100,1,replace=TRUE)
            flaw <- unlist(flaws_Table[min(which(flaws_Table[,1] >= roll)),])
            cat("Your character is flawed!\n\nSpecifically they suffer from:\n\t",flaw[2],"\na flaw which renders them:\n\t",flaw[3],"\n\nLet us hope you do not come to regret this choice...\n\n")
            if (interactive() ){ readline(prompt=paste0("Press Enter to continue"))} else{ cat("Press Enter to continue"); readLines("stdin",1)  }
            return( list("FLAW",flaw) )
        } else{
            return( list(res,ans) )
        }
    } else if( ans == 2 ){
        tab <- background_Special_Tables_List[[2]]
        roll <- sample(100,1,replace = TRUE)
        res <- tab[min(which(tab[,1] >= roll)),2]
        
        cat("Your special Wealth is:\n\t",res,sep="")
        choose.Again.Answer = read_Yes_Or_No("\n\nDo you accept this? Enter Yes (Y) to continue, \nor\n enter No (N) to take a flaw to choose a different (or the same!) Background Option?")
        if( choose.Again.Answer == "No"){
            roll <- sample(100,1,replace=TRUE)
            flaw <- unlist(flaws_Table[min(which(flaws_Table[,1] >= roll)),])
            cat("Your character is flawed!\n\nSpecifically they suffer from:\n\t",flaw[2],"\na flaw which renders them:\n\t",flaw[3],"\n\nLet us hope you do not come to regret this choice...\n\n")
            if (interactive() ){ readline(prompt=paste0("Press Enter to continue"))} else{ cat("Press Enter to continue"); readLines("stdin",1)  }
            return( list("FLAW",flaw) )
        } else{
            return( list(res,ans) )
        }
    } else if( ans == 3 ){
        tab <- background_Special_Tables_List[[3]]
        roll <- sample(100,1,replace = TRUE)
        res <- tab[min(which(tab[,1] >= roll)),2]
                
        cat("Your special Status is:\n\t",res,sep="")
        choose.Again.Answer = read_Yes_Or_No("\n\nDo you accept this? Enter Yes (Y) to continue, \nor\n enter No (N) to take a flaw to choose a different (or the same!) Background Option?")
        if( choose.Again.Answer == "No"){
            roll <- sample(100,1,replace=TRUE)
            flaw <- unlist(flaws_Table[min(which(flaws_Table[,1] >= roll)),])
            cat("Your character is flawed!\n\nSpecifically they suffer from:\n\t",flaw[2],"\na flaw which renders them:\n\t",flaw[3],"\n\nLet us hope you do not come to regret this choice...\n\n")
            if (interactive() ){ readline(prompt=paste0("Press Enter to continue"))} else{ cat("Press Enter to continue"); readLines("stdin",1)  }
            return( list("FLAW",flaw) )
        } else{
            return( list(res,ans) )
        }
    } else if( ans == 4 ){
        tab <- background_Special_Tables_List[[4]]
        roll <- sample(100,1,replace = TRUE)
        res <- tab[min(which(tab[,1] >= roll)),2]
                
        cat("Your special Family Tie is:\n\t",res,sep="")
        choose.Again.Answer = read_Yes_Or_No("\n\nDo you accept this? Enter Yes (Y) to continue, \nor\n enter No (N) to take a flaw to choose a different (or the same!) Background Option?")
        if( choose.Again.Answer == "No"){
            roll <- sample(100,1,replace=TRUE)
            flaw <- unlist(flaws_Table[min(which(flaws_Table[,1] >= roll)),])
            cat("Your character is flawed!\n\nSpecifically they suffer from:\n\t",flaw[2],"\na flaw which renders them:\n\t",flaw[3],"\n\nLet us hope you do not come to regret this choice...\n\n")
            if (interactive() ){ readline(prompt=paste0("Press Enter to continue"))} else{ cat("Press Enter to continue"); readLines("stdin",1)  }
            return( list("FLAW",flaw) )
        } else{
            return( list(res,ans) )
        }
    } else if( ans == 5 ){
        tab <- background_Special_Tables_List[[6]]
        roll <- sample(100,1,replace = TRUE)
        res <- unlist(tab[min(which(tab[,1] >= roll)),c(2,3)])
                
        cat("Your special Ability is:\n\t",res[1],"\nWhich gives you the following ability:\n\t",res[2],sep="")
        choose.Again.Answer = read_Yes_Or_No("\n\nDo you accept this? Enter Yes (Y) to continue, \nor\n enter No (N) to take a flaw to choose a different (or the same!) Background Option?")
        if( choose.Again.Answer == "No"){
            roll <- sample(100,1,replace=TRUE)
            flaw <- unlist(flaws_Table[min(which(flaws_Table[,1] >= roll)),])
            cat("Your character is flawed!\n\nSpecifically they suffer from:\n\t",flaw[2],"\na flaw which renders them:\n\t",flaw[3],"\n\nLet us hope you do not come to regret this choice...\n\n")
            if (interactive() ){ readline(prompt=paste0("Press Enter to continue"))} else{ cat("Press Enter to continue"); readLines("stdin",1)  }
            return( list("FLAW",flaw) )
        } else{
            return( list(res,ans) )
        }
    } else if( ans == nrow(background_Options) ){
        
        cat("You have chosen not to assign this background option point - it will be saved for later.\n\n", sep="")
        if (interactive() ){ readline(prompt=paste0("Press enter to continue"))} else{ cat("Press enter to continue"); readLines("stdin",1)  }
        return( "Unassigned" )
               
    } else {
        
        cat("Please choose a number between 1 and 5 \n\n", sep="")
        if (interactive() ){ readline("Press enter to continue...")} else{ cat("Press enter to continue..."); readLines("stdin",1)  }
        return( background_Options_Choice_ROLLING(background_Options,background_Special_Tables_List,flaws_Table,background_Options.Choices ) )
               
    }
}

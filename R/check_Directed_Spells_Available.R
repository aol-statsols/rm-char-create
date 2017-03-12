#' A TEST FUNCTION
#'
#' CHECKS TO SEE IF YOU'RE ABLE TO TAKE A PARTICULAR DIRECTED SPELL SKILL YET
#' @param skill.Name NAME OF THE SKILL THEY'RE TRYING TO INCREASE    
#' @param skills.Increase.Record.DF DATAFRAME OF SKILLS TO RECORD ANY INCREASES
#' @concept input generic 
#' @export
#' @examples
#' check_Directed_Spells_Available()

check_Directed_Spells_Available <-
function( skill.Name = -1, skills.Increase.Record.DF = -1 ){  

if(unlist(skill.Name)[1] == -1 & unlist(skills.Increase.Record.DF)[1] == -1) return(-1)
#returns TRUE/FALSE AS TO WHETHER OR NOT THEY CAN INCREASE IT    
    spellList.Names <- skills.Increase.Record.DF[,1][grepl( "Spells: .*: \\( .* \\)", skills.Increase.Record.DF[,1] )]
    spellList.Ranks <- skills.Increase.Record.DF[,"NumTimesIncreased"][grepl( "Spells: .*: \\( .* \\)", skills.Increase.Record.DF[,1] )] + skills.Increase.Record.DF[,"TotalRanks"][grepl( "Spells: .*: \\( .* \\)", skills.Increase.Record.DF[,1] )]
   
    spellList.DF <- data.frame( Lists = gsub( "Spells: .*: \\( (.*) \\)", "\\1", spellList.Names )[spellList.Ranks > 0], Ranks = spellList.Ranks[spellList.Ranks > 0])
    
    spell.Lists.Target_Bolt <- get( paste0( "spell.Lists.", gsub( ".*: (.*) Bolt", "\\1_Bolt", skill.Name ) ) )
    
    if( all( !( spellList.DF[,1] %in% spell.Lists.Target_Bolt[,1] ) ) ){
        cat( "You do have not have enough ranks to cast a bolt spell - so you can't learn to direct it.\n")
        if (interactive() ){ readline(prompt=paste0("Press enter to continue"))} else{ cat("Press enter to continue"); readLines("stdin",1)  }
        return( FALSE )
    }
    if ( any( spellList.DF[,2][spellList.DF[,1]  %in% spell.Lists.Target_Bolt[,1]] >= spell.Lists.Target_Bolt[,2][spell.Lists.Target_Bolt[,1]  %in% spellList.DF[,1]]  ) ){
        return( TRUE ) 
    } else {
        cat( "You do have not have enough ranks to cast a bolt spell - so you can't learn to direct it.\n")
        if (interactive() ){ readline(prompt=paste0("Press enter to continue"))} else{ cat("Press enter to continue"); readLines("stdin",1)  }
        return( FALSE )
    }
}

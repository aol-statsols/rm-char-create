#' A BACKGROUND OPTION FUNCTION
#'
#' LETS THE PLAYER CHOOSE THEIR BACKGROUND OPTIONS THAT REQUIRE ROLLING AND GENERATES THE BENEFITS
#' @param background_Options.Benefits.ROLLING LIST CONTAINING ALL BACKGROUND OPTION BENEFITS A PLAYER HAS TAKEN WHICH REQUIRED ROLLING
#' @param background.Option.Points NUMERIC VARIABLE CONTAINING THE NUMBER OF BACKGROUND OPTION POINTS A CHARACTER HAS
#' @param background_Options DF OF BACKGROUND OPTIONS AVAILABLE
#' @param background_Special_Tables_List TABLE CONTAINING ALL SPECIAL "ROLL ON" TABLE THE CHARACTER COULD USE
#' @param flaws_Table TABLE CONTAINING ALL FLAWS THE CHARACTER COULD TAKE
#' @concept  background options
#' @export
#' @examples
#' background_Options_Assignment_ROLLING()

background_Options_Assignment_ROLLING <-
function( background_Options.Benefits.ROLLING = -1, background.Option.Points = -1, background_Options = -1, background_Special_Tables_List = -1,  
                                                    flaws_Table = -1){ 
                                                    
if(unlist(background_Options.Benefits.ROLLING)[1] == -1 & unlist(background.Option.Points)[1] == -1 & unlist(background_Options)[1] == -1 &
	unlist(background_Special_Tables_List)[1] == -1 & unlist(flaws_Table)[1] == -1) return(-1)
#return:    LIST CONTAINING TWO OBJECTS. 1) LIST OF ALL BENEFITS CHOSEN. 2) DATAFRAME OF ALL FLAWS TAKEN.
    if( unlist(background_Options.Benefits.ROLLING)[1] == -1 ){
    
        background_Options.Selected <- list()
        flaws.Name <- NULL
        flaws.Description <- NULL
        background_Options.Rolling <- rbind( background_Options[12:16,],c(nrow(background_Options)+1,"None of these.") ) 
        rownames( background_Options.Rolling ) = NULL
        background_Options.Rolling[,1] = 1:nrow( background_Options.Rolling ) 
        
        for(i.bop in 1:background.Option.Points){
            
            flaw.Taken <- FALSE
            
            result <- background_Options_Choice_ROLLING(background_Options.Rolling,background_Special_Tables_List,flaws_Table,background_Options.Selected)
            if( unlist(result[[1]])[1] == "FLAW" ){
                flaws.Name[(length(flaws.Name) + 1)] = result[[2]][2]
                flaws.Description[(length(flaws.Description) + 1)] = result[[2]][3]
                flaw.Taken = TRUE
                while( flaw.Taken == TRUE ){
                    result <- background_Options_Choice_ROLLING(background_Options.Rolling,background_Special_Tables_List,flaws_Table,background_Options.Selected)
                    if( unlist(result[[1]])[1] == "FLAW" ){
                        flaws.Name[(length(flaws.Name) + 1)] = result[[2]][2]
                        flaws.Description[(length(flaws.Description) + 1)] = result[[2]][3]
                        flaw.Taken = TRUE
                    } else{
                        flaw.Taken = FALSE
                    }
                }
            }
            
            if( unlist(result)[1] != "Unassigned" ) background_Options.Selected[[(length(background_Options.Selected)+1)]] = result
            
        }
        
        
        if( length( background_Options.Selected ) ){
for( i in 1:length(background_Options.Selected) ) background_Options.Selected[[i]][[2]] = background_Options.Selected[[i]][[2]] + 11
} else{
background_Options.Selected = -1
}
        return ( list(background_Options.Selected, data.frame(Name = flaws.Name, Description = flaws.Description,stringsAsFactors = FALSE) ) )
    } else{
        return( background_Options.Benefits.ROLLING )
    }
    
}

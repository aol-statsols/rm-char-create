#' A BACKGROUND OPTIONS FUNCTION FUNCTION
#'
#' FUNCTION TO COMBINE BACKGROUND BENEFITS FROM ROLLING AND CHOOSING
#' @param background_Options.Benefits.ROLLING OPTIONS WHICH REQUIRED ROLLING CHOSEN WITH BACKGROUND OPTION POINTS
#' @param background_Options.Benefits.NO.ROLLING OPTIONS WHICH DID NOT REQUIR ROLLING CHOSEN WITH BACKGROUND OPTION POINTS
#' @concept background options
#' @export
#' @examples
#' background_Options_Choices_Combine()

background_Options_Choices_Combine <-
function( background_Options.Benefits.ROLLING = -1, background_Options.Benefits.NO.ROLLING = -1){ #FUNCTION TO COMBINE BACKGROUND BENEFITS FROM ROLLING AND CHOOSING
    
if(unlist(background_Options.Benefits.ROLLING)[1] == -1 & unlist(background_Options.Benefits.NO.ROLLING)[1] == -1) return(-1)
#returns:                                   LIST CONTAINING ALL BACKGROUND BENEFITS CHOSEN OR ROLLED
    background_Options.Benefits = list() 
    if( unlist(background_Options.Benefits.NO.ROLLING)[1] == -1 ){
        background_Options.Benefits[[1]] <- background_Options.Benefits.ROLLING[[1]]
        background_Options.Benefits[[2]] <- background_Options.Benefits.ROLLING[[2]]
    } else if( unlist(background_Options.Benefits.ROLLING)[1] == -1 ){
        background_Options.Benefits[[1]] <- background_Options.Benefits.NO.ROLLING[[1]]
        background_Options.Benefits[[2]] <- background_Options.Benefits.NO.ROLLING[[2]]
    } else{
        background_Options.Benefits[[1]] <- c(background_Options.Benefits.ROLLING[[1]],background_Options.Benefits.NO.ROLLING[[1]])
        background_Options.Benefits[[2]] <- c(background_Options.Benefits.ROLLING[[2]],background_Options.Benefits.NO.ROLLING[[2]])
    }
    
    return( background_Options.Benefits )
}

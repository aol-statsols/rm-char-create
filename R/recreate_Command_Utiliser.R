#' A RECREATE FUNCTION
#'
#' COMMAND TO USE A JSON CONTAINING ALL THE CHARACTER'S DETAILS TO DO SOMETHING WITH IT (LEVEL UP OR ALTER)
#' @param char.Name CHARACTER'S NAME. DEFAULTS TO EMPTY STRING.
#' @concept recreate
#' @export
#' @examples
#' recreate_Command_Utiliser()

recreate_Command_Utiliser <-
function( char.Name = "" ){ #

if(unlist(char.Name)[1] == "") return(-1)    
#returns:   NULL
      
    recreate.From.JSON <- fromJSON( paste0( char.Name, ".JSON" ) )  
    
}

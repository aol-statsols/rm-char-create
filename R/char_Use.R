#' A CHARACTER SHEET GENERATOR FUNCTION
#'
#' FUNCTION WHICH CREATES, ALTERS OR LEVELS A CHARACTER AND THEN GENERATES A CHARACTER SHEET FOR THEM
#' @param EXAMPLE BOOLEAN VARIABLE. IF IT'S AN EXAMPLE, IT RETURNS -1 SO THAT THE PACKAGE CAN BE CHECKED.
#' @concept character level alter create
#' @export
#' @examples
#' char_Use( EXAMPLE = TRUE )

char_Use <-
function(  EXAMPLE = FALSE ){ 
    if( EXAMPLE ) return(-1)
#returns NULL

    ans <- choose_Generic( "Character Option you want to use", c("Create Character", "Alter Character", "Level Up Character") )
    char.Name <- enter_Name( "char" )
    
    if( ans == "Create Character"){
        character_CREATE(char.Name)
    } else if( ans == "Alter Character"){
        character_ALTER(char.Name)
    } else if( ans == "Level Up Character"){
        character_LEVEL(char.Name)
    }
    
}

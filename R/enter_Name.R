#' AN INPUT READ AND CHECKING FUNCTION
#'
#' FUNCTION WHICH GETS THE PLAYER TO ENTER A NON-NUMERIC NAME
#' @param character.Or.Player STRING INDICATING WHETHER WE'RE ASKING FOR THE CHARACTER'S NAME OR THE PLAYER'S NAME
#' @concept input str string
#' @export
#' @examples
#' enter_Name()

enter_Name <-
function(character.Or.Player = -1){ 

if(unlist(character.Or.Player)[1] == -1) return(-1) 
#returns: A STRING "name"

target <- ifelse( character.Or.Player == "char", "character", "player")
cat("\nPlease enter a name for your ", target, ":\n", sep="" )
if (interactive() ){ name <- readline(prompt="Choose: ")} else{ cat("Choose: "); name <- readLines("stdin",1)  }
if( !suppressWarnings( is.na(as.numeric(name) ) ) ){
cat("\n\nA number is not a name...although Player 1 is acceptable.\n\n")
return(enter_Name(character.Or.Player))
}
if( nchar(name) > 20 ){
cat("\n\nThat name is longer than currently allowed. Enter a shorter one, please.\n\n")
return(enter_Name(character.Or.Player))
}
if( character.Or.Player == "char" && paste0(name,".pdf") %in% list.files() ){
y.n = read_Yes_Or_No("\n\nThat name already being used.\n\nDo you want to override the current sheet?")
if( y.n == "No" ) return(enter_Name(character.Or.Player))
}
return( name )
}

#' AN INPUT READ AND CHECKING FUNCTION
#'
#' FUNCTION TO ACCEPT SOME INDICATOR FOR "YES" OR "NO" FROM A USER AND RETURN "Yes" OR "No"
#' @param prom STRING WITH THE PROMPT TO OUTPUT TO THE SCREEN
#' @concept input string
#' @export
#' @examples
#' read_Yes_Or_No()

read_Yes_Or_No <-
function(prom = -1){ 

if(unlist(prom)[1] == -1) return(-1) 
#returns: "Yes" OR "No"

cat(prom,"\n\tYes (Y)\n\tNo (N)\n\n",sep="")
if (interactive() ){ ans <- readline(prompt="Choose: ")} else{ cat("Choose: "); ans <- readLines("stdin",1)  }
    if( ans == "Yes" || ans == "Y" || ans == "Yes (Y)" || ans == "(Y)" || ans == "y" ){
        return("Yes") 
    } else if( ans == "No" || ans == "N" || ans == "No (N)" || ans == "(N)" || ans == "n" ){
        return("No") 
    } else{
        prom <- ("\n\nPlease choose one of the options:")
        return( read_Yes_Or_No(prom) )
    }
    
}

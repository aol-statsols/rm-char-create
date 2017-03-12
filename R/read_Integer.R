#' AN INPUT READ AND CHECKING FUNCTION
#'
#' FUNCTION TO READ AN INTEGER FROM THE USER, BETWEEN TWO VALUES.
#' @param val.Min MINIMUM VALUE THE USER CAN ENTER
#' @param val.Max MAXIMUM VALUE THE USER CAN ENTER
#' @concept input int integer
#' @export
#' @examples
#' read_Integer()

read_Integer <-
function(val.Min = -1,val.Max = -1){  

if(unlist(val.Min)[1] == -1 & unlist(val.Max)[1] == -1) return(-1) 
#returns: AN INTEGER "ans"

cat("\n\nPlease enter an integer between ",val.Min," and ", val.Max, ":\n\n",sep="")
if (interactive() ){ ans <- suppressWarnings( as.numeric( readline(prompt="Choose: ") ) )} else{ cat("Choose: "); ans <- suppressWarnings( as.numeric( readLines("stdin",1) ) )  }
    
    if( is.na(ans) || ans < val.Min || ans > val.Max){
        return( read_Integer(val.Min,val.Max) )
    } else{
        if( all.equal( ans, as.integer(ans) ) != TRUE && nchar(ans) < 18  ){
cat("\nThe number must be an integer.\n")
return(  read_Integer(val.Min,val.Max) )
        }
        return(ans)
    }
    
}

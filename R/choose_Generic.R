#' A CHOICE FUNCTION
#'
#' GENERIC FUNCTION WHICH ASKS USER TO CHOOSE FROM A SPECIFIED LIST. 
#' @param choice.Name THE NAME OF THE CHARACTER SHEET ASPECT THE USER IS CHOOSING.
#' @param choices A VECTOR OF THE CHOICES AVAILABLE TO THEM
#' @param additional.Info ANY OTHER INFO FOR THE PLAYER TO SEE
#' @concept input generic 
#' @export
#' @examples
#' choose_Generic()

choose_Generic <-
function(choice.Name = -1,choices = -1,additional.Info = ""){ 

if(unlist(choice.Name)[1] == -1 & unlist(choices)[1] == -1 & unlist(additional.Info)[1] == "") return(-1)
#returns STRING CONTAINING THE NAME OF THE CHOICE MADE
numberOfChoices = length(choices)
    choice.DF <- data.frame( Number = 1:numberOfChoices, choices )
    
    if( additional.Info != "" ) cat(additional.Info)
    
    colnames(choice.DF)[2] = paste(choice.Name)
cat( "\nEnter the ", choice.Name,"\n\tOr\nChoose between 1 and ", numberOfChoices, " to choose a ",choice.Name,":\n",sep = "" ) 
print( choice.DF )
if (interactive() ){ strTmp <- readline(prompt="Choose: ")} else{ cat("Choose: "); strTmp <- readLines("stdin",1)  }

if( is.na( suppressWarnings( as.numeric(strTmp) ) ) ){
if( strTmp %in% choices){
retVal = strTmp
return(retVal)
} else{
cat(paste0( "\nThe name must be from\n", paste0(choices,collapse=", "), "\n" ))
return(  choose_Generic(choice.Name,choices) )
}
} else{
numTmp <- as.numeric(strTmp)
        if( all.equal( numTmp, as.integer(numTmp) ) != TRUE && nchar(strTmp) < 18 ){
cat("\nThe number must be an integer.\n")
return(  choose_Generic(choice.Name,choices) )
        }
if( 1 <= numTmp  &&  numTmp <= numberOfChoices ){
retVal <- choices[numTmp]
return(retVal)
} else{
if (interactive() ){ readline(prompt=paste0("The number must be between 1 and ", numberOfChoices))} else{ cat("The number must be between 1 and ", numberOfChoices); readLines("stdin",1)  }
return( choose_Generic(choice.Name,choices) )
}
}
    
}

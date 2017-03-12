#' A RECREATE FUNCTION
#'
#' FUNCTION TO GENERATE A JSON CONTAINING ALL THE CHARACTER'S DETAILS
#' @param recreate.Command LIST CONTAINING ALL THE CHARACTER'S DETAILS.
#' @param char.Name STRING CONTAINING THE CHARACTER'S NAME
#' @concept recreate
#' @export
#' @examples
#' recreate_Command_Generator()

recreate_Command_Generator <-
function(recreate.Command = -1, char.Name = ""){ 

if(unlist(recreate.Command)[1] == -1 & unlist(char.Name)[1] == "") return(-1) 
#returns:           NULL
    options(encoding = "utf8")
    recreate.JSON <- toJSON(recreate.Command, pretty=TRUE)  
    fileConn<-file(paste0(char.Name,".JSON"))
    writeLines( recreate.JSON, fileConn )
    close(fileConn)
    
}

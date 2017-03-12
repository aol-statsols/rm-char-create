#' A PDF GENERATOR FUNCTION
#'
#' TAKES IN THE CREATED PAGES AND OUTPUTS THEM AS A PDF
#' @param char.Name STRING CONTAINING THE CHARACTER'S NAME
#' @param deleteImages TRUE/FALSE SHOULD WE DELETE THE IMAGES ONCE WE'RE DONE WITH THEM.
#' @concept output pdf
#' @export
#' @examples
#' character_Sheet_Generator()

character_Sheet_Generator <-
function( char.Name = -1, deleteImages = FALSE ){ 

if(unlist(char.Name)[1] == -1 & unlist(deleteImages)[1] == FALSE) return(-1)
#returns NULL
    
    
    thePlots <- list()
    usr<-par("usr")    
    
    outfiles <- list.files()[grepl("outP[0-9].png",list.files())]
    for(i in 1:length(outfiles)) thePlots[[i]] <- grid::rasterGrob(readPNG(outfiles[i], native = FALSE), interpolate = FALSE)
    
output.Name = ifelse( ( char.Name == "" | char.Name == -1 ), "output.pdf", paste0( char.Name, ".pdf" ) )
        
    ml <- marrangeGrob(thePlots, nrow=1, ncol=1, top = NULL,padding = grid::unit(0.0, "line"))    
    ggsave(output.Name, ml, width = 210, height = 297, units = "mm")
    
    thePlots <- NULL
    gc()
    
    if( deleteImages ) file.remove(outfiles)
}

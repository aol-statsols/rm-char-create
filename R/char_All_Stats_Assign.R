#' A STAT FUNCTION
#'
#' FUNCTION TO ASSIGN THE STAT ROLLS TO SPECIFIC STATS
#' @param stats.Tmp CHARACTER'S TEMPORARY STATS
#' @param stats.Mat CHARACTER'S CURRENTLY ASSIGNED TEMPORARY STATS
#' @param stats VECTOR OF THE 10 STATS
#' @param resist.Realm SPELLCASTING REALM CHARACTER IS RESISTANT TO
#' @param adept.Realm SPELLCASTING REALM CHARACTER IS ADEPT WITH
#' @param char.Class.FullName CHARACTER'S PROFESSION'S FULL NAME
#' @concept skill increases
#' @export
#' @examples
#' char_All_Stats_Assign()

char_All_Stats_Assign <-
function(stats.Tmp = -1,stats.Mat = -1,stats = -1,resist.Realm ="",adept.Realm="",char.Class.FullName=""){ 

if(unlist(stats.Tmp)[1] == -1 & unlist(stats.Mat)[1] == -1 & unlist(stats)[1] == -1 & 
	unlist(resist.Realm)[1] == "" & unlist(adept.Realm)[1] == "" & unlist(char.Class.FullName)[1] == "") return(-1)
#returns:   MATRIX WITH ALL STATS ASSIGNED VALUES
for( i.Row in 1:10 ){
retList <- char_Single_Stat_Assign(stats.Tmp,stats.Mat,stats,i.Row,resist.Realm,adept.Realm,char.Class.FullName)
stats.Mat <- retList[[1]]
stats.Tmp <- retList[[2]]
}

    print( stats.Mat )
ans <- read_Yes_Or_No("Are you happy with your stat assignment?")

if( ans == "Yes" ){
return(stats.Mat)
} else{
        stats.Tmp <- sort(stats.Mat[,2])
        stats.Mat[,2] <- 0 
cat("\nThen assign again, my good chum.")
return( char_All_Stats_Assign(stats.Tmp,stats.Mat,stats,resist.Realm,adept.Realm,char.Class.FullName) )
}
    
}

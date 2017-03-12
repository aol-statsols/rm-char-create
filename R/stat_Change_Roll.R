#' A STAT CHANGE FUNCTION
#'
#' FUNCTION TO HANDLE THE STAT CHANGE ROLLS
#' @param stat.Tmp  CHARACTER'S TEMPORARY STATS
#' @param stat.Pot  CHARACTER'S POTENTIAL STATS
#' @concept stat stats
#' @export
#' @examples
#' stat_Change_Roll()

stat_Change_Roll <-
function( stat.Tmp = -1, stat.Pot = -1 ){ 

if(unlist(stat.Tmp)[1] == -1 & unlist(stat.Pot)[1] == -1) return(-1)
#returns:   CHARACTER'S NEW TEMPORARY STATS
        stat.Roll <- sample(10,2,replace=TRUE)
        
        if(stat.Roll[1] == stat.Roll[2] ){
            if( stat.Roll[1] > 5 ) {
                stat.Tmp <- min(stat.Pot,stat.Tmp + sum(stat.Roll))
            } else {
                stat.Tmp <- max(11,stat.Tmp - sum(stat.Roll))
            }
        } else if( stat.Pot - stat.Tmp <= 10 ){
            stat.Tmp = min( stat.Pot, stat.Tmp + min(stat.Roll) )
        } else if( stat.Pot - stat.Tmp > 10 && stat.Pot - stat.Tmp <= 20 ){
            stat.Tmp = min( stat.Pot, stat.Tmp + max(stat.Roll) )
        } else{
            stat.Tmp = min( stat.Pot, stat.Tmp  + sum(stat.Roll) )
        }

stat.Tmp
}

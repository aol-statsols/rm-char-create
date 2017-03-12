#' A FORMAT FUNCTION
#'
#' FUNCTION WHICH TRUNCATES VALUES TO WHERE I WANT THEM TO BE
#' @param x VALUE TO TRUNCATE
#' @param ... OTHER PARAMETERS TO PASS TO base::trunc
#' @param prec WHAT PRECISION TO TRUNCTATE x TO
#' @concept FORMATTING
#' @export
#' @examples
#' character_creator_trunc()

character_creator_trunc <-
function(x = -1, ..., prec = 0){
if(unlist(x)[1] == -1 & unlist(prec)[1] == 0) return(-1)
    base::trunc(x * 10^prec, ...) / 10^prec
}

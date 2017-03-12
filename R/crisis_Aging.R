#' AN AGING CRISIS FUNCTION
#'
#' FUNCTION TO ADJUST STATS DURING AN AGING CRISIS.
#' @param tmp CHARACTER'S TEMPORARY STATS
#' @param pot CHARACTER'S POTENTIAL STATS
#' @param num.Dice NUMBER OF DICE TO BE USED DURING THE AGING CRISIS
#' @param stat.Bonus THE CURRENT BONUS FROM EACH STAT
#' @param stat.Dev THE CURRENT DEV POINTS FROM EACH STAT
#' @concept stat stats aging crisi
#' @export
#' @examples
#' crisis_Aging()

crisis_Aging <-
function( tmp = -1, pot = -1, num.Dice = -1, stat.Bonus = -1, stat.Dev = -1 ){ 

if(unlist(tmp)[1] == -1 & unlist(pot)[1] == -1 & unlist(num.Dice)[1] == -1 & unlist(stat.Bonus)[1] == -1 & unlist(stat.Dev)[1] == -1) return(-1)     
#returns A MATRIX CONTAINING THE NEW TEMPORARY AND POTENTIAL STATISTICS

    stat = statVal_And_Bonus_And_DP[,1]
    bonus <- statVal_And_Bonus_And_DP[,2]
    dev_Points <- statVal_And_Bonus_And_DP[,3]
    
    print("What up with dem aging crises?")
    print(pot)
pot[1] = pot[1] - sum( sample( 1:10, num.Dice, replace = TRUE ) )
pot[2] = pot[2] - sum( sample( 1:10, num.Dice, replace = TRUE ) )
pot[6] = pot[6] - sum( sample( 1:10, num.Dice, replace = TRUE ) )
pot[7] = pot[7] - sum( sample( 1:10, num.Dice, replace = TRUE ) )

    print(pot)
    print("What up with dem temp stats for aging crises?")
    print(tmp)
for( i.Test in c(1,2,6,7) ) if( pot[i.Test] > tmp[i.Test] ) tmp[i.Test] = pot[i.Test]

    print(tmp)
for(i.Stat in (1:10)[-c(1,2,6,7)]){
stat.Roll <- sample(10,2,replace=TRUE)

if(stat.Roll[1] == stat.Roll[2] ){
if( stat.Roll[1] > 5 ) {
tmp[i.Stat] <- min(pot[i.Stat],tmp[i.Stat] + sum(stat.Roll))
} else {
tmp[i.Stat] <- max(11,tmp[i.Stat] - sum(stat.Roll))
}
} else if( pot[i.Stat] - tmp[i.Stat] <= 10 ){
tmp[i.Stat] = min( pot[i.Stat], tmp[i.Stat] + min(stat.Roll) )
} else if( pot[i.Stat] - tmp[i.Stat] > 10 && pot[i.Stat] - tmp[i.Stat] <= 20 ){
tmp[i.Stat] = min( pot[i.Stat], tmp[i.Stat] + max(stat.Roll) )
} else{
tmp[i.Stat] = min( pot[i.Stat], tmp[i.Stat] + sum(stat.Roll) )
}

stat.Bonus[i.Stat] = min(bonus[tmp[i.Stat] <= stat])
stat.Dev[i.Stat] = ifelse( i.Stat < 6, min(dev_Points[tmp[i.Stat] <= stat]), 0 )
}
cbind(tmp,pot,stat.Bonus,stat.Dev)
}

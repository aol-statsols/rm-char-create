#' A STAT FUNCTION
#'
#' CHANGES STATS AT EACH LEVEL, INCLUDING CHECKING FOR AGING CRISES
#' @param stat.Tmp CHARACTER'S CURRENT TEMPORARY STATS   
#' @param stat.Pot CHARACTER'S CURRENT POTENTIAL STATS
#' @param char.Age CHARACTER'S CURRENT AGE
#' @param char.Age_Die CHARACTER'S AGE DIE
#' @param species.Middle_Aged WHEN MIDDLE AGE HAPPENS FOR CHARACTER'S SPECIES
#' @param species.Old WHEN OLD AGE HAPPENS FOR CHARACTER'S SPECIES
#' @param species.Venerable WHEN VENERABLE AGE HAPPENS FOR CHARACTER'S SPECIES
#' @param age_Crisis.Middle_Aged HAS MIDDLE AGE CRISIS HAPPENED FOR CHARACTER'S SPECIES
#' @param age_Crisis.Old HAS OLD AGE CRISIS HAPPENED FOR CHARACTER'S SPECIES
#' @param age_Crisis.Venerable HAS VENERABLE AGE CRISIS HAPPENED FOR CHARACTER'S SPECIES
#' @concept stat stats
#' @export
#' @examples
#' level_Up_Stat_Changes()

level_Up_Stat_Changes <-
function(stat.Tmp = -1,stat.Pot = -1,char.Age = -1,char.Age_Die = -1,species.Middle_Aged = -1,species.Old = -1,species.Venerable = -1,
            age_Crisis.Middle_Aged = -1,age_Crisis.Old = -1,age_Crisis.Venerable = -1){ 

if(unlist(stat.Tmp)[1] == -1 & unlist(stat.Pot)[1] == -1 & unlist(char.Age)[1] == -1 & unlist(char.Age_Die)[1] == -1 &
	unlist(species.Middle_Aged)[1] == -1 & unlist(species.Old)[1] == -1 & unlist(species.Venerable)[1] == -1 &
	unlist(age_Crisis.Middle_Aged)[1] == -1 & unlist(age_Crisis.Old)[1] == -1 & unlist(age_Crisis.Venerable)[1] == -1) return(-1)           
#returns: LIST WITH 8 ITEMS. 1) NEW TMP STATS. 2) NEW POT STATS. 3) NEW STAT BONUS. 4) NEW DEV POINTS. 5) NEW CHARACTER AGE. 6) MIDDLE AGE CRISIS HAPPENED. 7) OLD AGE CRISIS HAPPENED. 8) VENERABLE AGE CRISIS HAPPENED. 
    stat.Bonus <- NULL
    stat.Dev <- NULL
    stat = statVal_And_Bonus_And_DP[,1]
    bonus <- statVal_And_Bonus_And_DP[,2]
    dev_Points <- statVal_And_Bonus_And_DP[,3]
    
    for(i.Stat in 1:10){
stat.Tmp[i.Stat] <- stat_Change_Roll ( stat.Tmp[i.Stat], stat.Pot[i.Stat] )
        stat.Bonus[i.Stat] = min(bonus[stat.Tmp[i.Stat] <= stat])
        stat.Dev[i.Stat] = ifelse( i.Stat < 6, min(dev_Points[stat.Tmp[i.Stat] <= stat]), 0 )
    }

    char.Age <- char.Age + sample(1:char.Age_Die, 1, replace=TRUE)
    
    if( char.Age > species.Middle_Aged && age_Crisis.Middle_Aged == FALSE  ){
        print("Middle Aged")
        stats.Mat.Tmp <- crisis_Aging(stat.Tmp,stat.Pot,1,stat.Bonus,stat.Dev)
        stat.Tmp = stats.Mat.Tmp[,1]
        stat.Pot = stats.Mat.Tmp[,2]
        stat.Bonus = stats.Mat.Tmp[,3]
        stat.Dev = stats.Mat.Tmp[,4]
        age_Crisis.Middle_Aged = TRUE
    } else if( char.Age > species.Old && age_Crisis.Old == FALSE  ){
        print("Old")
        stats.Mat.Tmp <- crisis_Aging(stat.Tmp,stat.Pot,2, stat.Bonus, stat.Dev)
        stat.Tmp = stats.Mat.Tmp[,1]
        stat.Pot = stats.Mat.Tmp[,2]
        stat.Bonus = stats.Mat.Tmp[,3]
        stat.Dev = stats.Mat.Tmp[,4]
        age_Crisis.Old = TRUE
    } else if( char.Age > species.Venerable && age_Crisis.Venerable == FALSE  ){
        print("Venerable")
        stats.Mat.Tmp <- crisis_Aging(stat.Tmp,stat.Pot,3,stat.Bonus,stat.Dev)
        stat.Tmp = stats.Mat.Tmp[,1]
        stat.Pot = stats.Mat.Tmp[,2]
        stat.Bonus = stats.Mat.Tmp[,3]
        stat.Dev = stats.Mat.Tmp[,4]
        age_Crisis.Venerable = TRUE
    }
    
    return( list( stat.Tmp, stat.Pot, stat.Bonus, stat.Dev, char.Age, age_Crisis.Middle_Aged, age_Crisis.Old, age_Crisis.Venerable ) )
    
}

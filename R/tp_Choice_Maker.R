#' A TRAINING PACKAGE FUNCTION
#'
#' FUNCTION WHICH ASSIGNS SKILLS THAT COME FROM "ANY" OR "UP TO"
#' @param tp.Choice EITHER A LIST CONTAINING ALL TP RESULTS OR -1
#' @param training_Packages_Cost_DF DATAFRAME CONTAINING THE COSTS OF ALL TRAINING PACKAGES
#' @param training_Packages_Gains DATAFRAME CONTAINING THE GAINS FROM ALL TRAINING PACKAGES
#' @param char.Class STRING WITH CHARACTER'S PROFESSION
#' @param stat.Tmp VECTOR OF CHARACTER'S TEMPORARY STATS
#' @param stat.Pot VECTOR OF CHARACTER'S POTENTIAL STATS
#' @param char.Realm STRING WITH CHARACTER'S REALM
#' @concept  training package
#' @export
#' @examples
#' tp_Choice_Maker()

tp_Choice_Maker <-
function(tp.Choice = -1,  training_Packages_Cost_DF = -1,  training_Packages_Gains = -1,  char.Class = -1,  stat.Tmp = -1,  stat.Pot = -1, char.Realm = -1){
          
if(unlist(tp.Choice)[1] == -1 & unlist(training_Packages_Cost_DF)[1] == -1 & unlist(training_Packages_Gains)[1] == -1 &
	unlist(char.Class)[1] == -1 & unlist(stat.Tmp)[1] == -1 & unlist(stat.Pot)[1] == -1) return(-1)     
#returns: LIST WITH 6 ITEMS. 1) LIST OF ALL TRAINING PACKAGE RESULTS. 2) DATAFRAME OF NEW STATS. 3) NEW STAT BONUSES. 4) NEW STAT DEV POINTS. 5) NEW DEV POINT TOTAL. 6) ORIGINAL DEV POINT TOTAL.
    stat.Bonus <- NULL
    stat.Dev <- NULL
    
    stats = stat_Name_DF[,1]
    stat = statVal_And_Bonus_And_DP[,1]
    bonus <- statVal_And_Bonus_And_DP[,2]
    dev_Points <- statVal_And_Bonus_And_DP[,3]
    
    for(i.Stat in 1:10){
        stat.Dev[i.Stat] = ifelse( i.Stat < 6, min(dev_Points[stat.Tmp[i.Stat] <= stat]), 0 )
        stat.Bonus[i.Stat] = min(bonus[stat.Tmp[i.Stat] <= stat])
    } 
    dev_Points.Total = floor(sum(stat.Dev))
    
    dev_Points.Level_1 = dev_Points.Total
    
    #SEE IF THEY WANT TO USE A TRAINING PACKAGE
if( unlist(tp.Choice)[1] == -1 ){
        cat(paste0("You have ", dev_Points.Level_1, " Development Points at level 1.\n") )
tp.Want <- read_Yes_Or_No( "Do you want to choose a training package?")
if( tp.Want == "Yes" ){
tp.Choice <- choose_Training_Package(training_Packages_Cost_DF, training_Packages_Gains, char.Class, dev_Points.Total)
tp.Skills.ANY <- tp.Choice[[1]]
tp.Skills.Normal <- tp.Choice[[2]]
tp.Skills.Other_Gains <- tp.Choice[[3]]
tp.Skills.Stat_Increases  <- tp.Choice[[4]]
dev_Points.Total = dev_Points.Total - as.numeric( tp.Choice[[5]] )
            tp.Name <- tp.Choice[[6]]
print(tp.Skills.Stat_Increases)
if( unlist(tp.Skills.Stat_Increases)[1] != "NONE" ){
stats.Chosen = NULL
for( i.tp.Stat in 1:length( tp.Skills.Stat_Increases ) ){
if( tp.Skills.Stat_Increases[i.tp.Stat] == "ANY" ){
if( i.tp.Stat == 1 ){
stats.Available = stats
} else{
stats.Available = stats[-(which(stats == stat.Chosen))]
}
stat.Chosen <- choose_Generic( "Stat to increase", stats.Available )
stat.Tmp[which(stats == stat.Chosen)] = stat_Change_Roll( stat.Tmp[which(stats == stat.Chosen)], stat.Pot[which(stats == stat.Chosen)] )
} else if( tp.Skills.Stat_Increases[i.tp.Stat] == "REALM" ){
stat.Chosen <- ifelse( length(char.Realm) == 1, char.Realm, choose_Generic( "a realm stat to increase", char.Realm ) )
stat.Tmp[which(stats == stat.Chosen)] = stat_Change_Roll( stat.Tmp[which(stats == stat.Chosen)], stat.Pot[which(stats == stat.Chosen)] )
                    } else{
stat.Chosen <- tp.Skills.Stat_Increases[i.tp.Stat]
stat.Tmp[which(stats == stat.Chosen)] = stat_Change_Roll( stat.Tmp[which(stats == stat.Chosen)], stat.Pot[which(stats == stat.Chosen)] )
}
}
}
} else{
tp.Skills.ANY <- data.frame()
tp.Skills.Normal <- data.frame()
tp.Skills.Other_Gains <- ""
tp.Skills.Stat_Increases  <- "NONE"
dev_Points.Total = dev_Points.Total 
            tp.Name <- ""
}
} else{
tp.Skills.ANY <- tp.Choice[[1]]
tp.Skills.Normal <- tp.Choice[[2]]
tp.Skills.Other_Gains <- tp.Choice[[3]]
tp.Skills.Stat_Increases  <- tp.Choice[[4]]
dev_Points.Total = dev_Points.Total - as.numeric( tp.Choice[[5]] )
        tp.Name <- tp.Choice[[6]]

if( tp.Skills.Stat_Increases[1] != "NONE" ){
for( i.tp.Stat in 1:length( tp.Stats.Increased ) ){
stat.Tmp[which(stats == tp.Stats.Increased[i.tp.Stat])] = stat.Tmp[which(stats == tp.Stats.Increased[i.tp.Stat])] + tp.Stat.Increases
}
}
}
    
    for(i.Stat in 1:10){
        if( stat.Tmp[i.Stat] > 101 ) stat.Tmp[i.Stat] = 101
        stat.Bonus[i.Stat] = min(bonus[stat.Tmp[i.Stat] <= stat])
    }
    
    return( list( tp.Choice, data.frame(Tmp = stat.Tmp, Pot = stat.Pot), stat.Bonus, stat.Dev, dev_Points.Total, dev_Points.Level_1 ) )
}

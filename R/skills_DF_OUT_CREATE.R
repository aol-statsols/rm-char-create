#' A SKILL DATAFRAME OUTPUT FUNCTION
#'
#' FUNCTION TO CREATE THE SKILL DATAFRAME THAT GETS OUTPUT TO THE SCREEN
#' @param skills.Increase.Record.DF DATA FRAME OF THE SKILLS, SKILL COSTS, NUMBER OF TIMES YOU CAN INCREASE, NUMBER OF TIMES YOU HAVE INCREASED
#' @param increase BOOLEAN VALUE FOR WHETHER WE ARE INCREASING THE NUMBER OF SKILLS TAKEN THAT LEVEL OR NOT. DEFAULTS TO INCREASING
#' @concept skill skills output
#' @export
#' @examples
#' skills_DF_OUT_CREATE()

skills_DF_OUT_CREATE <-
function(skills.Increase.Record.DF = -1, increase=TRUE){
 
if(unlist(skills.Increase.Record.DF)[1] == -1 & unlist(increase)[1] == TRUE) return(-1) 
#returns:                   DATAFRAME CONTAINING THE SKILLNAMES, THE CURRENT COST FOR EACH ONE, THE NUMBER OF TIMES IT CAN BE INCREASED AND THE TOTAL RANKS PUT IN IN PREVIOUS LEVELS
        skills.OUT.Cost <- 1:nrow(skills.Increase.Record.DF)
        if( increase ){
            for(i in 1:nrow(skills.Increase.Record.DF)){
                cost <- ifelse( skills.Increase.Record.DF[i,"NumTimesCanIncrease"] == 55, skills.Increase.Record.DF[i,2],
                                ifelse( skills.Increase.Record.DF[i,"NumTimesIncreased"] < skills.Increase.Record.DF[i,"NumTimesCanIncrease"],
                                    skills.Increase.Record.DF[i,2+skills.Increase.Record.DF[i,"NumTimesIncreased"]], 
                                    56 ) 
                            )
                skills.OUT.Cost[i] = cost
            }
        } else{
            for(i in 1:nrow(skills.Increase.Record.DF)){
                cost <- ifelse( skills.Increase.Record.DF[i,"NumTimesIncreased"] == 0, 0,
                                ifelse( skills.Increase.Record.DF[i,"NumTimesCanIncrease"] == 55, skills.Increase.Record.DF[i,2],
                                    skills.Increase.Record.DF[i,2+(skills.Increase.Record.DF[i,"NumTimesIncreased"]-1)] ) 
                            )
                skills.OUT.Cost[i] = cost
            }
        }
        return( data.frame(Skill = skills.Increase.Record.DF[,1], Cost = skills.OUT.Cost, Increased = skills.Increase.Record.DF[,"NumTimesIncreased"],
                                                        Limit = skills.Increase.Record.DF[,"NumTimesCanIncrease"], Total = skills.Increase.Record.DF[,"TotalRanks"]) )
                                                        
}

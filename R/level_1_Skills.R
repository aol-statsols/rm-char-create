#' A LEVEL FUNCTION
#'
#' PLAYER ASSIGNS SKILLS FROM HOBBY POINTS AND DEVELOPMENT POINTS FOR LEVEL 1
#' @param skills.Increase.Record.DF A DATAFRAME CONTAINING WHICH SKILLS HAVE BEEN INCREASED AND HOW OFTEN.
#' @param dev_Points.Start AN INTEGER WHICH IS HOW MANY DEV POINTS THEY HAVE THAT LEVEL
#' @param char.Class A STRING WHICH IS WHAT CLASS THEY ARE (DETERMINES SKILL FACTS)
#' @param background_Options.Benefits LIST CONTAINING ALL BACKGROUND OPTION BENEFITS A PLAYER HAS TAKEN
#' @param tp.Skills.Normal DATAFRAME OF THE SKILLS ASSIGNED AUTOMATICALLY FROM TRAINING PACKAGE
#' @param tp.Skills.ANY DATAFRAME OF THE SKILLS ASSIGNED BY THE PLAYER FROM TRAINING PACKAGE
#' @param tp.Skills.Assign TRUE/FALSE - HAVE THE TRAINING PACKAGE SKILLS ALREADY BEEN ASSIGNED
#' @param level_1_Skills.Increase.Record.List LIST WHICH MAY CONTAIN THE VARIABLES TO RETURNED IF THEY HAVE BEEN PREGENERATED
#' @param skills_Groups GROUPS OF SKILLS THAT SHARE HALF RANKS
#' @concept skills level training package
#' @export
#' @examples
#' level_1_Skills()

level_1_Skills <-
function(skills.Increase.Record.DF = -1, dev_Points.Start = -1, char.Class = -1, background_Options.Benefits = -1, tp.Skills.Normal = -1, tp.Skills.ANY = -1, tp.Skills.Assign = TRUE,
                                level_1_Skills.Increase.Record.List = -1, skills_Groups = -1){ 

if(unlist(skills.Increase.Record.DF)[1] == -1 & unlist(dev_Points.Start)[1] == -1 & unlist(char.Class)[1] == -1 & unlist(background_Options.Benefits)[1] == -1 & unlist(tp.Skills.Normal)[1] == -1 &
    unlist(tp.Skills.ANY)[1] == -1 & unlist(tp.Skills.Assign) == TRUE & unlist(level_1_Skills.Increase.Record.List)[1] == -1 & unlist(skills_Groups)[1] == -1) return(-1)   
#returns:   A LIST OF TWO OBJECTS: 1) THE COMPLETED DATAFRAME OF SKILL INCREASES. 2) THE TOTAL DEVELOPMENT POINTS REMAINING.
    if( unlist(level_1_Skills.Increase.Record.List)[1] == -1 ){
        skill.Num <- nrow(skills.Increase.Record.DF)
        skill.Names <- skills.Increase.Record.DF[,1]
        skill.Chosen.Name <- NULL
        continue = TRUE
        dev_Points.Total <- dev_Points.Start
        
        if( tp.Skills.Assign == TRUE ){
    #        print(tp.Skills.ANY)
            #ADDS ON SKILLS FROM TRAINING PACKAGE
            if( nrow( tp.Skills.Normal ) > 0 ){
                cat("\nThese skills have been assigned from your training package:\n")
                print(tp.Skills.Normal)
                if (interactive() ){ readline(prompt=paste0("Press enter to continue"))} else{ cat("Press enter to continue"); readLines("stdin",1)  }
                for(i in 1:nrow( tp.Skills.Normal )){
                    tp.N <- tp.Skills.Normal[i,]
                    skills.Increase.Record.DF[ skills.Increase.Record.DF[,"Skills.Name"] == tp.N[,1],"NumTimesIncreased" ] = 
                        skills.Increase.Record.DF[ skills.Increase.Record.DF[,"Skills.Name"] == tp.N[,1],"NumTimesIncreased" ] + tp.N[,2]
                }
            }
            
            #ADDS ON SKILLS FROM "ANY" SKILLS
            #print( skills.Increase.Record.DF[,c("Skills.Name","NumTimesIncreased")] )
            if( nrow( tp.Skills.ANY ) > 0 ){
                cat( "\nAssign your ANY skills from the Training Package as you desire.")
                if (interactive() ){ readline()} else{ readLines("stdin",1)  }
                
                ANY.skills <- assign_ANY_Skills( data.frame(), data.frame(), tp.Skills.ANY, skills.Increase.Record.DF)
                skills.Increase.Record.DF[,"NumTimesIncreased"] <- ANY.skills[,"NumTimesIncreased"]
            }
            
            tp.Skills.Assign = FALSE
        }
        
        while( dev_Points.Total > 0 && continue == TRUE ){
        
            skills.Increase.Record.DF.OUT <- skills_DF_OUT_CREATE(skills.Increase.Record.DF)
            print(skills.Increase.Record.DF.OUT)
            cat("\n\nYou have ", dev_Points.Total, " Development Points left to spend.\n\nEnter the name of the skill you wish to increase:\n\tOR\nChoose a number between 1 and ", skill.Num,":\n\n",sep="")
            if (interactive() ){ skill <- readline()} else{ skill <- readLines("stdin",1)  }
            
            if( is.na( suppressWarnings( as.numeric( skill ) ) ) ) {
                
                if( test_Skill_From_DF_Character(skill, skill.Names, skills.Increase.Record.DF, background_Options.Benefits, dev_Points.Total) ){
                    skillRow = skills.Increase.Record.DF[skills.Increase.Record.DF[,1] == skill,]
                    skill.Cost = unlist( ifelse( skillRow["NumTimesCanIncrease"] == 55, skillRow["Skills.Cost.1"],
                        ifelse( skillRow["NumTimesIncreased"] == 0, skillRow["Skills.Cost.1"], 
                        ifelse( skillRow["NumTimesIncreased"] == 1, skillRow["Skills.Cost.2"], 
                        ifelse( skillRow["NumTimesIncreased"] == 2, skillRow["Skills.Cost.3"]  ) ) ) ) )
                    dev_Points.Total = unlist( dev_Points.Total - skill.Cost ) 
                    skills.Increase.Record.DF[skills.Increase.Record.DF[,1] == skill,"NumTimesIncreased"] = skillRow["NumTimesIncreased"] + 1
                } else{
                    return( level_1_Skills(skills.Increase.Record.DF, dev_Points.Total, char.Class, background_Options.Benefits, tp.Skills.Normal, tp.Skills.ANY, tp.Skills.Assign,
                                            level_1_Skills.Increase.Record.List) )
                }
            
            } else{
                
                if( test_Skill_From_DF_Numeric(skill, skill.Names, skills.Increase.Record.DF, background_Options.Benefits, dev_Points.Total) ){
                    skillRow = skills.Increase.Record.DF[ as.numeric(skill),]
                    skill.Cost = unlist( ifelse( skillRow["NumTimesCanIncrease"] == 55, skillRow["Skills.Cost.1"],
                        ifelse( skillRow["NumTimesIncreased"] == 0, skillRow["Skills.Cost.1"], 
                        ifelse( skillRow["NumTimesIncreased"] == 1, skillRow["Skills.Cost.2"], 
                        ifelse( skillRow["NumTimesIncreased"] == 2, skillRow["Skills.Cost.3"]  ) ) ) ) )
                    dev_Points.Total = unlist( dev_Points.Total - skill.Cost ) 
                    skills.Increase.Record.DF[as.numeric(skill),"NumTimesIncreased"] = skillRow["NumTimesIncreased"] + 1
                } else{
                    return( level_1_Skills(skills.Increase.Record.DF, dev_Points.Total, char.Class, background_Options.Benefits, tp.Skills.Normal, tp.Skills.ANY, tp.Skills.Assign,
                                            level_1_Skills.Increase.Record.List) )
                }
            
            }
            
        }
        
        
        skills.Increase.Record.DF[,"TotalRanks"] = skills.Increase.Record.DF[,"TotalRanks"] + skills.Increase.Record.DF[,"NumTimesIncreased"] 
        
        if (interactive() ){ readline("Press enter to continue.")} else{ cat("Press enter to continue."); readLines("stdin",1)  }
        return( list( skills.Increase.Record.DF, dev_Points.Total ) )
    } else{
        return( level_1_Skills.Increase.Record.List )
    }
    print("Eh?")
}

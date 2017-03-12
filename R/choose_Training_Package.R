#' A TRAINING PACKAGE FUNCTION
#'
#' FUNCTION TO LET THE USER CHOOSE A SPECIFIC TRAINING PACKAGE.
#' @param training_Packages_Cost_DF DATAFRAME CONTAINING INFO ON THE COST OF TRAINING PACKAGES BASED ON WHAT PROFESSION THE USER CHOSE
#' @param training_Packages_Gains DATAFRAME CONTAINING INFO ON THE GAINS FROM TRAINING PACKAGES
#' @param char.Class STRING CONTAINING THE USERS PROFESSION
#' @param dev_Points.Total NUMERIC VARIABLE CONTAINING THE DEVELOPMENT POINTS A CHARACTER HAS TO SPEND
#' @concept skills 
#' @export
#' @examples
#' choose_Training_Package()

choose_Training_Package <-
function(training_Packages_Cost_DF = -1, training_Packages_Gains = -1, char.Class = -1, dev_Points.Total = -1){ 

if(unlist(training_Packages_Cost_DF)[1] == -1 & unlist(training_Packages_Gains)[1] == -1 & unlist(char.Class)[1] == -1 & unlist(dev_Points.Total)[1] == -1) return(-1)
#returns LIST CONTAINING 6 ITEMS. 1) Skill increases containing the word ANY. 2) Other skills increased. 3) The rewards from rolling. 4) Any stats to be increased. 5) The cost of the training package. 6) The name of the training package.
trainingPackages.Num <- nrow(training_Packages_Cost_DF)
trainingPackages.Names <- training_Packages_Cost_DF[,1]
print(training_Packages_Cost_DF[c("Training_Package",char.Class)])
cat("\nEnter the Training Package name to choose a Package\n\tOR\nEnter the number, between 1 and ", trainingPackages.Num, " to choose the Training Package\n", sep="" ) 

    
    
   
    if (interactive() ){ strTmp <- readline(prompt="Choose: ")} else{ cat("Choose: "); strTmp <- readLines("stdin",1)  }

if( is.na( suppressWarnings( as.numeric(strTmp) ) ) ){
if( strTmp %in% trainingPackages.Names){
            if( dev_Points.Total >= as.numeric( training_Packages_Cost_DF[training_Packages_Cost_DF[,1] == strTmp,char.Class ] ) ){
                strTmp = strTmp
            } else{
                cat( "\nYou don't have enough Developoment Points to choose that. Choose from one you can afford:\n" )
                if (interactive() ){ readline("Press Enter to continue.")} else{ cat("Press Enter to continue."); readLines("stdin",1)  }
                return( choose_Training_Package( training_Packages_Cost_DF, training_Packages_Gains, char.Class, dev_Points.Total ) )
            }
} else{
cat( "\nThe name must be from one of the following options:\n" )
return( choose_Training_Package( training_Packages_Cost_DF, training_Packages_Gains, char.Class, dev_Points.Total ) )
}
} else{
numTmp <- as.numeric(strTmp)
if( 1 <= numTmp  &&  numTmp <= trainingPackages.Num ){
            if( dev_Points.Total >= as.numeric( training_Packages_Cost_DF[numTmp,char.Class ] ) ){
                strTmp <- strTmp
            } else{
                cat( "\nYou don't have enough Developoment Points to choose that. Choose from one you can afford:\n\n" )
                if (interactive() ){ readline("Press Enter to continue.")} else{ cat("Press Enter to continue."); readLines("stdin",1)  }
                return( choose_Training_Package( training_Packages_Cost_DF, training_Packages_Gains, char.Class, dev_Points.Total ) )
                print("Should never reach")
            }
} else{
if (interactive() ){ readline(prompt=paste0("The number must be between 1 and ", trainingPackages.Num))} else{ cat("The number must be between 1 and ", trainingPackages.Num); readLines("stdin",1)  }
return( choose_Training_Package( training_Packages_Cost_DF, training_Packages_Gains, char.Class, dev_Points.Total ) )
}
}
    
    
if( is.na( suppressWarnings( as.numeric(strTmp) ) ) ){
if( strTmp %in% trainingPackages.Names ){
training.Package = strTmp
training.Package.Num = which(training_Packages_Cost_DF[,1] == strTmp)
            tp.Tmp <- training_Packages_Gains[training.Package.Num,]
            tp.Skills <- data.frame( Skills.Name = unlist( strsplit(tp.Tmp[,"Skills"],";") ), Skills.Ranks = as.numeric(unlist( strsplit(tp.Tmp[,"Ranks"],";") )) )
            tp.Skills.ANY <- tp.Skills[( grepl( "ANY", tp.Skills[,1] ) | grepl( "Up to", tp.Skills[,1] ) | grepl( "OWN REALM", tp.Skills[,1] ) ),]
            tp.Skills.NORMAL <- tp.Skills[!( grepl( "ANY", tp.Skills[,1] ) | grepl( "Up to", tp.Skills[,1] ) | grepl( "OWN REALM", tp.Skills[,1] )  ),]
            tp.Rewards.Achieved <- NULL
            tp.Rewards.Possible <- unlist( strsplit(tp.Tmp[,"Rewards"],";") )
            tp.Rewards.Target <- as.numeric( unlist( strsplit(tp.Tmp[,"Targets"],";") ) )
            for(i in 1:length(tp.Rewards.Target) ){
                if( sample(100,1) <= tp.Rewards.Target[i] ){
                    tp.Rewards.Achieved <- c(tp.Rewards.Achieved,tp.Rewards.Possible[i])
                    tp.Rewards.Target <- tp.Rewards.Target/2
                }
            }
            tp.Stats <- unlist( strsplit( tp.Tmp[,"Stats"], ";" ) )
return( list( tp.Skills.ANY, tp.Skills.NORMAL, tp.Rewards.Achieved, tp.Stats, training_Packages_Cost_DF[ training_Packages_Cost_DF[,1] == strTmp, char.Class ], strTmp ) )
} else{
cat(paste0( "\nThe name must be from\n", paste0(trainingPackages.Names,collapse=", "), "\n" ))
return( choose_Training_Package(training_Packages_Cost_DF, training_Packages_Gains, char.Class, as.numeric( training_Packages_Cost_DF[ training_Packages_Cost_DF[,1] == strTmp, char.Class ] ) ) )
}
} else{
numTmp <- as.numeric(strTmp)
if( 1 <= numTmp  &&  numTmp <= trainingPackages.Num ){
            tp.Tmp <- training_Packages_Gains[numTmp,]
            tp.Skills <- data.frame( Skills.Name = unlist( strsplit(tp.Tmp[,"Skills"],";") ), Skills.Ranks = as.numeric(unlist( strsplit(tp.Tmp[,"Ranks"],";") )) )
            tp.Skills.ANY <- tp.Skills[( grepl( "ANY", tp.Skills[,1] ) | grepl( "Up to", tp.Skills[,1] ) | grepl( "OWN REALM", tp.Skills[,1] )  ),]
            tp.Skills.NORMAL <- tp.Skills[!( grepl( "ANY", tp.Skills[,1] ) | grepl( "Up to", tp.Skills[,1] ) | grepl( "OWN REALM", tp.Skills[,1] )  ),]
            tp.Rewards.Achieved <- NULL
            tp.Rewards.Possible <- unlist( strsplit(tp.Tmp[,"Rewards"],";") )
            tp.Rewards.Target <- as.numeric( unlist( strsplit(tp.Tmp[,"Targets"],";") ) )
            for(i in 1:length(tp.Rewards.Target) ){
                if( sample(100,1) <= tp.Rewards.Target[i] ){
                    tp.Rewards.Achieved <- c(tp.Rewards.Achieved,tp.Rewards.Possible[i])
                    tp.Rewards.Target <- tp.Rewards.Target/2
                }
            }
            tp.Stats <- unlist( strsplit( tp.Tmp[,"Stats"], ";" ) )
return( list( tp.Skills.ANY, tp.Skills.NORMAL, tp.Rewards.Achieved, tp.Stats, training_Packages_Cost_DF[ numTmp, char.Class ], training_Packages_Cost_DF[numTmp,1] ) )
} else{
if (interactive() ){ readline(prompt=paste0("The number must be between 1 and ", trainingPackages.Num))} else{ cat("The number must be between 1 and ", trainingPackages.Num); readLines("stdin",1)  }
return( choose_Training_Package(training_Packages_Cost_DF, training_Packages_Gains, char.Class) )
}
}
}

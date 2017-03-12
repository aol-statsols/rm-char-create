#' A STAT FUNCTION
#'
#' FUNCTION TO ASSIGN ONE OF THE STAT VALUES ROLLED TO ONE OF THE "Co, Ag, SD, etc..."
#' @param stat.Tmp REMAINING STAT ROLLS TO ASSIGN
#' @param stat.Mat STATS ASSIGNED, 0s ELSEWHERE
#' @param stats NAME OF ALL STATS
#' @param i WHICH STAT IS BEING SET
#' @param resist.Realm SPELLCASTING REALM CHARACTER IS RESISTANT TO
#' @param adept.Realm SPELLCASTING REALM CHARACTER IS ADEPT WITH
#' @param char.Class.FullName CHARACTER'S CLASS'S FULL NAME
#' @concept level character 
#' @export
#' @examples
#' char_Single_Stat_Assign()

char_Single_Stat_Assign <-
function(stat.Tmp = -1,stat.Mat = -1,stats = -1,i = -1,resist.Realm = "",adept.Realm = "",char.Class.FullName = ""){
#################################################################################################################################################################################################################
#                                                                                                                                                                                                               #
#                                                                                                                                                                                                               #
#                                                                                    TO CONSIDER                                                                                                                #
#                                                                                                                                                                                                               #
#                                            1 TAKE THE NEXT VALUE GENERATED AND GIVE THE STAT LIST AND ASK THEM TO ASSIGN THEM TO THAT?                                                                        #        
#                                                                                                                                                                                                               #
#                                                                                                                                                                                                               #
#################################################################################################################################################################################################################

if(unlist(stat.Tmp)[1] == -1 & unlist(stat.Mat)[1] == -1 & unlist(stats)[1] == -1 & unlist(i)[1] == -1 & 
	unlist(resist.Realm)[1]=="" & unlist(adept.Realm)[1]=="" & unlist(char.Class.FullName)[1]=="") return(-1)         
#returns LIST CONTAINING A MATRIX WITH THE FIRST COLUMN BEING THE STAT NAMES AND THE SECOND ONE BEING THE STAT VALUES ASSIGNED SO FAR; STATS YET TO BE ASSIGNED.
writeLines("Current Stats:\n")
print(stat.Mat)
cat( paste0("\nAssign a value to ", stats[i], ".\nChoose between 1 and ", length(stat.Tmp), " to assign a value from:\n\n") )
    
    classes <- classesDF[,2]
    
    realms = c("Essence","Channeling","Mentalism")
    realms.Stats = c("Empathy","Intuition","Presence")
    
    if( resist.Realm != "" ) cat( paste0("Note, you are highly resistant to ", resist.Realm, ".\nYou will suffer pentalties if casting from that realm, using ", realms.Stats[realms==resist.Realm] ,".\n\n") )
    if( adept.Realm != "" ) cat( paste0("Note, you are exceptionally enchanted with respect to ", adept.Realm, ".\nYou have bonuses when casting from that realm, using ", realms.Stats[realms==adept.Realm], ".\n\n") )
    
    stats.Prime = stats_Primary[,which(classes == char.Class.FullName)]
    if( !all(is.na(stats.Prime)) ){
        if( substr( char.Class.FullName, 1, 1) %in% c("a","e","i","o","u","A","E","I","O","U") ){
            cat( paste0("Your character is an ",char.Class.FullName, ".\nYour prime stats are ", stats.Prime[1]," and ", stats.Prime[2], ".\n\n") )
        } else{
            cat( paste0("Your character is a ",char.Class.FullName, ".\nYour prime stats are ", stats.Prime[1]," and ", stats.Prime[2], ".\n\n") )
        }
    }
    
print(data.frame(Order = 1:length(stat.Tmp), Value = stat.Tmp))
if (interactive() ){ n <- readline(prompt=paste0("\nChoose between 1 and ", length(stat.Tmp), ":\n\n") )
    } else{ cat(paste0("\nChoose between 1 and ", length(stat.Tmp), ":\n\n")); n <- readLines("stdin",1)  }

if( is.na( suppressWarnings( as.numeric(n) ) ) || suppressWarnings( as.numeric(n) ) < 1 || suppressWarnings( as.numeric(n) ) > length(stat.Tmp) ){
cat("\nPlease enter a number between 1 and ", length(stat.Tmp),".\n", sep = "")
if (interactive() ){ readline("Press Enter to try again")} else{ cat("Press Enter to try again"); readLines("stdin",1)  }
return( char_Single_Stat_Assign(stat.Tmp,stat.Mat,stats,i) )
} else {
n <- as.integer(n)
stat.Mat[i,2] = stat.Tmp[n]
stat.Tmp <- stat.Tmp[-n]
  
return(list(stat.Mat,stat.Tmp))
}

}

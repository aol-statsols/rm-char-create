#' A CLASS AND STAT FUNCTION
#'
#' FUNCTION TO CHOOSE CLASS, REALM AND ASSIGN STATS
#' @param char.Class CHARACTER'S CLASS
#' @param resist.Realm SPELLCASTING REALM CHARACTER IS RESISTANT TO
#' @param adept.Realm SPELLCASTING REALM CHARACTER IS ADEPT WITH
#' @param realms STRING CONTAINING THE THREE SPELLCASTING REALMS
#' @param realms.Stats STRING CONTAINING THE STATS ASSOCIATED THREE SPELLCASTING REALMS
#' @param realms.Stats.Short STRING CONTAINING THE SHORTENED STATS ASSOCIATED THREE SPELLCASTING REALMS
#' @param classes STRING CONTAINING ALL PROFESSIONS
#' @param stat.Mat MATRIX OF CHARACTER'S STATS
#' @param char.Realm CHARACTER'S SPELLCASTING REALM
#' @concept level character 
#' @export
#' @examples
#' char_Class_Decisions_Stat_Assignment()

char_Class_Decisions_Stat_Assignment <-
function( char.Class = -1, resist.Realm = -1, adept.Realm = -1, realms = -1, realms.Stats = -1, realms.Stats.Short = -1, classes = -1, stat.Mat = -1, char.Realm = -1 ){
 
if(unlist(char.Class)[1] == -1 & unlist(resist.Realm)[1] == -1 & unlist(adept.Realm)[1] == -1 & unlist(realms)[1] == -1 & unlist(realms.Stats)[1] == -1 & 
	unlist(realms.Stats.Short)[1] == -1 & unlist(classes)[1] == -1 & unlist(stat.Mat)[1] == -1 & unlist(char.Realm)[1] == -1) return(-1) 
#returns A LIST WITH THE CLASS = -1, STATS AND SPELLCASTING REALM OF THE CHARACTER = -1, char.Class = -1, char.Class.FullName = -1, stats.Tmp.Pot = -1, char.Realm AND char.Realm.Name    
  
    #CHOOSE THE PLAYER'S PROFESSION AND ASSOCIATED VARIABLES
    if( unlist(char.Class)[1] == -1){
    
        if( resist.Realm != "" ){
            cat( paste0("\n\nNB: you are highly resistant to ", resist.Realm, ".\nYou will suffer pentalties if casting from that realm, using ", realms.Stats[realms==resist.Realm] ,".\n\n") )
            if (interactive() ){ readline("Press enter to continue.")} else{ cat("Press enter to continue."); readLines("stdin",1)  }
        }
        if( adept.Realm != "" ){
            cat( paste0("\n\nNB: you are exceptionally enchanted with respect to ", adept.Realm, ".\nYou have bonuses when casting from that realm, using ", realms.Stats[realms==adept.Realm] ,".\n\n") )
            if (interactive() ){ readline("Press enter to continue.")} else{ cat("Press enter to continue."); readLines("stdin",1)  }
        }
char.Class <- choose_Generic("Profession",classes) 
        
}
     
    if( gsub("(.*)_.*","\\1",char.Class) %in% c( "Alchemist", "Monk", "Bard" ) ){
        char.Class.FullName <- char.Class
        char.Class <- gsub("(.*)_.*","\\1",char.Class)
    } else{
        char.Class.FullName <- char.Class
    }

    #GENERATES STATS FOR PLAYER
    if( unlist(stat.Mat)[1] == -1 ){
        stats.Tmp.Pot <- char_Stat_Generate(resist.Realm, adept.Realm, char.Class.FullName)
    } else{
        stats.Tmp.Pot = stat.Mat
    }

    cat("\n\nYour character's starting stats are:\n")
    print( data.frame( Stats = stat_Name_DF[,1], Tmp = stats.Tmp.Pot[,1], Pot = stats.Tmp.Pot[,2]) )
    #ASSIGN REALM STAT
    if( unlist(char.Realm)[1] == -1 ){
        char.Realm <- char_Realm_Stat_Assignment( char.Class.FullName, classes )
    }
char.Realm.Name <- realms[ realms.Stats.Short %in% char.Realm ]
    
    return( list( c( char.Class, char.Class.FullName), stats.Tmp.Pot, char.Realm, char.Realm.Name ) )
}

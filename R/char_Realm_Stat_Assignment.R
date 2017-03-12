#' A REALM STAT FUNCTION
#'
#' FUNCTION TO GET THE PLAYER'S REALM STAT
#' @param char.Class STRING CONTAINING THE CHARACTER'S PROFESSION
#' @param classes VECTOR CONTAINING THE POSSIBLE CLASSES
#' @concept level character 
#' @export
#' @examples
#' char_Realm_Stat_Assignment()

char_Realm_Stat_Assignment <-
function( char.Class = -1, classes = -1 ){  

if(unlist(char.Class)[1] == -1 & unlist(classes)[1] == -1) return(-1)      
#returns A STRING WITH THE CHARACTER'S REALM STAT IN IT

    stats_Realm <- stat_Realm[,2]
    
    if( char.Class %in% classes[1:5] ){
        char.Realm = choose_Generic("Realm Stat",stats_Realm)
    } else if( char.Class %in% c("Magician","Illusionist","Alchemist","Monk") ){
        char.Realm = stats_Realm[1]
    } else if( char.Class %in% c("Cleric","Paladin","Animist","Healer","Alchemist_Channelling","Ranger")){
        char.Realm = stats_Realm[2]
    } else if( char.Class %in% c("Mentalist","Seer","Lay_Healer","Monk_Mentalism","Bard") ){
        char.Realm = stats_Realm[3]
    } else if( char.Class %in% c("Sorcerer") ){
        char.Realm = stats_Realm[c(1,2)]
    } else if( char.Class %in% c("Mystic") ){
        char.Realm = stats_Realm[c(1,3)]
    } else if( char.Class %in% c("Astrologer") ){
        char.Realm = stats_Realm[c(2,3)]
    }
    
}

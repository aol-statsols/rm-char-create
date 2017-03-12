#' A REALM FUNCTION
#'
#' CHECKS TO SEE IF CHARACTERS ARE PARTICULARLY RESISTANT TO OR ADEPT WITH ANY REALM
#' @param background_Options.Benefits.ROLLING A LIST CONTAINING THE RESULTS OF ALL THE BENEFITS WHICH HAVE TO BE ROLLED FOR, AND ANY FLAWS TAKEN
#' @param realms A STRING CONTAINING THE THREE SPELLCASTING REALMS  
#' @concept realms
#' @export
#' @examples
#' character_LEVEL()

char_Realm_Relations <-
function( background_Options.Benefits.ROLLING = -1, realms = -1 ){ 

if(unlist(background_Options.Benefits.ROLLING)[1] == -1 & unlist(realms)[1] == -1) return(-1)
#returns A STRING WITH resist.Realm AND adept.Realm   
    if( "Highly Resistant" %in% unlist(background_Options.Benefits.ROLLING) ){
        resist.Realm.Roll = sample(100,1)
        resist.Realm = realms[ifelse( resist.Realm.Roll <= 40, 1, ifelse( resist.Realm.Roll <= 70, 2, 3 ) )]
    } else{
        resist.Realm = ""
    }
    
    if( "Exceptionally Enchanted" %in% unlist(background_Options.Benefits.ROLLING) ){
        adept.Realm.Roll = sample(100,1)
        adept.Realm = realms[ifelse( adept.Realm.Roll <= 40, 1, ifelse( adept.Realm.Roll <= 70, 2, 3 ) )]
    } else{
        adept.Realm = ""
    }
   
    return( c( resist.Realm, adept.Realm ) )
}

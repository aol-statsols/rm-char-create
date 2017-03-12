#' A SKILL BONUS FUNCTION
#'
#' FUNCTION TO CALCULATE THE BONUSES TO A SKILL FOR A RANK.
#' @param x THE ROW CONTAINING THE RANKS INCREASED AND THE SKILL NAME AND OTHER DETAILS.     
#' @param bd.Progress THE RATE AT WHICH BODY DEVELOPMENT INCREASES FOR THE CHARACTER'S SPECIES.
#' @param char.Species STRING CONTAINING CHARACTER'S SPECIES
#' @param char.Level NUMERIC CONTAINING CHARACTER'S LEVEL
#' @concept bonus skill
#' @export
#' @examples
#' rank_Bonus_Calculator()

rank_Bonus_Calculator <-
function(x = -1,bd.Progress = -1,char.Species = -1,char.Level = -1){ 

if(unlist(x)[1] == -1 & unlist(bd.Progress)[1] == -1 & unlist(char.Species)[1] == -1 & unlist(char.Level)[1] == -1) return(-1) 
#returns:       THE BONUS TO THE SKILL FROM RANKS.
    if( x["Skills.Name"] == "Body Development" ){
            
            bd.Progress <- as.numeric( unlist ( strsplit( species_Description_DF[species_Description_DF[,"Race"] == char.Species,"Body_Dev"], '\\.' ) ) )
            if (as.numeric(x["NumTimesIncreased"]) == 0 ){
                ans = 0
            } else if( as.numeric(x["NumTimesIncreased"]) <= 10 ){
                ans = as.numeric(x["NumTimesIncreased"])*bd.Progress[2]
            } else if ( as.numeric(x["NumTimesIncreased"]) <= 20 ){
                ans = 10*bd.Progress[2] + (as.numeric(x["NumTimesIncreased"])-10)*bd.Progress[3]
            } else if (  as.numeric(x["NumTimesIncreased"]) <= 30 ){
                ans = 10*bd.Progress[2] + 10*bd.Progress[3] + (as.numeric(x["NumTimesIncreased"])-20)*bd.Progress[4]
            } else {
                ans = floor( 10*bd.Progress[2] + 10*bd.Progress[3] + 10*bd.Progress[4] + (as.numeric(x["NumTimesIncreased"])-30)*bd.Progress[5] )
            }
        
        } else if( x["Skills.Name"] == "Power Point Development" ){
            
            pp.Progress <- as.numeric( unlist ( strsplit( " 0 . 6 . 5 . 4 . 3", ' \\. ' ) ) )
            if (as.numeric(x["NumTimesIncreased"]) == 0 ){
                ans = 0
            } else if( as.numeric(x["NumTimesIncreased"]) <= 10 ){
                ans = as.numeric(x["NumTimesIncreased"])*pp.Progress[2]
            } else if ( as.numeric(x["NumTimesIncreased"]) <= 20 ){
                ans = 10*pp.Progress[2] + (as.numeric(x["NumTimesIncreased"])-10)*pp.Progress[3]
            } else if (  as.numeric(x["NumTimesIncreased"]) <= 30 ){
                ans = 10*pp.Progress[2] + 10*pp.Progress[3] + (as.numeric(x["NumTimesIncreased"])-20)*pp.Progress[4]
            } else {
                ans = floor( 10*pp.Progress[2] + 10*pp.Progress[3] + 10*pp.Progress[4] + (as.numeric(x["NumTimesIncreased"])-30)*pp.Progress[5] )
            }
        
        } else if( x["Skills.Name"] == "Maneuver in Armour" ){
        
            if ( as.numeric(x["NumTimesIncreased"]) == 0 ){
                ans = 0
            }else{
                ans = as.numeric(x["NumTimesIncreased"])*5
            }
        
        } else if( grepl( "Spells", x["Skills.Name"] ) & x["Skills.Name"] != "Directed Spells" ){
            
            spell.Progress <- as.numeric( unlist ( strsplit( "0 . 1 . 1 . 0.5 . 0", ' \\. ' ) ) )
            if (as.numeric(x["NumTimesIncreased"]) == 0 ){
                ans = 0
            } else if( as.numeric(x["NumTimesIncreased"]) <= 10 ){
                ans = as.numeric(x["NumTimesIncreased"])*spell.Progress[2]
            } else if ( as.numeric(x["NumTimesIncreased"]) <= 20 ){
                ans = 10*spell.Progress[2] + (as.numeric(x["NumTimesIncreased"])-10)*spell.Progress[3]
            } else if (  as.numeric(x["NumTimesIncreased"]) <= 30 ){
                ans = 10*spell.Progress[2] + 10*spell.Progress[3] + (as.numeric(x["NumTimesIncreased"])-20)*spell.Progress[4]
            } else {
                ans = floor( 10*spell.Progress[2] + 10*spell.Progress[3] + 10*spell.Progress[4] + (as.numeric(x["NumTimesIncreased"])-30)*spell.Progress[5] )
            }
        
        } else if( x["Skills.Name"] == "Control" ){
        
            if (as.numeric(x["NumTimesIncreased"]) == 0 ){
                ans = -25
            } else if( as.numeric(x["NumTimesIncreased"]) <= 10 ){
                ans = as.numeric(x["NumTimesIncreased"])*5
            } else if ( as.numeric(x["NumTimesIncreased"]) <= 20 ){
                ans = 10*5 + (as.numeric(x["NumTimesIncreased"])-10)*2
            }  else if ( as.numeric(x["NumTimesIncreased"]) <= 30 ){
                ans = 10*5 + 10*2 + (as.numeric(x["NumTimesIncreased"])-20)*1
            } else{
                ans = floor( 10*5 + 10*2 + 10*1 + (as.numeric(x["NumTimesIncreased"])-30)*0.5 )
            }
            
            ans = ans + char.Level*5
        }else{
        
            if (as.numeric(x["NumTimesIncreased"]) == 0 ){
                ans = -25
            } else if( as.numeric(x["NumTimesIncreased"]) <= 10 ){
                ans = as.numeric(x["NumTimesIncreased"])*5
            } else if ( as.numeric(x["NumTimesIncreased"]) <= 20 ){
                ans = 10*5 + (as.numeric(x["NumTimesIncreased"])-10)*2
            }  else if ( as.numeric(x["NumTimesIncreased"]) <= 30 ){
                ans = 10*5 + 10*2 + (as.numeric(x["NumTimesIncreased"])-20)*1
            } else{
                ans = floor( 10*5 + 10*2 + 10*1 + (as.numeric(x["NumTimesIncreased"])-30)*0.5 )
            }
        
        }
    
    return(ans)
}

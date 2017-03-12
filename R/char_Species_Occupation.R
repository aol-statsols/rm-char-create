#' AN OCCUPATION FUNCTION
#'
#' FUNCTION TO GET THE PLAYER'S OCCUPATION BASED ON THEIR SPECIES/SOCIAL CLASS
#' @param background.Social_Class VARIABLE THAT DETERMINES WHERE THEY ARE ON THE SLAVE-NOBLE SPECTRUM.
#' @param occupation.List LIST CONTAINING ALL POSSIBLE OCCUPATIONS, WITH A DATAFRAME FOR EACH CLASS INCLUDED.
#' @param background.Occupation CONTAINS EITHER THE OCCUPATION OF THE CHARACTER'S PARENTS OR -1
#' @concept occupation 
#' @export
#' @examples
#' char_Species_Occupation()

char_Species_Occupation <-
function( background.Social_Class = -1, occupation.List = -1, background.Occupation = -1){

if(unlist(background.Social_Class)[1] == -1 & unlist(occupation.List)[1] == -1 & unlist(background.Occupation)[1] == -1) return(-1)
#returns LIST WITH THREE ITEMS. 1) THE ROW WITH ALL DETAILS ON OCCUPATION. 2) THE NORMAL SKILLS INCREASED BY SPECIFIC AMOUNTS. 3) THE "ANY" SKILLS INCREASED BY HOWEVER MUCH.
    
    if( background.Social_Class == "Guilded" ){ occupation.Bonuses = occupation.List[[1]][which(occupation.List[[1]][,"Occupation"] == background.Occupation),]
        } else if( background.Social_Class == "Noble" ){ occupation.Bonuses = occupation.List[[2]][which(occupation.List[[2]][,"Occupation"] == background.Occupation),]
        } else if( background.Social_Class == "Serf" ){ occupation.Bonuses = occupation.List[[3]][which(occupation.List[[3]][,"Occupation"] == background.Occupation),]
        } else if( background.Social_Class == "Slave" ){ occupation.Bonuses = occupation.List[[4]][which(occupation.List[[4]][,"Occupation"] == background.Occupation),]
        } else if( background.Social_Class == "Unguilded" ){ occupation.Bonuses = occupation.List[[5]][which(occupation.List[[5]][,"Occupation"] == background.Occupation),]
    }
   
    background.Occupation.Skills.Increased.Name <- unlist( strsplit( occupation.Bonuses[,"Skills"], ";" ) )
    background.Occupation.Skills.Increased.Amount <- unlist( strsplit( occupation.Bonuses[,"Ranks"], ";" ) )
    
    background.Occupation.Skills.Increased.Name.Normal <-background.Occupation.Skills.Increased.Name[ !grepl( "ANY",background.Occupation.Skills.Increased.Name ) & !grepl( "TOTAL",background.Occupation.Skills.Increased.Amount )  & !grepl( "Total",background.Occupation.Skills.Increased.Amount ) ]
    background.Occupation.Skills.Increased.Amount.Normal <- as.numeric(background.Occupation.Skills.Increased.Amount[ !grepl( "ANY",background.Occupation.Skills.Increased.Name ) & !grepl( "TOTAL",background.Occupation.Skills.Increased.Amount )  & !grepl( "Total",background.Occupation.Skills.Increased.Amount ) ] )
    background.Occupation.Skills.Normal <- data.frame( Skill = background.Occupation.Skills.Increased.Name.Normal, Ranks = background.Occupation.Skills.Increased.Amount.Normal, stringsAsFactors = FALSE)
    
    background.Occupation.Skills.Increased.Name.ANY <-background.Occupation.Skills.Increased.Name[ grepl( "ANY",background.Occupation.Skills.Increased.Name ) | grepl( "TOTAL",background.Occupation.Skills.Increased.Amount ) | grepl( "Total",background.Occupation.Skills.Increased.Amount ) ]
    background.Occupation.Skills.Increased.Amount.ANY <- as.numeric(background.Occupation.Skills.Increased.Amount[ grepl( "ANY",background.Occupation.Skills.Increased.Name ) | grepl( "TOTAL",background.Occupation.Skills.Increased.Amount ) | grepl( "Total",background.Occupation.Skills.Increased.Amount ) ] )
    background.Occupation.Skills.ANY <- data.frame( Skill = background.Occupation.Skills.Increased.Name.ANY, Ranks = background.Occupation.Skills.Increased.Amount.ANY, stringsAsFactors = FALSE)
    
    return( list(occupation.Bonuses,background.Occupation.Skills.Normal,background.Occupation.Skills.ANY) )
    
}

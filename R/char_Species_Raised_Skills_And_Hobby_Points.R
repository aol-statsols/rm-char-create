#' A SPECIES BORN AND SPECIES RAISED FUNCTION
#'
#' FUNCTION WHICH DETERMINES WHAT SKILL AND HOBBY POINTS THEIR BACKGROUND GIVES THEM  
#' @param char.Species.Raised WHAT SPECIES THEY WERE RAISED AS
#' @param char_Skills_Background WHAT SKILLS ARE AVAILABLE FROM DIFFERENT BACKGROUNDS
#' @param char.Society WHAT KIND OF SOCIETY ARE THEY FROM?
#' @param background.Social_Class WHAT CLASS ARE THEY FROM - GUILDED, UNGUILDED ETC. 
#' @param urban.Chance WHAT CHANCE DOES THAT JOB HAVE OF BEING URBAN?
#' @concept level character 
#' @export
#' @examples
#' char_Species_Raised_Skills_And_Hobby_Points()

char_Species_Raised_Skills_And_Hobby_Points <-
function( char.Species.Raised = -1, char_Skills_Background = -1, char.Society = -1, background.Social_Class = -1, urban.Chance = -1 ){ 

if(unlist(char.Species.Raised)[1] == -1 & unlist(char_Skills_Background)[1] == -1 & unlist(char.Society)[1] == -1 & unlist(background.Social_Class)[1] == -1
	& unlist(urban.Chance)[1] == -1) return(-1)     
#returns LIST WITH 3 ITEMS. 1) NORMAL SKILLS INCREASED AND HOW MANY RANKS. 2) "ANY" SKILLS INCREASED AND HOW MANY RANKS. 3) HOBBY RANKS.

char.Species.Raised <- gsub( " ", "_", char.Species.Raised )

    if( grepl("Man", char.Species.Raised) | grepl("Human", char.Species.Raised) ){
        
        if( char.Society == "Tribal" ){
            background.Social.Skills <- char_Skills_Background[,c(1,which( grepl( "Tribal.Man", colnames(char_Skills_Background) ) ) )]
            background.Social.Skills[,2] <- as.numeric( gsub( "-", 0, background.Social.Skills[,2] ) )
            background.Social.Hobby_Points <- background.Social.Skills[nrow(char_Skills_Background),2]
            background.Social.Skills <- background.Social.Skills[-nrow(char_Skills_Background),]
            background.Social.Skills.Normal <- background.Social.Skills[ !grepl( "ANY", background.Social.Skills[,1] ) & !grepl("Lore",background.Social.Skills[,1]), ]
            background.Social.Skills.ANY <- background.Social.Skills[ grepl( "ANY", background.Social.Skills[,1] ) | grepl("Lore",background.Social.Skills[,1]), ]
        } else if( background.Social_Class == "Noble" ){
            background.Social.Skills <- char_Skills_Background[,c(1,which( grepl( "Noble.Man", colnames(char_Skills_Background) ) ) )]
            background.Social.Skills[,2] <- as.numeric( gsub( "-", 0, background.Social.Skills[,2] ) )
            background.Social.Hobby_Points <- background.Social.Skills[nrow(char_Skills_Background),2]
            background.Social.Skills <- background.Social.Skills[-nrow(char_Skills_Background),]
            background.Social.Skills.Normal <- background.Social.Skills[ !grepl( "ANY", background.Social.Skills[,1] ) & !grepl("Lore",background.Social.Skills[,1]), ]
            background.Social.Skills.ANY <- background.Social.Skills[ grepl( "ANY", background.Social.Skills[,1] ) | grepl("Lore",background.Social.Skills[,1]), ]
        } else{
            urban.Or.Rural <- ifelse( sample( 100, 1 ) <= urban.Chance, "Urban", "Rural" ) 
            
            background.Social.Skills <- char_Skills_Background[,c(1,which( grepl( paste0(urban.Or.Rural,".Man"), colnames(char_Skills_Background) ) ) )]
            background.Social.Skills[,2] <- as.numeric( gsub( "-", 0, background.Social.Skills[,2] ) )
            background.Social.Hobby_Points <- background.Social.Skills[nrow(char_Skills_Background),2]
            background.Social.Skills <- background.Social.Skills[-nrow(char_Skills_Background),]
            background.Social.Skills.Normal <- background.Social.Skills[ !grepl( "ANY", background.Social.Skills[,1] ) & !grepl("Lore",background.Social.Skills[,1]), ]
            background.Social.Skills.ANY <- background.Social.Skills[ grepl( "ANY", background.Social.Skills[,1] ) | grepl("Lore",background.Social.Skills[,1]), ]
        } 
        
    } else if( grepl( "High_Elf", char.Species.Raised ) ){
        background.Social.Skills <- char_Skills_Background[,c(1,which( colnames(char_Skills_Background) == "High.Elf" ) )]
        background.Social.Skills[,2] <- as.numeric( gsub( "-", 0, background.Social.Skills[,2] ) )
        background.Social.Hobby_Points <- background.Social.Skills[nrow(char_Skills_Background),2]
        background.Social.Skills <- background.Social.Skills[-nrow(char_Skills_Background),]
        background.Social.Skills.Normal <- background.Social.Skills[ !grepl( "ANY", background.Social.Skills[,1] ) & !grepl("Lore",background.Social.Skills[,1]), ]
        background.Social.Skills.ANY <- background.Social.Skills[ grepl( "ANY", background.Social.Skills[,1] ) | grepl("Lore",background.Social.Skills[,1]), ]
    } else if( grepl( "Wood_Elf", char.Species.Raised ) ){
        background.Social.Skills <- char_Skills_Background[,c(1,which( colnames(char_Skills_Background) == "Wood.Elf" ) )]
        background.Social.Skills[,2] <- as.numeric( gsub( "-", 0, background.Social.Skills[,2] ) )
        background.Social.Hobby_Points <- background.Social.Skills[nrow(char_Skills_Background),2]
        background.Social.Skills <- background.Social.Skills[-nrow(char_Skills_Background),]
        background.Social.Skills.Normal <- background.Social.Skills[ !grepl( "ANY", background.Social.Skills[,1] ) & !grepl("Lore",background.Social.Skills[,1]), ]
        background.Social.Skills.ANY <- background.Social.Skills[ grepl( "ANY", background.Social.Skills[,1] ) | grepl("Lore",background.Social.Skills[,1]), ]
    } else if( grepl( "Dwarf", char.Species.Raised ) ){
        background.Social.Skills <- char_Skills_Background[,c(1,which( colnames(char_Skills_Background) == "Dwarf" ) )]
        background.Social.Skills[,2] <- as.numeric( gsub( "-", 0, background.Social.Skills[,2] ) )
        background.Social.Hobby_Points <- background.Social.Skills[nrow(char_Skills_Background),2]
        background.Social.Skills <- background.Social.Skills[-nrow(char_Skills_Background),]
        background.Social.Skills.Normal <- background.Social.Skills[ !grepl( "ANY", background.Social.Skills[,1] ) & !grepl("Lore",background.Social.Skills[,1]), ]
        background.Social.Skills.ANY <- background.Social.Skills[ grepl( "ANY", background.Social.Skills[,1] ) | grepl("Lore",background.Social.Skills[,1]), ]
    } else if( grepl( "Halfling", char.Species.Raised ) ){
        background.Social.Skills <- char_Skills_Background[,c(1,which( colnames(char_Skills_Background) == "Hobbit" ) )]
        background.Social.Skills[,2] <- as.numeric( gsub( "-", 0, background.Social.Skills[,2] ) )
        background.Social.Hobby_Points <- background.Social.Skills[nrow(char_Skills_Background),2]
        background.Social.Skills <- background.Social.Skills[-nrow(char_Skills_Background),]
        background.Social.Skills.Normal <- background.Social.Skills[ !grepl( "ANY", background.Social.Skills[,1] ) & !grepl("Lore",background.Social.Skills[,1]), ]
        background.Social.Skills.ANY <- background.Social.Skills[ grepl( "ANY", background.Social.Skills[,1] ) | grepl("Lore",background.Social.Skills[,1]), ]
    } else if( grepl( "Gnome", char.Species.Raised ) ){
        background.Social.Skills <- char_Skills_Background[,c(1,which( colnames(char_Skills_Background) == "Gnome" ) )]
        background.Social.Skills[,2] <- as.numeric( gsub( "-", 0, background.Social.Skills[,2] ) )
        background.Social.Hobby_Points <- background.Social.Skills[nrow(char_Skills_Background),2]
        background.Social.Skills <- background.Social.Skills[-nrow(char_Skills_Background),]
        background.Social.Skills.Normal <- background.Social.Skills[ !grepl( "ANY", background.Social.Skills[,1] ) & !grepl("Lore",background.Social.Skills[,1]), ]
        background.Social.Skills.ANY <- background.Social.Skills[ grepl( "ANY", background.Social.Skills[,1] ) | grepl("Lore",background.Social.Skills[,1]), ]
    }
    
    return( list( background.Social.Skills.Normal, background.Social.Skills.ANY, background.Social.Hobby_Points ) )
    
}

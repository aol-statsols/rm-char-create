#' A CHARACTER BUILDING FUNCTION
#'
#' GETS THE SPECIES, NAME, PLAYER, SEX AND SOCIETY FOR THE CHARACTER
#' @param char.Species SPECIES THE CHARACTER IS (-1 MEANS YET TO BE DECIDED)
#' @param char.Name NAME OF THE CHARACTER (-1 MEANS YET TO BE DECIDED)
#' @param char.Player NAME OF THE PLAYER (-1 MEANS YET TO BE DECIDED)
#' @param char.Sex SEX OF THE CHARACTER (-1 MEANS YET TO BE DECIDED)
#' @param char.Society SOCIETY THE CHARACTER GREW UP IN (-1 MEANS YET TO BE DECIDED)
#' @param char.Level LEVEL OF THE CHARACTER (-1 MEANS YET TO BE DECIDED)
#' @param char_Social_Class DATAFRAME OF POSSIBLE SOCIAL CLASSES THE CHARACTER CAN HAVE
#' @param species_Description_DF DATAFRAME OF POSSIBLE SPECIES THE CHARACTER CAN BE
#' @concept species name player sex society level 
#' @export
#' @examples
#' char_Basic_Info()

char_Basic_Info <-
function( char.Species = -1, char.Name = -1, char.Player = -1, char.Sex = -1, char.Society = -1, char.Level = -1, 
                                char_Social_Class = -1, species_Description_DF = -1 ){ #

if(unlist(char.Species)[1] == -1 & unlist(char.Name)[1] == -1 & unlist(char.Player)[1] == -1 & unlist(char.Sex)[1] == -1 & 
	unlist(char.Society)[1] == -1 & unlist(char.Level)[1] == -1 &
    unlist(char_Social_Class)[1] ==  -1 & unlist(species_Description_DF)[1] == -1) return(-1)
#returns A LIST CONTAINING THE BASIC INFO FOR THE CHARACTER
    if( unlist(char.Species)[1] == -1 ) {
char.Species <- choose_Species(species_Description_DF)  #CHOOSE THE CHARACTER'S SPECIES
}
    print(char.Species)
    if( unlist(char.Society)[1] == -1){
        char.Species.Raised <- ifelse( grepl( "Half_Dwarf", char.Species ), 
choose_Generic(paste("culture you were raised in, Human or", gsub("Half_","",char.Species)), c("Human",gsub("Half_","",char.Species))), 
ifelse( grepl( "Half_Elf", char.Species ), 
choose_Generic(paste("culture you were raised in, Human, Wood Elf or High Elf"), c("Human","Wood Elf","High Elf")), 
char.Species ) 
) 
        social.Classes <- colnames(char_Social_Class)[-1]
        
        if(grepl("Dwarf",char.Species.Raised)){
            social.Class.Options <- social.Classes[ grepl("Dwarf",social.Classes) ]
} else if(grepl("Elf",char.Species.Raised)){
            social.Class.Options <- social.Classes[ grepl("Elf",social.Classes) ]
} else if(grepl("Gnome",char.Species.Raised)){
            social.Class.Options <- social.Classes[ grepl("Gnome",social.Classes) ]
}  else if(grepl("Halfling",char.Species.Raised)){
            social.Class.Options <- social.Classes[ grepl("Hobbit",social.Classes) ]
}else if(grepl("Human",char.Species.Raised) | grepl("Man", char.Species.Raised)){
            social.Class.Options <- social.Classes[ !( grepl("Dwarf",social.Classes) | grepl("Elf",social.Classes) | 
                                                                    grepl("Gnome",social.Classes) | grepl("Hobbit",social.Classes) ) ]
}
        
        char.Society = choose_Generic("type of society you come from.",social.Class.Options)   #CHOOSE THE CHARACTER'S SOCIETY, e.g., URBAN MAN, RURAL MAN, HIGH ELF, etc...
}

if( unlist(char.Name)[1] == -1 ){
char.Name = enter_Name("char")  #NAME THE CHARACTER
}

if( unlist(char.Player)[1] == -1 ){
char.Player = enter_Name("player")  #NAME THE PLAYER
}

    if( unlist(char.Sex)[1] == -1 ) {
char.Sex <- choose_Generic("character's sex at birth, for calculation of weight and height.", c("Male", "Female") )  #CHOOSE THE CHARACTER'S SEX
}
    
    #CHOOSE THE MAX LEVEL FOR THE CHARACTER
    if( unlist(char.Level)[1] == -1 ){
cat("Please choose your character's level:\n\n")
char.Level <- read_Integer(0,100)
}

    #AGES AT WHICH THE AGING CRISES HAPPEN
    species.Middle_Aged <-  as.numeric( species_Description_DF[which(species_Description_DF[,1] == char.Species),"Middle_Aged"] )
    species.Old <-  as.numeric( species_Description_DF[which(species_Description_DF[,1] == char.Species),"Old"] )
    species.Venerable <-  as.numeric( species_Description_DF[which(species_Description_DF[,1] == char.Species),"Venerable"] )
    #BACKGROUND OPTIONS POINTS THAT ARE SPENT TO ROLL, IN CASE THERE'S SOMETHING THAT WILL AFFECT THE PLAYER'S CHOICES COMING UP (LIKE BEING EXCEPTIONALLY ENCHANTED)
    background.Option.Points <- species_Description_DF[which(species_Description_DF[,1] == char.Species),"Background"]
    
    
    return( list( c( char.Species, char.Name, char.Player, char.Sex, char.Society, char.Species.Raised ), c( species.Middle_Aged, species.Old, species.Venerable, background.Option.Points, char.Level ) ) )
}

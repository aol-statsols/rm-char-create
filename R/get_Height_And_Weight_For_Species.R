#' A CHARACTER BACKGROUND FUNCTION
#'
#' FUNCTION TO GET THE HEIGHT AND WEIGHT FOR THE PLAYER'S SPECIES
#' @param char.Species THE SPECIES OF THE CHARACTER
#' @param char.Sex THE Sex OF THE CHARACTER
#' @concept height weight
#' @export
#' @examples
#' get_Height_And_Weight_For_Species()

get_Height_And_Weight_For_Species <-
function( char.Species = -1, char.Sex = -1 ){ 

if(unlist(char.Species)[1] == -1 & unlist(char.Sex)[1] == -1) return(-1) 
#returns A VECTOR CONTAINING THE HEIGHT AND WEIGHT GENERATING PARTS FOR A SPECIES AND GENDER.
species = ""
if( char.Species %in% species_Description_DF[1:2,1] ){
species = "Human"
} else if( char.Species %in% species_Description_DF[3,1] ){
species = "High Man"
} else if( char.Species %in% species_Description_DF[4,1] ){
species = "Dwarf"
} else if( char.Species %in% species_Description_DF[5,1] ){
species = "Half-Dwarf"
} else if( char.Species %in% species_Description_DF[6,1] ){
species = "Gnome"
} else if( char.Species %in% species_Description_DF[7:9,1] ){
species = "Halfling"
} else if( char.Species %in% species_Description_DF[10,1] ){
species = "Half-elf"
} else if( char.Species %in% species_Description_DF[11:12,1] ){
species = "Elf"
}
char_Height_And_Weight[char_Height_And_Weight[,1] == species & char_Height_And_Weight[,2] == paste0("(",char.Sex,")"),]

}

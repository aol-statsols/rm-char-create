#' A HEIGHT AND WEIGHT FUNCTION
#'
#' CACULATES HEIGHT, WEIGHT, AND MOVE MOD FOR THE CHARACTER
#' @param char.Species STRING CONTAINING CHARACTER'S SPECIES
#' @param char.Sex STRING CONTAINING CHARACTER'S SEX
#' @param char.Height = -1 NUMERIC VARIABLE CONTAINING CHARACTER'S HEIGHT
#' @param char.Weight = -1 NUMERIC VARIABLE CONTAINING CHARACTER'S WEIGHT
#' @concept character description
#' @export
#' @examples
#' calculate_Height_And_Weight()

calculate_Height_And_Weight <-
function( char.Species = -1, char.Sex = -1, char.Height = -1, char.Weight = -1 ){ #

if(unlist(char.Species)[1] == -1 & unlist(char.Sex)[1] == -1 & unlist(char.Height)[1] == -1 & unlist(char.Weight)[1] == -1) return(-1)
#returns:    A VECTOR WITH THE CHARACTER'S HEIGHT, WEIGHT AND MOVEMENT MOD

hnM = get_Height_And_Weight_For_Species( char.Species, char.Sex )

move.Mods.Inches.Char = char_Movement_Mods
move.Mods.Inches = t( apply( move.Mods.Inches.Char, 1, function(x){
tmp = as.numeric( unlist( strsplit( gsub( "\"", "", x[1] ), "'"  ) ) )
ans = tmp[1]*12 + tmp[2]
c( ans , as.numeric( x[2] ) )
} ) )
height.Base.Split =  as.numeric( unlist( strsplit( gsub( "\"", "", hnM["Base.Height"] ), "'"  ) ) )
height.Base.Inches = height.Base.Split[1]*12 + height.Base.Split[2]
if( unlist(char.Height)[1] == -1 ){
height.Dice = as.numeric( unlist( strsplit( gsub( "\\+", "", hnM["Height.Mod"] ), "d"  ) ) )

height.Mod = sum( sample( 1:height.Dice[2], height.Dice[1], replace = TRUE ) )
char.Height =  height.Base.Inches + height.Mod
if( grepl( "Tallfellow", char.Species ) ) char.Height = char.Height + 4
}

if( unlist(char.Weight)[1] == -1 ){
weight.Base.Lb = as.numeric( hnM["Base.Weight..lb."] )
weight.Dice = as.numeric( unlist( strsplit( gsub( "[\\*\\(\\)]", "", hnM["Weight.Mod"] ), "d"  ) ) )
weight.Mod = sum( sample( 1:weight.Dice[2], weight.Dice[1], replace = TRUE ) )

height.Mod = char.Height - height.Base.Inches
char.Weight =  weight.Base.Lb + height.Mod*weight.Mod
        if( grepl( "Tallfellow", char.Species ) ) char.Height = char.Height + 4
if( grepl( "Stout", char.Species ) ) char.Weight = char.Weight + 20
}
move.Mod = 10 + move.Mods.Inches[max(which(move.Mods.Inches >= char.Height)),2]

return(c(char.Height,char.Weight,move.Mod))

}

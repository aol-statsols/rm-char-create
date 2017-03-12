#' A SPECIES FUNCTION
#'
#' FUNCTION TO LET THE USER CHOOSE WHAT SPECIES THEY ARE
#' @param species_Description_DF A DATAFRAME WITH A FULL DESCRIPTION OF EACH SPECIES - BONUSES, AGE, etc...
#' @concept species 
#' @export
#' @examples
#' choose_Species()

choose_Species <-
function(species_Description_DF = -1){ 

if(unlist(species_Description_DF)[1] == -1) return(-1) 
#returns:                   STRING CONTAINING THE NAME OF THE SPECIES CHOSEN.
numberOfSpecies <- nrow(species_Description_DF)
cat("What species do you want to play as?\n")

    species_Description_DF.PRINT <- species_Description_DF
    species_Description_DF.PRINT[,1] <- gsub("_", " ", species_Description_DF.PRINT[,1])
    colnames(species_Description_DF.PRINT)[2:11] <- stat_Name_DF[,1]
    colnames(species_Description_DF.PRINT)<-  gsub("_", " ", colnames(species_Description_DF.PRINT))
    print(species_Description_DF.PRINT)

    strTmp <- gsub( " ", "_", choose_Generic( "Species", gsub( "_" , " ", species_Description_DF[,1] ) ) )

    if( is.na( suppressWarnings( as.numeric(strTmp) ) ) ){
if( strTmp %in% species_Description_DF[,1]){
species = strTmp
return(species)
} else{
if (interactive() ){ readline(prompt=paste0("The name must be from ", paste(species_Description_DF[,1],collapse=", ")))} else{ cat("The name must be from ", paste(species_Description_DF[,1],collapse=", ")); readLines("stdin",1)  }
return( choose_Species(species_Description_DF) )
}
} else{
numTmp <- as.numeric(strTmp)
if( 1 <= numTmp  &&  numTmp <= numberOfSpecies ){
species = species_Description_DF[numTmp,1]
return(species)
} else{
if (interactive() ){ readline(prompt=paste0("The number must be between 1 and ", numberOfSpecies))} else{ cat("The number must be between 1 and ", numberOfSpecies); readLines("stdin",1)  }
return( choose_Species(species_Description_DF) )
}
}

}

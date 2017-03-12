#' A PDF GENERATOR FUNCTION
#'
#' CREATED IMAGES OF EACH PAGE OF THE CHARACTER SHEET, OUTPUTS THEM AS A PDF, THEN DELETES THEM BY DEFAULT
#' @param species_Description_DF DATAFRAME OF POSSIBLE SPECIES THE CHARACTER CAN BE
#' @param char.Species SPECIES THE CHARACTER IS (-1 MEANS YET TO BE DECIDED)
#' @param stats.End FINAL STATS OF THE CHARACTER    
#' @param char.Realm CHARACTER'S SPELLCASTING REALM
#' @param char.Class CHARACTER'S CLASS
#' @param char.Class.FullName CHARACTER'S CLASS'S FULL NAME
#' @param char.Level LEVEL OF THE CHARACTER (-1 MEANS YET TO BE DECIDED)
#' @param char.Height NUMERIC CHARACTER'S HEIGHT
#' @param char.Weight NUMERIC CHARACTER'S WEIGHT
#' @param char.Move_Mod NUMERIC CHARACTER'S Move Mod
#' @param char.Name NAME OF THE CHARACTER (-1 MEANS YET TO BE DECIDED)
#' @param char.Player NAME OF THE PLAYER (-1 MEANS YET TO BE DECIDED)
#' @param resist.Realm SPELLCASTING REALM CHARACTER IS RESISTANT TO
#' @param adept.Realm SPELLCASTING REALM CHARACTER IS ADEPT WITH
#' @param background_Options.Benefits LIST CONTAINING ALL BACKGROUND OPTION BENEFITS A PLAYER HAS TAKEN
#' @param skills.Final.Level DATAFRAME CONTAINING ALL SKILL OUTPUTS GENERATED DURING LEVELING.
#' @param class_Costs_And_Info DATAFRAME CONTAINING THE COSTS AND STATS ASSOCIATED WITH SKILLS FOR ALL PROFESSIONS.
#' @param char_Profession_Bonus VECTOR WITH THE BONUSES TO SKILLS FROM PROFESSIONS.
#' @param skills.Primary.Stats DF OF THE PRIMARY SKILLS AND THE STATS ASSOCIATED WITH THEM.
#' @param skills.Secondary.Stats DF OF THE SECONDARY SKILLS AND THE STATS ASSOCIATED WITH THEM.
#' @param tp.Skills.Other_Gains TRAINING PACKAGE GAINS THAT AREN'T STATS/SKILLS
#' @param tp.Name TRAINING PACKAGE NAME
#' @param background_Options DATAFRAME OF BACKGROUND OPTIONS AVAILABLE
#' @param char.Flaws DATAFRAME CONTAINING ALL FLAWS THE CHARACTER TOOK
#' @param character.Life LIST CONTAINING THE VARIOUS FACTS OF THE CHARACTER'S LIFE
#' @param char.Age.List LIST CONTAINING THE CHARACTER'S AGE AT EACH LEVEL
#' @param img.Size STRING CONTAINING THE SIZE - "BIG" "MED" - OF THE IMAGE DESIRED.
#' @param deleteImages TRUE/FALSE - SHOULD THE IMAGES BE DELETED AFTER THEY HAVE CREATED THE pdf OF THE CHARACTER SHEET
#' @concept output pdf img png character
#' @export
#' @examples
#' character_Sheet_Create()

character_Sheet_Create <-
function( species_Description_DF = -1, char.Species = -1, stats.End = -1, char.Realm = -1, char.Class = -1, char.Class.FullName = -1, char.Level = -1, 
			char.Height = -1, char.Weight = -1, char.Move_Mod = -1, 
			char.Name = -1, char.Player = -1, resist.Realm = -1, adept.Realm = -1, background_Options.Benefits = -1,
			skills.Final.Level = -1, class_Costs_And_Info = -1, char_Profession_Bonus = -1, skills.Primary.Stats = -1, skills.Secondary.Stats = -1, 
			tp.Skills.Other_Gains = -1, tp.Name = -1,
			background_Options = -1, char.Flaws = -1, 
			character.Life = -1, char.Age.List = -1, 
			img.Size="MED", deleteImages = TRUE 
		){ 
		
if(unlist(species_Description_DF)[1] == -1 & unlist(char.Species)[1] == -1 & unlist(stats.End)[1] == -1 & unlist(char.Realm)[1] == -1 & unlist(char.Class)[1] == -1 &
	unlist(char.Class.FullName)[1] == -1 & unlist(char.Level)[1] == -1 & unlist(char.Height)[1] == -1 & unlist(char.Weight)[1] == -1 & unlist(char.Move_Mod)[1] == -1 &
	unlist(char.Name)[1] == -1 & unlist(char.Player)[1] == -1 & unlist(resist.Realm)[1] == -1 & unlist(adept.Realm)[1] == -1 & unlist(background_Options.Benefits)[1] == -1 &
	unlist(skills.Final.Level)[1] == -1 & unlist(class_Costs_And_Info)[1] == -1 & unlist(char_Profession_Bonus)[1] == -1 & 
	unlist(skills.Primary.Stats)[1] == -1 & unlist(skills.Secondary.Stats)[1] == -1 &
	unlist(tp.Skills.Other_Gains)[1] == -1 & unlist(tp.Name)[1] == -1 &
	unlist(background_Options)[1] == -1 & length(char.Flaws)[1] == 0 &
	unlist(character.Life)[1] == -1 & unlist(char.Age.List)[1] == -1 &
	unlist(img.Size) == "MED"  & unlist(deleteImages) == TRUE ) return(-1)      
	
if(length(char.Flaws)[1] > 0)    if(unlist(species_Description_DF)[1] == -1 & unlist(char.Species)[1] == -1 & unlist(stats.End)[1] == -1 & unlist(char.Realm)[1] == -1 & unlist(char.Class)[1] == -1 &
	unlist(char.Class.FullName)[1] == -1 & unlist(char.Level)[1] == -1 & unlist(char.Height)[1] == -1 & unlist(char.Weight)[1] == -1 & unlist(char.Move_Mod)[1] == -1 &
	unlist(char.Name)[1] == -1 & unlist(char.Player)[1] == -1 & unlist(resist.Realm)[1] == -1 & unlist(adept.Realm)[1] == -1 & unlist(background_Options.Benefits)[1] == -1 &
	unlist(skills.Final.Level)[1] == -1 & unlist(class_Costs_And_Info)[1] == -1 & unlist(char_Profession_Bonus)[1] == -1 & 
	unlist(skills.Primary.Stats)[1] == -1 & unlist(skills.Secondary.Stats)[1] == -1 &
	unlist(tp.Skills.Other_Gains)[1] == -1 & unlist(tp.Name)[1] == -1 &
	unlist(background_Options)[1] == -1 &
	unlist(character.Life)[1] == -1 & unlist(char.Age.List)[1] == -1 &
	unlist(img.Size) == "MED"  & unlist(deleteImages) == TRUE ) return(-1)      
    
#returns NULL                  

    #CALCULATES RANK BONUS AND CREATES A DF FOR OUTPUT
    bd.Progress <- as.numeric( unlist ( strsplit( species_Description_DF[species_Description_DF[,"Race"] == char.Species,"Body_Dev"], '\\.' ) ) )
    if( "High Pain Threshold" %in% unlist(background_Options.Benefits) ){
        bd.Progress[2] <- bd.Progress[2] + 2
        bd.Progress[3:length(bd.Progress)] <- bd.Progress[3:length(bd.Progress) ] + 1
    }
    rank.Bonus <-  apply(skills.Final.Level,1,rank_Bonus_Calculator,bd.Progress,char.Species,char.Level)
    
    skills.OUTPUT <- data.frame(skills.Final.Level, Rank_Bonus = rank.Bonus )
    skills.OUTPUT <- apply(skills.OUTPUT,2,as.character)
    
    #RESET THE CLASS NAME
    if( char.Class %in% c( "Alchemist", "Monk", "Bard" ) ){
        char.Class <- char.Class.FullName
    }
    
    #GENERATES THE PDF.
    bonus.Total <- first_Page_Generator(species_Description_DF, char.Species, stats.End, char.Realm, char.Class, char.Level, char.Height, char.Weight, char.Move_Mod, 
char.Name, char.Player, resist.Realm, adept.Realm, background_Options.Benefits, skills.OUTPUT, img.Size)
    gc()
    
    second_Page_Generator(skills.OUTPUT, class_Costs_And_Info, char_Profession_Bonus, char.Class, skills.Primary.Stats, skills.Secondary.Stats, bonus.Total, 
char.Realm, char.Species, species_Description_DF, img.Size)
    gc()
    
char.Flaws = background_Options.Benefits[[2]]
    
third_Page_Generator( tp.Skills.Other_Gains, tp.Name,
background_Options, char.Flaws, background_Options.Benefits, 
                            character.Life, char.Age.List, resist.Realm, adept.Realm, img.Size )
                 
    character_Sheet_Generator( char.Name, deleteImages )   
    gc()
    
    #CHECK DO THEY WANT TO OPEN THE PDF NOW THAT IT'S CREATED
    ans <- read_Yes_Or_No("Open the pdf of the character sheet?")
    if( ans == "Yes" ) system(paste0('open \"', char.Name, '.pdf\"'))
}

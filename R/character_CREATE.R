#' A CHARACTER SHEET CREATOR FUNCTION
#'
#' FUNCTION WHICH CREATES THE CHARACTER AND THEN GENERATES A CHARACTER SHEET
#' @param char.Species SPECIES THE CHARACTER IS (-1 MEANS YET TO BE DECIDED)
#' @param char.Sex THE SEX OF THE CHARACTER (-1 MEANS YET TO BE DECIDED)
#' @param char.Society WHAT KIND OF SOCIETY ARE THEY FROM? (-1 MEANS YET TO BE DECIDED)
#' @param char.Class CHARACTER'S CLASS (-1 MEANS YET TO BE DECIDED)
#' @param char.Level LEVEL OF THE CHARACTER (-1 MEANS YET TO BE DECIDED)
#' @param stat.Mat MATRIX OF TEMPORARY AND POTENTIAL STATS (-1 MEANS YET TO BE DECIDED)
#' @param char.Realm CHARACTER'S SPELLCASTING REALM (-1 MEANS YET TO BE DECIDED)
#' @param background.No_of_Children DATAFRAME CONTAINING THE WAYS TO CALCULATE THE NUMBER OF CHILDREN FOR EACH SPECIES
#' @param background.Sibling_Rank DATAFRAME CONTAINING THE WAYS TO CALCULATE THE NUMBER CHILD THE PLAYER WAS
#' @param background.Family_Status DATAFRAME CONTAINING THE DIFFERENT POSSIBLE STATUSES FOR A FAMILY FOR EACH SPECIES, e.g., OFFSPRING, ADOPTED, etc.
#' @param background.Environment.Raised DATAFRAME CONTAINING THE DIFFERENT POSSIBLE FAMILY ENVIRONMENTS FOR EACH SPECIES, e.g., BASTARD, STILL TOGETHER, etc.
#' @param background.Clanhead.Relation DATAFRAME CONTAINING THE DIFFERENT POSSIBLE RELATIONSHIPS BETWEEN THE CHARACTER AND THE CLANHEAD
#' @param background.Clan.Status DATAFRAME CONTAINING THE DIFFERENT POSSIBLE STATUSES THE CHARACTER CAN HAVE IN THE CLAN
#' @param background.Social_Class DATAFRAME CONTAINING THE DIFFERENT POSSIBLE SOCIAL CLASSES OF THE FAMILY A CHARACTER IS RAISED IN
#' @param background.Occupation DATAFRAME CONTAINING THE DIFFERENT POSSIBLE OCCUPATIONS OF THE FAMILY A CHARACTER IS RAISED IN
#' @param char.Age = -1 CHARACTER'S CURRENT AGE
#' @param char.Height NUMERIC CHARACTER'S HEIGHT
#' @param char.Weight NUMERIC CHARACTER'S WEIGHT
#' @param char.Name NAME OF THE CHARACTER (-1 MEANS YET TO BE DECIDED)
#' @param char.Player NAME OF THE PLAYER (-1 MEANS YET TO BE DECIDED)
#' @param contact.Info = -1 DATAFRAME CONTAINING THE DIFFERENT POSSIBLE OCCUPATIONS OF THE FAMILY A CHARACTER IS RAISED IN
#' @param background_Options.Benefits.ROLLING LIST CONTAINING ALL BACKGROUND OPTION BENEFITS A PLAYER HAS TAKEN
#' @param background_Options.Benefits.NO.ROLLING LIST CONTAINING ALL BACKGROUND OPTION BENEFITS A PLAYER HAS TAKEN
#' @param tp.Choice EITHER A LIST CONTAINING ALL TP RESULTS OR -1
#' @param level_0_Skills.Increase.Record.List LIST CONTAINING ALL THE level_0 INFORMATION IF THAT HAS BEEN PREGENERATED
#' @param level_1_Skills.Increase.Record.List RESULT OF LEVELING UP AT LEVEL 1
#' @param char.Age.List LIST CONTAINING THE CHARACTER'S AGE AT EACH LEVEL
#' @param skills.Each.Level.List LIST CONTAINING DATAFRAMES FOR THE SKILLS LEVELED UP EACH LEVEL
#' @param level_Up.Result.List EITHER A LIST CONTAINING ALL RESULTS FOR LEVELING UP FOR ***STATS*** WHICH HAVE BEEN PREGENERATED OR -1
#' @param level_Up.Repeated.Result.List = -1 EITHER A LIST CONTAINING ALL RESULTS FOR LEVELING UP FOR ***SKILLS*** WHICH HAVE BEEN PREGENERATED OR -1
#' @param img.Size STRING CONTAINING THE SIZE - "BIG" "MED" - OF THE IMAGE DESIRED.
#' @param EXAMPLE BOOLEAN VARIABLE. IF IT'S AN EXAMPLE, IT RETURNS -1 SO THAT THE PACKAGE CAN BE CHECKED.
#' @concept output pdf img png character input
#' @export
#' @examples
#' character_CREATE( EXAMPLE = TRUE)

character_CREATE <-
function( char.Species = -1, char.Name = -1, char.Player = -1, char.Sex = -1, char.Society = -1, char.Class = -1, char.Level = -1, stat.Mat = -1, 
                                char.Realm = -1, 
                                background.No_of_Children  = -1,  background.Sibling_Rank = -1, background.Family_Status = -1, background.Environment.Raised = -1,
                                background.Clanhead.Relation = -1, background.Clan.Status = -1, background.Social_Class = -1, background.Occupation = -1, 
                                char.Age = -1, char.Height = -1, char.Weight = -1,
                                contact.Info = -1,
                                background_Options.Benefits.ROLLING = -1, background_Options.Benefits.NO.ROLLING = -1, tp.Choice = -1, 
                                level_0_Skills.Increase.Record.List = -1, level_1_Skills.Increase.Record.List = -1, char.Age.List = -1, 
                                skills.Each.Level.List = -1, level_Up.Result.List = -1, level_Up.Repeated.Result.List = -1,
                                img.Size = "MED", EXAMPLE = FALSE
                            ){

#returns NULL

    if( EXAMPLE == TRUE ) return(-1)
	
	
#########################################################################################################
#					FOR NOW, img.Size = "MED" IS ENFORCED, STRICTLY AND WITHOUT MERCY					#
#########################################################################################################

	img.Size = "MED"
	
#########################################################################################################
#					FOR NOW, img.Size = "MED" IS ENFORCED, STRICTLY AND WITHOUT MERCY					#
#########################################################################################################



    stat.Bonus = NULL;stat.Tmp <- NULL; stat.Pot <- NULL; stat.Dev <- NULL; 
    char.Flaws <- NULL
    dev_Points.Level_1 <- NULL
    char.Alive <- TRUE; age_Crisis.Middle_Aged <- FALSE; age_Crisis.Old <- FALSE; age_Crisis.Venerable <- FALSE
recreate.Command <- list()

    stats = stat_Name_DF[,1]
    classes <- classesDF[,2]
    skills_Groups = skills_GroupsDF[,2]
    background_Special_Tables_List <- list(background_Special_Items, background_Special_Wealth, background_Special_Status, background_Special_Family_Ties, background_Special_Curses, background_Special_Abilities)

    
    char.Basics <- char_Basic_Info( char.Species, char.Name, char.Player, char.Sex, char.Society, char.Level, char_Social_Class, species_Description_DF )
    
    char.Species <- char.Basics[[1]][1]
    char.Name <- char.Basics[[1]][2]
    char.Player <- char.Basics[[1]][3]
    char.Sex <- char.Basics[[1]][4]
    char.Society <- char.Basics[[1]][5]
char.Species.Raised <- char.Basics[[1]][6]

    species.Middle_Aged <- char.Basics[[2]][1]
    species.Old <- char.Basics[[2]][2]
    species.Venerable <- char.Basics[[2]][3]
    background.Option.Points <- char.Basics[[2]][4]
    char.Level <- char.Basics[[2]][5]
    
    recreate.Command[[1]] <- c( char.Species, char.Name, char.Player, char.Sex, char.Society )
    recreate.Command[[2]] <- char.Level
    #ASSIGN BACKGROUND OPTION POINTS
    background_Options.Benefits.ROLLING <- background_Options_Assignment_ROLLING( background_Options.Benefits.ROLLING, background.Option.Points, background_Options, 
																					background_Special_Tables_List, flaws_Table )
 
 
    #CHECK IF ANY BACKGROUND OPTIONS AFFECT HOW A PLAYER INTERACTS WITH A REALM OF MAGIC
    realms = c("Essence","Channeling","Mentalism")
    realms.Stats = c("Empathy","Intuition","Presence")
    realms.Stats.Short = c("Em","In","Pr")
    realm.Relations <- char_Realm_Relations( background_Options.Benefits.ROLLING, realms)
    resist.Realm = realm.Relations[1]
    adept.Realm = realm.Relations[2]
 
    
    #CHOOSE THE PLAYER'S PROFESSION AND ASSOCIATED VARIABLES
    class.And.Stats <- char_Class_Decisions_Stat_Assignment( char.Class, resist.Realm, adept.Realm, realms, realms.Stats, realms.Stats.Short, classes, stat.Mat, char.Realm )
    
char.Class <- class.And.Stats[[1]][1]
char.Class.FullName <- class.And.Stats[[1]][2]
stats.Tmp.Pot <- class.And.Stats[[2]]
char.Realm <- class.And.Stats[[3]]
char.Realm.Name <- class.And.Stats[[4]]

recreate.Command[[3]] <- stats.Tmp.Pot
recreate.Command[[4]] <- c( char.Class, char.Realm )

#CREATES THE DATA FRAMES FOR THE SKILLS. RETURNS A LIST CONTAINING ALL OF THEM.
skill.Data.Frames.List <- create_Skill_Data_Frames(char.Class, char.Class.FullName, char.Realm.Name, char.Realm, background_Options.Benefits.ROLLING) 
 
    skills.Increase.Record.DF <- skill.Data.Frames.List[[1]]
    background.Skills.Increase.Record.DF <- skill.Data.Frames.List[[2]]
    skills.Primary.DF <- skill.Data.Frames.List[[3]]
    skills.Primary.Stats <- skill.Data.Frames.List[[4]]
    skills.Secondary.DF <- skill.Data.Frames.List[[5]]
    skills.Secondary.Stats <- skill.Data.Frames.List[[6]]
   
    #USER DECIDES ON HOW TO SPEND BACKGROUND OPTION POINTS
background_Options.Benefits.NO.ROLLING <- background_Options_Assignment_NO_ROLLING( background_Options.Benefits.ROLLING, background_Options.Benefits.NO.ROLLING, 
																						background.Option.Points,stats.Tmp.Pot, stats, background_Options, 
																						skills.Primary.DF, skills.Secondary.DF,
																						skills.Increase.Record.DF,background_Special_Tables_List,flaws_Table,
																						background.Skills.Increase.Record.DF )
	
    if(unlist(background_Options.Benefits.NO.ROLLING)[1] == -1){ 
        stat.Tmp <- stats.Tmp.Pot[,1]
        stat.Pot <- stats.Tmp.Pot[,2]
    } else{ 
        stat.Tmp <- background_Options.Benefits.NO.ROLLING[[3]] 
        stat.Pot <- background_Options.Benefits.NO.ROLLING[[4]]
        background.Skills.Increase.Record.DF <- background_Options.Benefits.NO.ROLLING[[5]]
    }
    
    recreate.Command[[5]] <- background_Options.Benefits.ROLLING
    recreate.Command[[6]] <- background_Options.Benefits.NO.ROLLING
    
    background_Options.Benefits <- background_Options_Choices_Combine( background_Options.Benefits.ROLLING, background_Options.Benefits.NO.ROLLING) #COMBINE THE BACKGROUND OPTION BENEFIT LISTS
    char.Flaws <- background_Options.Benefits[[2]]
	
    #WORK OUT A CHARACTER'S BACKGROUND - FAMILY STUFF, HEIGHT/WEIGHT, ALL THAT JAZZ.
    background.Stuff <- generate_Background( background_Options.Benefits,
                                                background.No_of_Children, background.Sibling_Rank, background.Family_Status, background.Environment.Raised,
                                                background.Clanhead.Relation, background.Clan.Status, background.Social_Class, background.Occupation, contact.Info,
                                                char.Age, char.Species, char.Species.Raised, char.Sex, char.Height, char.Weight, char.Society, char.Class, 
                                                skills.Increase.Record.DF, background.Skills.Increase.Record.DF, level_0_Skills.Increase.Record.List )
background.Stuff[[1]][[7]] = paste0( char.Society, " - ", background.Stuff[[1]][[7]] )
recreate.Command[7:17] = background.Stuff[[1]]

    char.Age = background.Stuff[[1]][[9]]
    char.Age_Die = background.Stuff[[2]]
    char.Height = background.Stuff[[3]]
    char.Weight = background.Stuff[[4]]
    char.Move_Mod = background.Stuff[[5]]
    level_0_Skills.Increase.Record.List = background.Stuff[[6]]
    skills.Increase.Record.DF.Level_1 = background.Stuff[[7]]
     
char.Age.List <- list( char.Age )
     
     
    recreate.Command[[18]] <- level_0_Skills.Increase.Record.List[[1]]
    
    #SEE IF THE PLAYER WANTS TO USE A TRAINING PACKAGE
    tp.Choice <- tp_Choice_Maker(tp.Choice, training_Packages_Cost_DF, training_Packages_Gains, char.Class, stat.Tmp, stat.Pot, char.Realm)
      
    tp.Skills.ANY <- tp.Choice[[1]][[1]]
    tp.Skills.Normal <- tp.Choice[[1]][[2]]
    tp.Skills.Other_Gains <- tp.Choice[[1]][[3]]
    tp.Skills.Stat_Increases  <- tp.Choice[[1]][[4]]
    tp.Name <- tp.Choice[[1]][[6]]
    stat.Tmp <- tp.Choice[[2]][,1]
    stat.Pot <- tp.Choice[[2]][,2]
    stat.Bonus <- tp.Choice[[3]]
    stat.Dev <- tp.Choice[[4]]
    dev_Points.Total <- tp.Choice[[5]]
    dev_Points.Level_1 <- tp.Choice[[6]]
    recreate.Command[[19]] <- tp.Choice[[1]]

    #ASSIGN SKILLS FOR LEVEL 1 AND LEVEL UP
    level_1_Skills.Increase.Record.List <- level_1_Skills(skills.Increase.Record.DF.Level_1, dev_Points.Total, char.Class, background_Options.Benefits, tp.Skills.Normal, tp.Skills.ANY, TRUE,
                                                            level_1_Skills.Increase.Record.List)
    
    level_1_Skills.Increase.Record.DF <- level_1_Skills.Increase.Record.List[[1]]
    level_1_Skills.Increase.Record.DP_Spent <- dev_Points.Total - level_1_Skills.Increase.Record.List[[2]]
recreate.Command[[20]] <- level_1_Skills.Increase.Record.List
    #ASSIGN SKILLS FOR THE OTHER LEVELS AND LEVEL UP
    levelUp_Skills_Stats.Results <- levelUp_Skills_Stats(char.Level, level_Up.Result.List, stat.Tmp, stat.Pot, char.Age, char.Age_Die, char.Age.List, char.Alive, char.Class, 
species.Middle_Aged, species.Old, species.Venerable, age_Crisis.Middle_Aged, age_Crisis.Old, age_Crisis.Venerable,
skills.Increase.Record.DF, background_Options.Benefits,
dev_Points.Level_1, level_1_Skills.Increase.Record.DF, level_1_Skills.Increase.Record.List, level_1_Skills.Increase.Record.DP_Spent,
level_Up.Repeated.Result.List, skills.Each.Level.List
)
    skills.Each.Level.Fully.Assigned <- levelUp_Skills_Stats.Results[[1]]
    stat.List <- levelUp_Skills_Stats.Results[[2]]
                
    stats.End <- stat.List[[length(stat.List)]] #THE CHARACTER'S STATS AT THE END OF LEVELING UP
    
    recreate.Command[22:25] <- levelUp_Skills_Stats.Results[1:4]
    #CREATE THE JSON CONTAINING ALL THE DETAILS ABOUT THE CHARACTER.
    recreate_Command_Generator(recreate.Command, char.Name)
    
    #GENERATES THE PDF.
    character_Sheet_Create(species_Description_DF, char.Species, stats.End, char.Realm, char.Class, char.Class.FullName, char.Level, char.Height, char.Weight, char.Move_Mod, 
                                    char.Name, char.Player, resist.Realm, adept.Realm, background_Options.Benefits,
                                    skills.Each.Level.Fully.Assigned[[length(skills.Each.Level.Fully.Assigned)]][[1]], class_Costs_And_Info, char_Profession_Bonus, 
                                    skills.Primary.Stats, skills.Secondary.Stats, 
                                    tp.Skills.Other_Gains, as.character(tp.Name),
                                    background_Options, char.Flaws, 
                                    background.Stuff[[1]], char.Age.List, 
                                    img.Size, deleteImages = TRUE )
    print("Success!")
}

#' A CHARACTER BACKGROUND FUNCTION
#'
#' FUNCTION TO GENERATE A CHARACTER'S BACKGROUND
#' @param background_Options.Benefits LIST CONTAINING ALL BACKGROUND OPTION BENEFITS A PLAYER HAS TAKEN
#' @param background.No_of_Children DATAFRAME CONTAINING THE WAYS TO CALCULATE THE NUMBER OF CHILDREN FOR EACH SPECIES
#' @param background.Sibling_Rank DATAFRAME CONTAINING THE WAYS TO CALCULATE THE NUMBER CHILD THE PLAYER WAS
#' @param background.Family_Status DATAFRAME CONTAINING THE DIFFERENT POSSIBLE STATUSES FOR A FAMILY FOR EACH SPECIES, e.g., OFFSPRING, ADOPTED, etc.
#' @param background.Environment.Raised DATAFRAME CONTAINING THE DIFFERENT POSSIBLE FAMILY ENVIRONMENTS FOR EACH SPECIES, e.g., BASTARD, STILL TOGETHER, etc.
#' @param background.Clanhead.Relation DATAFRAME CONTAINING THE DIFFERENT POSSIBLE RELATIONSHIPS BETWEEN THE CHARACTER AND THE CLANHEAD
#' @param background.Clan.Status DATAFRAME CONTAINING THE DIFFERENT POSSIBLE STATUSES THE CHARACTER CAN HAVE IN THE CLAN
#' @param background.Social_Class DATAFRAME CONTAINING THE DIFFERENT POSSIBLE SOCIAL CLASSES OF THE FAMILY A CHARACTER IS RAISED IN
#' @param background.Occupation DATAFRAME CONTAINING THE DIFFERENT POSSIBLE OCCUPATIONS OF THE FAMILY A CHARACTER IS RAISED IN
#' @param contact.Info = -1 DATAFRAME CONTAINING THE DIFFERENT POSSIBLE OCCUPATIONS OF THE FAMILY A CHARACTER IS RAISED IN
#' @param char.Age = -1 CHARACTER'S CURRENT AGE
#' @param char.Species STRING CONTAINING CHARACTER'S BIRTH SPECIES
#' @param char.Species.Raised STRING CONTAINING SPECIES WITHIN WHOSE SOCIETY THE CHARACTER WAS RAISED
#' @param char.Sex STRING CONTAINING CHARACTER'S SEX
#' @param char.Height = -1 NUMERIC VARIABLE CONTAINING CHARACTER'S HEIGHT
#' @param char.Weight = -1 NUMERIC VARIABLE CONTAINING CHARACTER'S WEIGHT
#' @param char.Society STRING VARIABLE CONTAINING CHARACTER'S SOCIETY
#' @param char.Class STRING VARIABLE CONTAINING CHARACTER'S CLASS
#' @param skills.Increase.Record.DF DATAFRAME OF SKILLS YOU CAN INCREASE WHICH WILL GENERATE ONES USED TO RECORD ANY INCREASES
#' @param background.Skills.Increase.Record.DF DATAFRAME OF SKILLS YOU CAN INCREASE WHICH WILL RECORD ANY INCREASES FOR A CHARACTER'S BACKGROUND/LEVEL 0 STUFF
#' @param level_0_Skills.Increase.Record.List LIST CONTAINING ALL THE level_0 INFORMATION IF THAT HAS BEEN PREGENERATED
#' @concept height weight background options society family
#' @export
#' @examples
#' generate_Background()

generate_Background <-
function( background_Options.Benefits = -1, background.No_of_Children=-1, background.Sibling_Rank=-1, background.Family_Status=-1, background.Environment.Raised=-1,
                                    background.Clanhead.Relation=-1, background.Clan.Status=-1, background.Social_Class=-1, background.Occupation=-1, contact.Info = -1,
                                    char.Age = -1, char.Species = -1, char.Species.Raised = -1, char.Sex = -1, char.Height = -1, char.Weight = -1, char.Society = -1, char.Class = -1, 
                                    skills.Increase.Record.DF = -1, background.Skills.Increase.Record.DF = -1, level_0_Skills.Increase.Record.List = -1 ){ 

if(unlist(background_Options.Benefits)[1] == -1 & unlist(background.No_of_Children)[1] == -1 & unlist(background.Sibling_Rank)[1] == -1 &
	unlist(background.Family_Status)[1] == -1 & unlist(background.Environment.Raised)[1] == -1 & unlist(background.Clanhead.Relation)[1] == -1 &
	unlist(background.Clan.Status)[1] == -1 & unlist(background.Social_Class)[1] == -1 & unlist(background.Occupation)[1] == -1 & unlist(contact.Info)[1] == -1 &
	unlist(char.Age)[1] == -1 & unlist(char.Species)[1] == -1 & unlist(char.Species.Raised)[1] == -1 & unlist(char.Sex)[1] == -1 &
	unlist(char.Height)[1] == -1 & unlist(char.Weight)[1] == -1 &
	unlist(char.Society)[1] == -1 & unlist(char.Class)[1] == -1 &
	unlist(skills.Increase.Record.DF)[1] == -1  & unlist(background.Skills.Increase.Record.DF)[1] == -1 & 
	unlist(level_0_Skills.Increase.Record.List)[1] == -1 ) return(-1) 
#returns LIST CONTAINING ALL BACKGROUND MATERIAL - list(background.No_of_Children, background.Sibling_Rank, background.Environment.Raised, background.Clanhead.Relation, 
#                                           background.Clan.Status, background.Clan.Status.Description, background.Social_Class, background.Occupation, 
#                                           char.Age, contact.Info, background.Family_Status), char.Age_Die, char.Height, char.Weight, char.Move_Mod,
#                                           level_0_Skills.Increase.Record.List, skills.Increase.Record.DF.Level_1
    
    occupation.List <- list(occupation_Guilded,occupation_Noble,occupation_Serf,occupation_Slave,occupation_Unguilded,char_Skills_Background)
    
#    occupation_Guilded <- occupation.List[[1]]
 #   occupation_Noble <-occupation.List[[2]]
  #  occupation_Serf <- occupation.List[[3]]
   # occupation_Slave <-occupation.List[[4]] 
    #occupation_Unguilded <- occupation.List[[5]]
    #char_Skills_Background <- occupation.List[[6]]
   
   if( "Choose Background" %in% unlist(background_Options.Benefits) ){#DID THEY DECIDE TO CHOOSE THEIR OWN BACKGROUND?
#During "Background" for rolling social class, 
#parents etc, you may any or all results rather
#than rolling them.
maxRoll = max(char_Sibling_Rank[,"Roll"]) + char_No_of_Siblings[which(gsub(" ","_",char_No_of_Siblings[,"Race"]) == char.Species),"Roll_Mod"] 
maxRank = char_Sibling_Rank[min( which ( char_Sibling_Rank[,"Roll"] >= maxRoll ) ),"Rank"]
maxDieRoll = char_No_of_Siblings[which(gsub(" ","_",char_No_of_Siblings[,"Race"]) == char.Species),"Siblings_Dice"] 
maxChildren = maxRank + maxDieRoll

if( unlist(background.No_of_Children)[1] == -1) {
background.No_of_Children = choose_Generic("How many children did your parents have?",1:maxChildren)
}

if( unlist( background.Sibling_Rank ) == -1){
background.Sibling_Rank = choose_Generic("Which child are you?",1:background.No_of_Children)
}

        if( unlist( background.Family_Status ) == -1){
background.Family_Status = choose_Generic("In what environment did you grow up?",char_Family_Status[,"Status"])
}

        if( unlist( background.Environment.Raised ) == -1){
            if( background.Family_Status == "Offspring" ){
                background.Environment.Raised = choose_Generic("What is the status of your parents?",char_Parents_Status[,"Status"])
            } else if( background.Family_Status == "Fostered" ){
                background.Environment.Raised[1] = choose_Generic("What is the status of your biological parents?",char_Parents_Status[,"Status"])
                background.Environment.Raised[2] = choose_Generic("What is the status of your foster parents?",char_Parents_Status[,"Status"])
            } else if( background.Family_Status == "Adopted" ){
                background.Environment.Raised = choose_Generic("What is the status of your adoptive parents?",char_Parents_Status[,"Status"])
            } else if( background.Family_Status == "Orphan" ){
                background.Environment.Raised = choose_Generic("How were you raised?",char_Orphan_Status[,"Status"])
            }
        }
        
if( unlist(background.Clanhead.Relation)[1] == -1){
background.Clanhead.Relation = choose_Generic("What is your relationship to the Clanhead/head of the family/etc?",char_Clanhead_Relationship[,"Status"])
}

if( unlist(background.Clan.Status)[1] == -1){ 
background.Clan.Status = choose_Generic("What is your position within the clan/family?",char_Clan_Status[,"Status"])
}
        background.Clan.Status.Description = char_Clan_Status[ which( char_Clan_Status[,"Status"] == background.Clan.Status ), "Description"]


if( unlist(background.Social_Class)[1] == -1){
background.Social_Class = choose_Generic("Social Class",char_Social_Class[,1])
}

if( unlist(background.Occupation)[1] == -1){
            if( background.Social_Class == "Slave" ){ occupations = occupation_Slave[,"Occupation"]
            } else if( background.Social_Class == "Serf" ){ occupations = occupation_Serf[,"Occupation"]
            } else if( background.Social_Class == "Unguilded" ){ occupations = occupation_Unguilded[,"Occupation"]
            } else if( background.Social_Class == "Guilded" ){ occupations = occupation_Guilded[,"Occupation"]
            } else if( background.Social_Class == "Noble" ){ occupations = occupation_Noble[,"Occupation"]
            }
background.Occupation = choose_Generic("Occupation",occupations)
}

char.Age_Die <- as.numeric( strsplit(species_Description_DF[which(species_Description_DF[,1] == char.Species),"Age_Die"],"d")[[1]][2] )
char.Class_Age <- as.numeric( class_Costs_And_Info[which(class_Costs_And_Info[,1] == "Start_Age_Dice"),char.Class] )
minAge = species_Description_DF[species_Description_DF[,"Race"] == char.Species,"Adolescent"] + 1*char.Class_Age
maxAge = species_Description_DF[species_Description_DF[,"Race"] == char.Species,"Adolescent"] + char.Age_Die*char.Class_Age
if( unlist(char.Age)[1] == -1 ){
(char.Age <- choose_Generic(paste0("Pick an age between ", minAge," and ",maxAge),minAge:maxAge))
}

contact.Number = max(1, floor(char.Age/char_Contacts_Number[gsub(" ","_",char_Contacts_Number[,"Race"]) == char.Species,"Divisor"]))

if( unlist(contact.Info)[1] == -1 ){

            contact.Identity = NULL
contact.Relationship = NULL
contact.Loyalty = NULL
            
for( i.Contact in 1:contact.Number ){
contact.Identity[i.Contact] = choose_Generic(paste0("Who is contact ",i.Contact),char_Contacts_Identity[,"Status"])
contact.Relationship[i.Contact] =  choose_Generic(paste0("What is your relationship to contact ",i.Contact),char_Contacts_Relationship[,"Status"])

contact.Loyalty.Roll = unlist( strsplit( char_Contacts_Relationship[char_Contacts_Relationship[,"Status"] == contact.Relationship[i.Contact],"Loyalty"], "_1d") )

minLoyal = as.numeric( contact.Loyalty.Roll[1] )
maxLoyal = minLoyal + as.numeric( contact.Loyalty.Roll[2] )

                if( contact.Relationship[i.Contact] == "Enemy" ) { 
                    contact.Loyalty[i.Contact] = 0
                } else{
                    contact.Loyalty[i.Contact] = choose_Generic(paste0("How loyal is contact ",i.Contact),minLoyal:maxLoyal )
                }
}
contact.Info <- list( contact.Identity, contact.Relationship, contact.Loyalty )
}

hnW.row <- get_Height_And_Weight_For_Species( char.Species, char.Sex )
height.Base.Split =  as.numeric( unlist( strsplit( gsub( "\"", "", hnW.row["Base.Height"] ), "'"  ) ) )
height.Base.Inches = height.Base.Split[1]*12 + height.Base.Split[2]
        height.Dice = as.numeric( unlist( strsplit( gsub( "\\+", "", hnW.row["Height.Mod"] ), "d"  ) ) )
        height.Max.Inches =  height.Base.Inches + height.Dice[1]*height.Dice[2]

        weight.Base.Lb = as.numeric( hnW.row["Base.Weight..lb."] )
        weight.Dice = as.numeric( unlist( strsplit( gsub( "[\\*\\(\\)]", "", hnW.row["Weight.Mod"] ), "d"  ) ) )

if( "Great Strength" %in% unlist( background_Options.Benefits ) ){
                char.Height = height.Max.Inches
                char.Weight = weight.Base.Lb + (char.Height - height.Base.Inches)*weight.Dice[1]*weight.Dice[2]
        } else{
        
            if( unlist(char.Height)[1] == -1 ){
            
                height.Options = height.Base.Inches:height.Max.Inches
                height.Options.Char = paste(floor(height.Options/12),"\'",height.Options%%12,"\"",sep="")
                char.Height = height.Options[ height.Options.Char == choose_Generic( "height of your character", height.Options.Char ) ]
            
            }
           
            if( unlist(char.Weight[1]) == -1 ){
                weight.Options = weight.Base.Lb:(weight.Base.Lb + (char.Height - height.Base.Inches)*weight.Dice[1]*weight.Dice[2])
                char.Weight = as.numeric( choose_Generic( "weight of your character", weight.Options ) )
            }
        }
             
char.Move_Mod = calculate_Height_And_Weight( char.Species, char.Sex, char.Height, char.Weight )[3]

} else{


if( unlist(background.Sibling_Rank)[1] == -1 ){
rank.Roll = max(1,sample(100,1) + char_No_of_Siblings[which(gsub(" ","_",char_No_of_Siblings[,"Race"]) == char.Species),"Roll_Mod"])
background.Sibling_Rank = char_Sibling_Rank[min( which ( char_Sibling_Rank[,"Roll"] >= rank.Roll ) ),"Rank"]
}
 
if( unlist(background.No_of_Children)[1] == -1 ){
sibling.Roll = sample( char_No_of_Siblings[which(gsub(" ","_",char_No_of_Siblings[,"Race"]) == char.Species),"Siblings_Dice"], 1)
background.No_of_Children = background.Sibling_Rank  + sibling.Roll 
}


if( unlist(background.Family_Status)[1] == -1 ){

family.Status.Roll = sample(100,1) 
background.Family_Status = char_Family_Status[min( which ( char_Family_Status[,"Roll"] >= family.Status.Roll ) ),"Status"]

}
        
        if( unlist(background.Environment.Raised)[1] == -1 ){
            if( background.Family_Status == "Offspring" ){
family.Environment.Roll = sample(100,1) 
background.Environment.Raised = char_Parents_Status[min( which ( char_Parents_Status[,"Roll"] >= family.Environment.Roll ) ),"Status"]
} else if( background.Family_Status == "Fostered" ){
family.Environment.Roll = sample(100,3,replace=TRUE) 
family.Environment.Roll[1] = min(100,family.Environment.Roll [1] + 30) 
background.Environment.Raised[1] = char_Parents_Status[min( which ( char_Parents_Status[,"Roll"] >= family.Environment.Roll[2] ) ),"Status"]
background.Environment.Raised[2] = char_Parents_Status[min( which ( char_Parents_Status[,"Roll"] >= family.Environment.Roll[3] ) ),"Status"]
} else if( background.Family_Status == "Adopted" ){
family.Environment.Roll = sample(100,1) 
background.Environment.Raised = char_Parents_Status[min( which ( char_Parents_Status[,"Roll"] >= family.Environment.Roll ) ),"Status"]
} else if( background.Family_Status == "Orphan" ){
family.Environment.Roll = sample(100,1) 
background.Environment.Raised = char_Orphan_Status[min( which ( char_Orphan_Status[,"Roll"] >= family.Environment.Roll ) ),"Status"]
} else if( background.Family_Status == "Bastard - Acknowledged" ){
background.Environment.Raised = "Your father acknowledges you."
} else if( background.Family_Status == "Bastard - Unacknowledged" ){
background.Environment.Raised = "Your father does not acknowledges you."
}
        }
        
clanhead.Status.Rolls = sample(100,2,replace=TRUE) 

if( unlist(background.Clanhead.Relation)[1] == -1 ){
background.Clanhead.Relation = char_Clanhead_Relationship[min( which ( char_Clanhead_Relationship[,"Roll"] >= clanhead.Status.Rolls[1] ) ),"Status"]
}
        
if( unlist(background.Clan.Status)[1] == -1 ){
background.Clan.Status = char_Clan_Status[min( which ( char_Clan_Status[,"Roll"] >= clanhead.Status.Rolls[2] ) ),"Status"]
}
        background.Clan.Status.Description = char_Clan_Status[ which( char_Clan_Status[,"Status"] == background.Clan.Status ), "Description"]

char.Occupation.Rolls = sample(100,2,replace=TRUE) 

if( unlist(background.Social_Class)[1] == -1 ){
background.Social_Class.Targets = as.numeric( gsub( "-", "0", char_Social_Class[,char.Society] ) )
background.Social_Class =  char_Social_Class[min( which ( background.Social_Class.Targets >= char.Occupation.Rolls[1] ) ),"Class" ]
}

if( unlist(background.Occupation)[1] == -1 ){
            if( background.Social_Class == "Slave" ){ occupations = occupation_Slave[,c(char.Society,"Occupation")]
            } else if( background.Social_Class == "Serf" ){ occupations = occupation_Serf[,c(char.Society,"Occupation")]
            } else if( background.Social_Class == "Unguilded" ){ occupations = occupation_Unguilded[,c(char.Society,"Occupation")]
            } else if( background.Social_Class == "Guilded" ){ occupations = occupation_Guilded[,c(char.Society,"Occupation")]
            } else if( background.Social_Class == "Noble" ){ occupations = occupation_Noble[,c(char.Society,"Occupation")]
            }
background.Occupation = occupations[min( which ( suppressWarnings( as.numeric( occupations[,1] ) ) >= char.Occupation.Rolls[2] ) ) ,2 ]
}

char.Age_Die <- as.numeric( strsplit(species_Description_DF[which(species_Description_DF[,1] == char.Species),"Age_Die"],"d")[[1]][2] )
char.Class_Age <- as.numeric( class_Costs_And_Info[which(class_Costs_And_Info[,1] == "Start_Age_Dice"),char.Class] )
minAge = species_Description_DF[species_Description_DF[,"Race"] == char.Species,"Adolescent"] + 1*char.Class_Age
maxAge = species_Description_DF[species_Description_DF[,"Race"] == char.Species,"Adolescent"] + char.Age_Die*char.Class_Age

if( unlist(char.Age)[1] == -1 ){
(char.Age <- sample(minAge:maxAge,1))
}

contact.Number = max(1, floor(char.Age/char_Contacts_Number[gsub(" ","_",char_Contacts_Number[,"Race"]) == char.Species,"Divisor"]))

if( unlist(contact.Info)[1] == -1 ){
contact.Identity = NULL
contact.Relationship = NULL
contact.Loyalty = NULL

contact.d100.Rolls <- sample(100,(contact.Number)*2,replace=TRUE)

for( i.Contact in 1:contact.Number ){
contact.Identity[i.Contact] = char_Contacts_Identity[min( which ( char_Contacts_Identity[,"Roll"] >= contact.d100.Rolls[1+(2*(i.Contact-1))] ) ),"Status"]
contact.Relationship[i.Contact] = char_Contacts_Relationship[min( which ( char_Contacts_Relationship[,"Roll"] >= contact.d100.Rolls[2+(2*(i.Contact-1))] ) ),"Status"]

if( contact.Relationship[i.Contact] == "Enemy" ){ contact.Loyalty[i.Contact] = 0
} else{
contact.Loyalty.Roll = unlist( strsplit( char_Contacts_Relationship[char_Contacts_Relationship[,"Status"] == contact.Relationship[i.Contact],"Loyalty"], "_1d") )

minLoyal = as.numeric( contact.Loyalty.Roll[1] )
dieLoyal = as.numeric( contact.Loyalty.Roll[2] )

contact.Loyalty[i.Contact] = minLoyal + sample(dieLoyal,1)
}
}

contact.Info <- list( contact.Identity, contact.Relationship, contact.Loyalty )
}

        if( "Great Strength" %in% unlist( background_Options.Benefits ) ){
                
                hnW.row <- get_Height_And_Weight_For_Species( char.Species, char.Sex )
                height.Base.Split =  as.numeric( unlist( strsplit( gsub( "\"", "", hnW.row["Base.Height"] ), "'"  ) ) )
                height.Base.Inches = height.Base.Split[1]*12 + height.Base.Split[2]
                height.Dice = as.numeric( unlist( strsplit( gsub( "\\+", "", hnW.row["Height.Mod"] ), "d"  ) ) )
                height.Max.Inches =  height.Base.Inches + height.Dice[1]*height.Dice[2]
                
                weight.Base.Lb = as.numeric( hnW.row["Base.Weight..lb."] )
                weight.Dice = as.numeric( unlist( strsplit( gsub( "[\\*\\(\\)]", "", hnW.row["Weight.Mod"] ), "d"  ) ) )
                
                char.Height = height.Max.Inches
                char.Weight = weight.Base.Lb + (char.Height - height.Base.Inches)*weight.Dice[1]*weight.Dice[2]
                char.Move_Mod = calculate_Height_And_Weight( char.Species, char.Sex, char.Height, char.Weight )[3]
            
        } else{
            
            char_Height_And_Weight.Calcs = calculate_Height_And_Weight( char.Species, char.Sex, char.Height, char.Weight )
        
            char.Height = char_Height_And_Weight.Calcs[1]
            char.Weight = char_Height_And_Weight.Calcs[2]
            char.Move_Mod = char_Height_And_Weight.Calcs[3]
            
        }

        char.Move_Mod = calculate_Height_And_Weight( char.Species, char.Sex, char.Height, char.Weight )[3]

}
    
    #GETS THE BONUSES AND SKILLS ASSOCIATED WITH THE OCCUPATION
    background.Occupation.Tmp <- char_Species_Occupation(background.Social_Class, occupation.List, background.Occupation)
    occupation.Bonuses <- background.Occupation.Tmp[[1]]
    background.Occupation.Skills.Normal <- background.Occupation.Tmp[[2]]
    background.Occupation.Skills.ANY <- background.Occupation.Tmp[[3]]
    
    #GETS THE BONUSES AND SKILLS ASSOCIATED WITH THE SPECIES AND SOCIETY
    char.Species.Skills.And.Hobby_Points.List <- char_Species_Raised_Skills_And_Hobby_Points( char.Species.Raised, char_Skills_Background, char.Society, background.Social_Class, 
                                                                                                occupation.Bonuses[,"Ur."] )
    background.Social.Skills.Normal <- char.Species.Skills.And.Hobby_Points.List[[1]]
    background.Social.Skills.ANY <- char.Species.Skills.And.Hobby_Points.List[[2]]
    background.Social.Hobby_Points <- char.Species.Skills.And.Hobby_Points.List[[3]]
    


    skills.Increase.Record.DF.Level_1 <- skills.Increase.Record.DF
#    skills.Increase.Record.DF.Level_0 <- skills.Increase.Record.DF
    skills.Increase.Record.DF.Level_0 <- background.Skills.Increase.Record.DF
    if( unlist(level_0_Skills.Increase.Record.List)[1] == -1 ){ 
        level_0_Skills.Increase.Record.List <- level_0_Skills(skills.Increase.Record.DF.Level_0, char.Class, background.Social.Hobby_Points, background_Options.Benefits, 
                                                    background.Social.Skills.Normal, background.Social.Skills.ANY, background.Occupation.Skills.Normal, background.Occupation.Skills.ANY) 
}
    
    level_0_Skills.Increase.Record.List[[1]][,"TotalRanks"] = level_0_Skills.Increase.Record.List[[1]][,"NumTimesIncreased"] 
    skills.Increase.Record.DF.Level_1[,"TotalRanks"] = level_0_Skills.Increase.Record.List[[1]][,"NumTimesIncreased"] 
    
    return( list( list(background.No_of_Children, background.Sibling_Rank, background.Environment.Raised, background.Clanhead.Relation, 
                    background.Clan.Status, background.Clan.Status.Description, background.Social_Class, background.Occupation, 
                    char.Age, contact.Info, background.Family_Status), char.Age_Die, char.Height, char.Weight, char.Move_Mod,
                    level_0_Skills.Increase.Record.List, skills.Increase.Record.DF.Level_1 ) )
}

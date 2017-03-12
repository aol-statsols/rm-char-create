#' A CHARACTER SHEET FUNCTION
#'
#' FUNCTION TO GENERATE THE SECOND PAGE FOR THE CHARACTER SHEET.
#' @param skills.OUTPUT DATAFRAME CONTAINING ALL SKILL OUTPUTS GENERATED DURING LEVELING.
#' @param class_Costs_And_Info DATAFRAME CONTAINING THE COSTS AND STATS ASSOCIATED WITH SKILLS FOR ALL PROFESSIONS.
#' @param char_Profession_Bonus VECTOR WITH THE BONUSES TO SKILLS FROM PROFESSIONS.
#' @param char.Class STRING CONTAINING CHARACTER'S PROFESSION.
#' @param skills.Primary.Stats VECTOR OF THE PRIMARY SKILLS.
#' @param skills.Secondary.Stats VECTOR OF THE SECONDARY SKILLS.
#' @param bonus.Total TOTAL BONUS ASSOCIATED WITH EACH STAT.
#' @param char.Realm STRING CONTAINING CHARACTER'S REALM STAT
#' @param char.Species STRING CONTAINING CHARACTER'S SPECIES
#' @param species_Description_DF DATAFRAME CONTAINING BONUSES FOR STATS AND RRs FOR EACH SPECIES
#' @param img.Size STRING CONTAINING THE SIZE - "BIG" "MED" - OF THE IMAGE DESIRED.
#' @concept test skill
#' @export
#' @examples
#' second_Page_Generator()

second_Page_Generator <-
function(skills.OUTPUT = -1, class_Costs_And_Info = -1, char_Profession_Bonus = -1, char.Class = -1, skills.Primary.Stats = -1, skills.Secondary.Stats = -1, bonus.Total = -1, 
                                    char.Realm = -1, char.Species = -1, species_Description_DF = -1, img.Size = -1){
        
if(unlist(skills.OUTPUT)[1] == -1 & unlist(class_Costs_And_Info)[1] == -1 & unlist(char_Profession_Bonus)[1] == -1 & unlist(char.Class)[1] == -1 &
	unlist(skills.Primary.Stats)[1] == -1 & unlist(skills.Secondary.Stats)[1] == -1 &
    unlist(bonus.Total)[1] == -1 & unlist(char.Realm)[1] == -1 & unlist(char.Species)[1] == -1 &
	unlist(species_Description_DF)[1] == -1 & unlist(img.Size)[1] == -1) return(-1)                             
#returns:   NULL.

    stats = stat_Name_DF[,1]
    
species.Bonuses <- species_Description_DF[species_Description_DF[,1] == gsub( " ", "_", char.Species),2:11]
    y.SkillName.Primary <- NULL
    y.SkillName.Secondary <- NULL
    y.SkillCost.Primary <- NULL
    y.SkillCost.Secondary <- NULL
    y.SkillStatRelated.Primary <- NULL
    y.SkillStatRelated.Secondary <- NULL
    y.SkillRank.Primary <- NULL
    y.SkillRank.Secondary <- NULL
    y.SkillRankBonus.Primary <- NULL
    y.SkillRankBonus.Secondary <- NULL
    y.SkillStatBonus.Primary <- NULL
    y.SkillStatBonus.Secondary <- NULL
    y.SkillProf.Primary <- NULL
    y.SkillProf.Secondary <- NULL
    y.SkillProfBonus.Primary.Increased <- NULL
    y.SkillProfBonus.Secondary.Increased <- NULL
    y.SkillProfBonus.Primary.Not_Increased <- NULL
    y.SkillProfBonus.Secondary.Not_Increased <- NULL
    y.SkillTotal.Primary <- NULL
    y.SkillTotal.Secondary <- NULL
    
    xMisc <- NULL; yMisc <- NULL; nMisc <- NULL; sMisc <- NULL
    
    skills.Primary = skills.Primary.Stats[,1] 
    skills.Secondary = skills.Secondary.Stats[,1]
    
    skills.Increased <- skills.OUTPUT[as.numeric( skills.OUTPUT[,"NumTimesIncreased"] ) > 0,]
    skills.Increased.Primary <- skills.Increased[skills.Increased[,"Skills.Name"] %in% skills.Primary,]
    skills.Increased.Secondary <- skills.Increased[skills.Increased[,"Skills.Name"] %in% skills.Secondary,]
#   skills.StatRelated.Primary.Vec <- class_Costs_And_Info[class_Costs_And_Info[,"Skill"] %in% skills.Increased.Primary[,"Skills.Name"],"Stat"]
#   skills.StatRelated.Secondary.Vec <- class_Costs_And_Info[class_Costs_And_Info[,"Skill"] %in% skills.Increased.Secondary[,"Skills.Name"],"Stat"]
    skills.StatRelated.Primary.Vec <- skills.Primary.Stats[skills.Primary.Stats[,1]  %in% skills.Increased.Primary[,"Skills.Name"],2]
    skills.StatRelated.Secondary.Vec <- skills.Secondary.Stats[skills.Secondary.Stats[,1] %in% skills.Increased.Secondary[,"Skills.Name"],2]
    
    skills.StatBonus.Primary.List <- strsplit(skills.StatRelated.Primary.Vec,'/')
    skills.StatBonus.Primary <- NULL
    skills.StatBonus.Secondary.List <- strsplit(skills.StatRelated.Secondary.Vec,'/')
    skills.StatBonus.Secondary <- NULL
    
    skills.Increased.Primary.Costs <- apply(skills.Increased.Primary[,2:4],1,function(x){ gsub("[[:space:]]","",paste(x,collapse="/") ) })
    skills.Increased.Primary.Costs[skills.Increased.Primary[,6] == "55"] <- paste0(substr( skills.Increased.Primary.Costs[skills.Increased.Primary[,6] == "55"],1,2 ), "*")
    skills.Increased.Primary.Costs[grepl("0",skills.Increased.Primary.Costs)] <- gsub( "/0", "", skills.Increased.Primary.Costs[grepl("0",skills.Increased.Primary.Costs)] )
    
    skills.Increased.Secondary.Costs <- apply(skills.Increased.Secondary[,2:4],1,function(x){ gsub("[[:space:]]","",paste(x,collapse="/") ) })
    skills.Increased.Secondary.Costs[skills.Increased.Secondary[,6] == "55"] <- paste0(substr( skills.Increased.Secondary.Costs[skills.Increased.Secondary[,6] == "55"],1,2 ), "*")
    skills.Increased.Secondary.Costs[grepl("0",skills.Increased.Secondary.Costs)] <- gsub( "/0", "", skills.Increased.Secondary.Costs[grepl("0",skills.Increased.Secondary.Costs)] )
    
    for( i in 1:length(skills.StatBonus.Primary.List) ){
        if( length( skills.StatBonus.Primary.List[[i]] ) == 3 ){
            stats.Bonuses <- 1:3
            for( j in 1:3 ) stats.Bonuses[j] = bonus.Total[toupper(stats) %in% skills.StatBonus.Primary.List[[i]][j]]
            skills.StatBonus.Primary[i] <- character_creator_trunc( mean(stats.Bonuses), prec = 1 )
        } else{
            skills.StatBonus.Primary[i] <- ifelse( skills.StatBonus.Primary.List[[i]][1] == "var", mean(bonus.Total[stats %in% char.Realm]), 
                                            ifelse( skills.StatBonus.Primary.List[[i]][1] == "-", "0", mean(bonus.Total[toupper(stats) %in% skills.StatBonus.Primary.List[[i]]])
                                            ) )
        }
        if( skills.StatBonus.Primary.List[[i]][1] == "var" ){
            skills.StatRelated.Primary.Vec[i] = toupper(paste(char.Realm,collapse="/"))
        }
    }
    for( i in 1:length(skills.StatBonus.Secondary.List) ){
        if( length( skills.StatBonus.Secondary.List[[i]] ) == 3 ){
            stats.Bonuses <- 1:3
            for( j in 1:3 ) stats.Bonuses[j] = bonus.Total[toupper(stats) %in% skills.StatBonus.Secondary.List[[i]][j]]
            skills.StatBonus.Secondary[i] <- character_creator_trunc( mean(stats.Bonuses), prec = 1 )
        } else{
            skills.StatBonus.Secondary[i] <- ifelse( any(skills.StatBonus.Secondary.List[[i]] == "var"), mean(bonus.Total[stats %in% char.Realm]), 
                                            ifelse( any(skills.StatBonus.Secondary.List[[i]] == "-"), "0", mean(bonus.Total[toupper(stats) %in% skills.StatBonus.Secondary.List[[i]]])
                                            ) )
        }
        if( skills.StatBonus.Secondary.List[[i]][1] == "var" ){
            skills.StatRelated.Secondary.Vec[i] = toupper(paste(char.Realm,collapse="/"))
        }
    }
    skills.ProfBonus.SkillNames <- unlist(strsplit( char_Profession_Bonus[char_Profession_Bonus[,"Profession"]==char.Class,"Profession.Bonuses"], ";" ) )
    skills.ProfBonus.Bonuses <- unlist(strsplit( char_Profession_Bonus[char_Profession_Bonus[,"Profession"]==char.Class,"Bonuses"], ";" ) )
    skills.ProfBonus.SkillNames.Primary.Increased <- 
        skills.OUTPUT[as.numeric( skills.OUTPUT[,"NumTimesIncreased"] )> 0 ,"Skills.Name"][ 
            ( gsub( "(Weapons)(:.*)","\\1", gsub( "(Spells)(:.*)","\\1", skills.OUTPUT[as.numeric( skills.OUTPUT[,"NumTimesIncreased"] )> 0 ,"Skills.Name" ] ) ) %in% 
            gsub( "(Weapons)(:.*)","\\1", gsub( "(Spells)(:.*)","\\1", skills.ProfBonus.SkillNames ) ) ) &
            ( gsub( "(Weapons)(:.*)","\\1", gsub( "(Spells)(:.*)","\\1", skills.OUTPUT[as.numeric( skills.OUTPUT[,"NumTimesIncreased"] )> 0 ,"Skills.Name" ] ) ) %in% 
            gsub( "(Weapons)(:.*)","\\1", gsub( "(Spells)(:.*)","\\1", skills.Primary ) ) )]
    skills.ProfBonus.SkillNames.Secondary.Increased <- 
        skills.OUTPUT[as.numeric( skills.OUTPUT[,"NumTimesIncreased"] )> 0 ,"Skills.Name"][ 
            ( gsub( "(Weapons)(:.*)","\\1", gsub( "(Spells)(:.*)","\\1", skills.OUTPUT[as.numeric( skills.OUTPUT[,"NumTimesIncreased"] )> 0 ,"Skills.Name" ] ) ) %in% 
            gsub( "(Weapons)(:.*)","\\1", gsub( "(Spells)(:.*)","\\1", skills.ProfBonus.SkillNames ) ) ) &
            ( gsub( "(Weapons)(:.*)","\\1", gsub( "(Spells)(:.*)","\\1", skills.OUTPUT[as.numeric( skills.OUTPUT[,"NumTimesIncreased"] )> 0 ,"Skills.Name" ] ) ) %in% 
            gsub( "(Weapons)(:.*)","\\1", gsub( "(Spells)(:.*)","\\1", skills.Secondary ) ) )]
    skills.ProfBonus.SkillNames.Primary.Not_Increased <- 
        skills.OUTPUT[as.numeric( skills.OUTPUT[,"NumTimesIncreased"] ) == 0 ,"Skills.Name"][ 
            ( gsub( "(Weapons)(:.*)","\\1", gsub( "(Spells)(:.*)","\\1", skills.OUTPUT[as.numeric( skills.OUTPUT[,"NumTimesIncreased"] ) == 0 ,"Skills.Name" ] ) ) %in% 
            gsub( "(Weapons)(:.*)","\\1", gsub( "(Spells)(:.*)","\\1", skills.ProfBonus.SkillNames ) ) ) &
            ( gsub( "(Weapons)(:.*)","\\1", gsub( "(Spells)(:.*)","\\1", skills.OUTPUT[as.numeric( skills.OUTPUT[,"NumTimesIncreased"] ) == 0 ,"Skills.Name" ] ) ) %in% 
            gsub( "(Weapons)(:.*)","\\1", gsub( "(Spells)(:.*)","\\1", skills.Primary ) ) )]
    skills.ProfBonus.SkillNames.Secondary.Not_Increased <- 
        skills.OUTPUT[as.numeric( skills.OUTPUT[,"NumTimesIncreased"] ) == 0 ,"Skills.Name"][ 
            ( gsub( "(Weapons)(:.*)","\\1", gsub( "(Spells)(:.*)","\\1", skills.OUTPUT[as.numeric( skills.OUTPUT[,"NumTimesIncreased"] ) == 0 ,"Skills.Name" ] ) ) %in% 
            gsub( "(Weapons)(:.*)","\\1", gsub( "(Spells)(:.*)","\\1", skills.ProfBonus.SkillNames ) ) ) &
            ( gsub( "(Weapons)(:.*)","\\1", gsub( "(Spells)(:.*)","\\1", skills.OUTPUT[as.numeric( skills.OUTPUT[,"NumTimesIncreased"] ) == 0 ,"Skills.Name" ] ) ) %in% 
            gsub( "(Weapons)(:.*)","\\1", gsub( "(Spells)(:.*)","\\1", skills.Secondary ) ) )]
            
    skills.ProfBonus.Bonus.Primary.Increased <- NULL
    skills.ProfBonus.Bonus.Secondary.Increased <- NULL
    skills.ProfBonus.Bonus.Primary.Not_Increased <- NULL
    skills.ProfBonus.Bonus.Secondary.Not_Increased <- NULL        
    if( length(skills.ProfBonus.SkillNames.Primary.Increased) ){
        for(i in 1:length(skills.ProfBonus.SkillNames.Primary.Increased)){
            skills.ProfBonus.Bonus.Primary.Increased[i] = skills.ProfBonus.Bonuses[ which( gsub( "(Weapons)(:.*)","\\1", gsub( "(Spells)(:.*)","\\1", skills.ProfBonus.SkillNames ) ) == 
                                                                                        gsub( "(Weapons)(:.*)","\\1", gsub( "(Spells)(:.*)","\\1", skills.ProfBonus.SkillNames.Primary.Increased[i]) ) ) ]
        }        
    }
    if( length(skills.ProfBonus.SkillNames.Secondary.Increased) ){
        for(i in 1:length(skills.ProfBonus.SkillNames.Secondary.Increased)){
            skills.ProfBonus.Bonus.Secondary.Increased[i] = skills.ProfBonus.Bonuses[ which( gsub( "(Weapons)(:.*)","\\1", gsub( "(Spells)(:.*)","\\1", skills.ProfBonus.SkillNames ) ) == 
                                                                                        gsub( "(Weapons)(:.*)","\\1", gsub( "(Spells)(:.*)","\\1", skills.ProfBonus.SkillNames.Secondary.Increased[i]) ) ) ]
        }        
    }
    if( length(skills.ProfBonus.SkillNames.Primary.Not_Increased) ){
        for(i in 1:length(skills.ProfBonus.SkillNames.Primary.Not_Increased)){
            skills.ProfBonus.Bonus.Primary.Not_Increased[i] = skills.ProfBonus.Bonuses[ which( gsub( "(Weapons)(:.*)","\\1", gsub( "(Spells)(:.*)","\\1", skills.ProfBonus.SkillNames ) ) == 
                                                                                        gsub( "(Weapons)(:.*)","\\1", gsub( "(Spells)(:.*)","\\1", skills.ProfBonus.SkillNames.Primary.Not_Increased[i]) ) ) ]
        }        
    }
    if( length(skills.ProfBonus.SkillNames.Secondary.Not_Increased) ){
        for(i in 1:length(skills.ProfBonus.SkillNames.Secondary.Not_Increased)){
            skills.ProfBonus.Bonus.Secondary.Not_Increased[i] = skills.ProfBonus.Bonuses[ which( gsub( "(Weapons)(:.*)","\\1", gsub( "(Spells)(:.*)","\\1", skills.ProfBonus.SkillNames ) ) == 
                                                                                        gsub( "(Weapons)(:.*)","\\1", gsub( "(Spells)(:.*)","\\1", skills.ProfBonus.SkillNames.Secondary.Not_Increased[i]) ) ) ]
        }        
    }
    skills.ProfBonus.Bonus.Primary.Increased 
    skills.ProfBonus.Bonus.Secondary.Increased
    skills.ProfBonus.Bonus.Primary.Not_Increased 
    skills.ProfBonus.Bonus.Secondary.Not_Increased 
     
    skills.Increased.Primary <- apply(skills.Increased.Primary,2,as.character)
    skills.Increased.Secondary <- apply(skills.Increased.Secondary,2,as.character)
    
    x.SkillName.Primary = rep(0.144,nrow(skills.Increased.Primary))
    for(i in 1:nrow(skills.Increased.Primary)) y.SkillName.Primary[i] = 0.9388-(i-1)*0.01925
    n.SkillName.Primary = gsub( "One Handed", "OH", gsub( ".*(Weapons:)\\s.*:\\s(.*).*","\\1 \\2", gsub("Development","Dev", gsub( " Lists","",skills.Increased.Primary[,"Skills.Name"] ) ) ) )
    cex.SkillName.Primary = rep(ifelse( img.Size == "MED", 4, 12.25 ),nrow(skills.Increased.Primary))
    x.SkillName.Secondary = rep(0.144,nrow(skills.Increased.Secondary))
    for(j in 1:nrow(skills.Increased.Secondary)) y.SkillName.Secondary[j] = 0.9388-(i+j+5)*0.01925
    n.SkillName.Secondary =  gsub("Athletics","Ath", 
                                gsub("Knowledge: General","Know: Gen",    
                                gsub("Knowledge: Magical:","Know: Mag:", 
                                gsub( "Linguistics: Own Language","Ling: Own",
                                    skills.Increased.Secondary[,"Skills.Name"] ) ) ) )
    cex.SkillName.Secondary = rep(ifelse( img.Size == "MED", 4, 12.25 ),nrow(skills.Increased.Secondary))
    
    x.SkillCost.Primary = rep(0.2425,nrow(skills.Increased.Primary))
    for(i in 1:nrow(skills.Increased.Primary)) y.SkillCost.Primary[i] = 0.9388-(i-1)*0.01925
    n.SkillCost.Primary = skills.Increased.Primary.Costs
    cex.SkillCost.Primary = rep(ifelse( img.Size == "MED", 4, 12.25 ),nrow(skills.Increased.Primary))
    cex.SkillCost.Primary[ grepl( "[0-9]/[0-9]/[0-9]", skills.Increased.Primary.Costs ) ] = ifelse( img.Size == "MED", 3, 5.333 )
    
    x.SkillCost.Secondary = rep(0.2425,nrow(skills.Increased.Secondary))
    for(j in 1:nrow(skills.Increased.Secondary)) y.SkillCost.Secondary[j] = y.SkillName.Secondary[j]
    n.SkillCost.Secondary = skills.Increased.Secondary.Costs
    cex.SkillCost.Secondary = rep(ifelse( img.Size == "MED", 4, 12.25 ),nrow(skills.Increased.Secondary))
    cex.SkillCost.Secondary[ grepl( "[0-9]/[0-9]/[0-9]", skills.Increased.Secondary.Costs ) ] = ifelse( img.Size == "MED", 3, 5.333 )
    
    
    numberOfSkills <-  length( y.SkillName.Primary ) + length( y.SkillName.Secondary )
    if( numberOfSkills > 39 & numberOfSkills < 44){
        y.SkillName.Secondary = y.SkillName.Secondary
    } else if( numberOfSkills > 44 ){
        y.SkillName.Secondary = y.SkillName.Secondary + min(6,numberOfSkills-44)*0.01925
        if( numberOfSkills > 50 ) y.SkillName.Secondary[y.SkillName.Secondary > 50] = y.SkillName.Secondary[y.SkillName.Secondary > 50] + 1*0.01925
    }
    
    
    x.SkillStatRelated.Primary = rep(0.282,nrow(skills.Increased.Primary))
    for(i in 1:nrow(skills.Increased.Primary)) y.SkillStatRelated.Primary[i] = 0.9388-(i-1)*0.01925
    n.SkillStatRelated.Primary = skills.StatRelated.Primary.Vec 
    
    cex.SkillStatRelated.Primary <- 1:length(n.SkillStatRelated.Primary)
    for( i in 1:length(n.SkillStatRelated.Primary) ){
        if( length( skills.StatBonus.Primary.List[[i]] ) == 3 ){
            cex.SkillStatRelated.Primary[i] = ifelse( img.Size == "MED", 2, 5.333 )
        } else{ 
            cex.SkillStatRelated.Primary[i] = ifelse( img.Size == "MED", 3, 8 )
        }
    }
    
    x.SkillStatRelated.Secondary = rep(0.282,nrow(skills.Increased.Secondary))
    for(j in 1:nrow(skills.Increased.Secondary)) y.SkillStatRelated.Secondary[j] = y.SkillName.Secondary[j]
    n.SkillStatRelated.Secondary = skills.StatRelated.Secondary.Vec 
    
    cex.SkillStatRelated.Secondary <- 1:length(n.SkillStatRelated.Secondary)
    for( i in 1:length(n.SkillStatRelated.Secondary) ){
        if( length( skills.StatBonus.Secondary.List[[i]] ) == 3 ){
            cex.SkillStatRelated.Secondary[i] = ifelse( img.Size == "MED", 2, 5.333 )
        } else{ 
            cex.SkillStatRelated.Secondary[i] = ifelse( img.Size == "MED", 3, 8 )
        }
    }
    
    x.SkillRank.Primary = rep(0.5,nrow(skills.Increased.Primary))
    for(i in 1:nrow(skills.Increased.Primary)) y.SkillRank.Primary[i] = 0.9388-(i-1)*0.01925
    n.SkillRank.Primary = skills.Increased.Primary[,"NumTimesIncreased"]
    cex.SkillRank.Primary = rep(ifelse( img.Size == "MED", 4, 12.25 ),nrow(skills.Increased.Primary))
    x.SkillRank.Secondary = rep(0.5,nrow(skills.Increased.Secondary))
    for(j in 1:nrow(skills.Increased.Secondary)) y.SkillRank.Secondary[j] = y.SkillName.Secondary[j]
    n.SkillRank.Secondary = skills.Increased.Secondary[,"NumTimesIncreased"]
    cex.SkillRank.Secondary = rep(ifelse( img.Size == "MED", 4, 12.25 ),nrow(skills.Increased.Secondary))
    x.SkillRankBonus.Primary = rep(0.7185,nrow(skills.Increased.Primary))
    for(i in 1:nrow(skills.Increased.Primary)) y.SkillRankBonus.Primary[i] = 0.9388-(i-1)*0.01925
    n.SkillRankBonus.Primary = skills.Increased.Primary[,"Rank_Bonus"]
    cex.SkillRankBonus.Primary = rep(ifelse( img.Size == "MED", 4, 12.25 ),nrow(skills.Increased.Primary))
    x.SkillRankBonus.Secondary = rep(0.7185,nrow(skills.Increased.Secondary))
    for(j in 1:nrow(skills.Increased.Secondary)) y.SkillRankBonus.Secondary[j] = y.SkillName.Secondary[j]
    n.SkillRankBonus.Secondary = skills.Increased.Secondary[,"Rank_Bonus"]
    cex.SkillRankBonus.Secondary = rep(ifelse( img.Size == "MED", 4, 12.25 ),nrow(skills.Increased.Secondary))
    x.SkillStatBonus.Primary = rep(0.7585,nrow(skills.Increased.Primary))
    for(i in 1:nrow(skills.Increased.Primary)) y.SkillStatBonus.Primary[i] = 0.9388-(i-1)*0.01925
    n.SkillStatBonus.Primary = skills.StatBonus.Primary
    cex.SkillStatBonus.Primary = rep(ifelse( img.Size == "MED", 4, 12.25 ),nrow(skills.Increased.Primary))
    x.SkillStatBonus.Secondary = rep(0.7585,nrow(skills.Increased.Secondary))
    for(j in 1:nrow(skills.Increased.Secondary)) y.SkillStatBonus.Secondary[j] = y.SkillName.Secondary[j]
    n.SkillStatBonus.Secondary = skills.StatBonus.Secondary
    cex.SkillStatBonus.Secondary = rep(ifelse( img.Size == "MED", 4, 12.25 ),nrow(skills.Increased.Secondary))
if( "Stealth" %in% n.SkillName.Secondary ){
if( grepl( "Elf", char.Species ) | grepl( "Halfling", char.Species ) ){
n.SkillStatBonus.Secondary[ n.SkillName.Secondary == "Stealth" ] = mean( c ( as.numeric(bonus.Total[toupper(stats) %in% "AG"]),
                                                                                    ( as.numeric( bonus.Total[toupper(stats) %in% "SD"] ) + 2*(-1)*as.numeric( species.Bonuses["Self_Discipline"] ) ) ) )
}
}
    x.SkillProfBonus.Primary.Increased = rep( 0.7985, length(skills.ProfBonus.Bonus.Primary.Increased) )
    for(i in 1:length(skills.ProfBonus.Bonus.Primary.Increased)){
        y.SkillProfBonus.Primary.Increased[i] = y.SkillName.Primary[which( n.SkillName.Primary == 
                                                                        gsub( "One Handed", "OH", 
                                                                        gsub( "(Weapons:)(.*:)(.*)","\\1\\3", 
                                                                        gsub( "Power Point Development", "Power Point Dev", 
                                                                        gsub( "Body Development", "Body Dev", gsub( " Lists", "", skills.ProfBonus.SkillNames.Primary.Increased[i] ) ) ) ) ) ) ]
    }
    n.SkillProfBonus.Primary.Increased = skills.ProfBonus.Bonus.Primary.Increased
    cex.SkillProfBonus.Primary.Increased = rep(ifelse( img.Size == "MED", 4, 12.25 ),length(x.SkillProfBonus.Primary.Increased))
    x.SkillProfBonus.Secondary.Increased = rep( 0.7985, length(skills.ProfBonus.Bonus.Secondary.Increased) )
    for(j in 1:length(skills.ProfBonus.Bonus.Secondary.Increased)) y.SkillProfBonus.Secondary.Increased[j] = y.SkillName.Secondary[which( n.SkillName.Secondary ==
                                                                                                            gsub( "Knowledge: General:", "Know: Gen:",
                                                                                                            gsub( "Linguistics", "Ling",
                                                                                                            gsub( "Knowledge: Magical:", "Know: Mag:", 
                                                                                                            gsub( "Athletics", "Ath", gsub( " Lists", "", skills.ProfBonus.SkillNames.Secondary.Increased[j] ) ) ) ) ) )]
    n.SkillProfBonus.Secondary.Increased = skills.ProfBonus.Bonus.Secondary.Increased
    cex.SkillProfBonus.Secondary.Increased = rep(ifelse( img.Size == "MED", 4, 12.25 ),length(x.SkillProfBonus.Secondary.Increased))
    
    
    xMisc <- 0.8772
    yMisc <- y.SkillName.Primary[n.SkillName.Primary == "Body Dev"]
    nMisc <- "10"
    cexMisc <- ifelse( img.Size == "MED", 4, 12.25 )
    
    x.SkillTotal.Primary = rep(0.9172,nrow(skills.Increased.Primary))
    for(i in 1:nrow(skills.Increased.Primary)) y.SkillTotal.Primary[i] = 0.9388-(i-1)*0.01925
    n.SkillTotal.Primary = as.numeric(skills.Increased.Primary[,"Rank_Bonus"]) + as.numeric(skills.StatBonus.Primary)
    n.SkillTotal.Primary[n.SkillName.Primary == "Body Dev"] = as.numeric(n.SkillTotal.Primary[n.SkillName.Primary == "Body Dev"]) + 10
    n.SkillTotal.Primary[which( n.SkillName.Primary %in% 
                            gsub( "One Handed", "OH", 
                            gsub( "(Weapons:)(.*:)(.*)","\\1\\3", 
                            gsub( "Power Point Development", "Power Point Dev", 
                            gsub( "Body Development", "Body Dev", gsub( " Lists", "", skills.ProfBonus.SkillNames.Primary.Increased ) ) ) ) ) ) ] =
                                n.SkillTotal.Primary[which( n.SkillName.Primary %in% 
                                                                        gsub( "One Handed", "OH", 
                                                                        gsub( "(Weapons:)(.*:)(.*)","\\1\\3", 
                                                                        gsub( "Power Point Development", "Power Point Dev", 
                                                                        gsub( "Body Development", "Body Dev", gsub( " Lists", "", skills.ProfBonus.SkillNames.Primary.Increased) ) ) ) ) ) ] +
                                as.numeric(skills.ProfBonus.Bonus.Primary.Increased)
    n.SkillTotal.Primary =  floor( as.numeric(n.SkillTotal.Primary) )                            
    cex.SkillTotal.Primary = rep(ifelse( img.Size == "MED", 4, 12.25 ),nrow(skills.Increased.Primary))
    x.SkillTotal.Secondary = rep(0.9172,nrow(skills.Increased.Secondary))
    for(j in 1:nrow(skills.Increased.Secondary)) y.SkillTotal.Secondary[j] = y.SkillName.Secondary[j]
    n.SkillTotal.Secondary = as.numeric(skills.Increased.Secondary[,"Rank_Bonus"]) + as.numeric(skills.StatBonus.Secondary)
    n.SkillTotal.Secondary[which( n.SkillName.Secondary %in% 
                            gsub( "Knowledge: General:", "Know: Gen:",
                                gsub( "Linguistics", "Ling",
                                gsub( "Artistic", "Art",
                                gsub( "Knowledge: Magical:", "Know: Mag:", 
                                gsub( "Athletics", "Ath", gsub( " Lists", "", skills.ProfBonus.SkillNames.Secondary.Increased ) ) ) ) ) ) ) ] =
                                n.SkillTotal.Secondary[which( n.SkillName.Secondary %in% 
                                                                        gsub( "Knowledge: General:", "Know: Gen:",
                                                                            gsub( "Linguistics", "Ling",
                                                                            gsub( "Artistic", "Art",
                                                                            gsub( "Knowledge: Magical:", "Know: Mag:", 
                                                                            gsub( "Athletics", "Ath", gsub( " Lists", "", skills.ProfBonus.SkillNames.Secondary.Increased ) ) ) ) ) ) ) ] +
                                as.numeric(skills.ProfBonus.Bonus.Secondary.Increased)
    n.SkillTotal.Secondary =  floor( as.numeric(n.SkillTotal.Secondary) )                         
    cex.SkillTotal.Secondary = rep(ifelse( img.Size == "MED", 4, 12.25 ),nrow(skills.Increased.Secondary))
    
    
    n.SkillName.Primary <- gsub ( "Channeling:", "C:", 
                                gsub ( "Essence:", "E:", 
                                gsub ( "Mentalism:", "M:", 
                                gsub( "Spells: (.).*\\( (.*)\\)", "Sp: \\1: \\2", 
                                gsub( "Adrenal Moves", "Adrenal",
                                gsub( "Grappling and Sweeps", "Grappling",
                                gsub( "Weapons: OH Concussion", "Wea: OH Concussion",
                                gsub( "Weapons: Two Handed", "W: Two Handed",
                                gsub( "Unarmed: Martial Arts", "Un: MA",n.SkillName.Primary 
                                )
                                )
                                )
                                )
                                )
                                )
                                )
                                )
                            )
    
n.SkillName.Secondary <- gsub( "Dungeoneering", "Dung", 
                                    gsub( "The Planes", "Planes",
                                    gsub( "Engineering", "Eng", 
                                    gsub( "Geography", "Geog", 
                                    gsub( "Combat Maneuvers", "CM", 
                                    gsub( "Two Weapon Combat", "TW Combat", 
                                    gsub( "Art: Active [0-9]", "Art: Ac",
                                    gsub( "Artistic", "Art",
                                    gsub( "Know: Gen [0-9]", "K: Gen", 
                                    gsub( "Knowledge: Magical [0-9]", "K: Mag", 
                                    gsub( "Profession [0-9]", "Prof", 
                                    gsub( "Linguistics [0-9]", "Ling", n.SkillName.Secondary 
                                    ) 
                                    ) 
                                    ) 
                                    )
                                    )
                                    )
                                    )
                                    )
                                    )
                                    )
                                    )
                                )

   x <- c(x.SkillName.Primary,x.SkillName.Secondary,
            x.SkillCost.Primary, x.SkillCost.Secondary,
            x.SkillStatRelated.Primary,x.SkillStatRelated.Secondary,
            x.SkillRank.Primary,x.SkillRank.Secondary,
            x.SkillRankBonus.Primary,x.SkillRankBonus.Secondary,
            x.SkillStatBonus.Primary,x.SkillStatBonus.Secondary,
            x.SkillProfBonus.Primary.Increased,x.SkillProfBonus.Secondary.Increased,
            xMisc,
            x.SkillTotal.Primary, x.SkillTotal.Secondary)
    y <- c(y.SkillName.Primary,y.SkillName.Secondary,
            y.SkillCost.Primary, y.SkillCost.Secondary,
            y.SkillStatRelated.Primary,y.SkillStatRelated.Secondary,
            y.SkillRank.Primary,y.SkillRank.Secondary,
            y.SkillRankBonus.Primary,y.SkillRankBonus.Secondary,
            y.SkillStatBonus.Primary,y.SkillStatBonus.Secondary,
            y.SkillProfBonus.Primary.Increased,y.SkillProfBonus.Secondary.Increased,
            yMisc,
            y.SkillTotal.Primary, y.SkillTotal.Secondary)
    stats.Out <- c(n.SkillName.Primary,n.SkillName.Secondary,
            n.SkillCost.Primary, n.SkillCost.Secondary,
            n.SkillStatRelated.Primary,n.SkillStatRelated.Secondary,
            gsub( "[[:space:]]", "", c( n.SkillRank.Primary,n.SkillRank.Secondary,
            n.SkillRankBonus.Primary,n.SkillRankBonus.Secondary,
            n.SkillStatBonus.Primary,n.SkillStatBonus.Secondary,
            n.SkillProfBonus.Primary.Increased,n.SkillProfBonus.Secondary.Increased,
            nMisc,
            n.SkillTotal.Primary, n.SkillTotal.Secondary ) ) )
    text.Cex <- c(cex.SkillName.Primary,cex.SkillName.Secondary,
            cex.SkillCost.Primary, cex.SkillCost.Secondary,
            cex.SkillStatRelated.Primary,cex.SkillStatRelated.Secondary,
            cex.SkillRank.Primary,cex.SkillRank.Secondary,
            cex.SkillRankBonus.Primary,cex.SkillRankBonus.Secondary,
            cex.SkillStatBonus.Primary,cex.SkillStatBonus.Secondary,
            cex.SkillProfBonus.Primary.Increased,cex.SkillProfBonus.Secondary.Increased,
            cexMisc,
            cex.SkillTotal.Primary, cex.SkillTotal.Secondary)
    length(x)
    length(y)
    length(stats.Out)
    length(text.Cex)
    stats.out <- gsub("[[:space:]]+$", "", stats.Out)
    stats.out <- gsub("^[[:space:]]+", "", stats.Out)
    text.Cex[nchar( gsub("[[:space:]]","",stats.Out), type = "width" )>= 18] = ifelse( img.Size == "MED", 3, 9.187 )
    text.Cex[nchar( gsub("[[:space:]]","",stats.Out), type = "width" )>= 24] = ifelse( img.Size == "MED", 2.75, 8.421417 )
    if( numberOfSkills > 39 ){
        y = y + 0.031375
    }
            
#    cs.p2<-readPNG( ifelse( img.Size == "MED",  ifelse( numberOfSkills <= 39, "cs2_MED.png", "cs2_MED_Longer.png"), ifelse( numberOfSkills <= 39, "cs2_BIG.png","cs2_BIG.png" ) ) )
#    cs.p2<-readPNG( ifelse( numberOfSkills <= 39, ifelse( img.Size == "NORM", "cs2_NORM.png", ifelse( img.Size == "MED", "cs2_MED.png", "cs2_BIG.png") ), 
#                        ifelse( img.Size == "NORM", "cs2_NORM_Longer.png", ifelse( img.Size == "MED", "cs2_MED_Longer.png","cs2_BIG.png" ) ) ) )
	cs.p2.Tmp<-cs.p2
	
    gc()
    #get size
    h<-dim(cs.p2.Tmp)[1]
    w<-dim(cs.p2.Tmp)[2]
    png("outP2.png", width=w, height=h)
    par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
    plot.new()
    plot.window(0:1, 0:1)
    #fill plot with image
    usr<-par("usr")    
    rasterImage(cs.p2.Tmp, usr[1], usr[3], usr[2], usr[4])
    gc()
    text(x,y, stats.Out, cex=text.Cex, col = "black")
    gc()
    #,col=rgb(.2,.2,.2,.7)
    #close image
    dev.off()
    #}
    usr <- NULL
    cs.p2.Tmp<- NULL
    gc()
    
}

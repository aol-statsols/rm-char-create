#' A CHARACTER SHEET FUNCTION
#'
#' FUNCTION TO GENERATE THE FIRST PAGE FOR THE CHARACTER SHEET.
#' @param species_Description_DF DATAFRAME CONTAINING BONUSES FOR STATS AND RRs FOR EACH SPECIES
#' @param char.Species STRING CONTAINING CHARACTER'S SPECIES
#' @param stats.End DATAFRAME CONTAINING CHARACTER'S STATS AFTER ALL LEVELING UP
#' @param char.Realm STRING CONTAINING CHARACTER'S REALM STAT
#' @param char.Class STRING CONTAINING CHARACTER'S PROFESSION
#' @param char.Level STRING CONTAINING CHARACTER'S LEVEL
#' @param char.Height NUMERIC CHARACTER'S HEIGHT
#' @param char.Weight NUMERIC CHARACTER'S WEIGHT
#' @param char.Move_Mod NUMERIC CHARACTER'S Move Mod
#' @param char.Name NUMERIC CHARACTER'S NAME
#' @param char.Player NUMERIC PLAYER'S NAME
#' @param resist.Realm STRING SPELLCASTING REALM CHARACTER IS RESISTANT TO
#' @param adept.Realm STRING SPELLCASTING REALM CHARACTER IS ADEPT WITH
#' @param background_Options.Benefits LIST CONTAINING ALL BACKGROUND OPTION BENEFITS A PLAYER HAS TAKEN
#' @param skills.OUTPUT DATAFRAME WITH ALL SKILLS AND RANK BONSUSES TO BE OUTPUT
#' @param img.Size STRING CONTAINING THE SIZE - "BIG" "MED" - OF THE IMAGE DESIRED.
#' @concept sheet output
#' @export
#' @examples
#' first_Page_Generator()

first_Page_Generator <-
function(species_Description_DF = -1, char.Species = -1, stats.End = -1, char.Realm = -1, char.Class = -1, char.Level = -1, 
 char.Height = -1, char.Weight = -1, char.Move_Mod = -1, char.Name = "", char.Player = "", resist.Realm = "",adept.Realm = "",
                                     background_Options.Benefits = -1, skills.OUTPUT = -1, img.Size = -1){ 
    
if(unlist(species_Description_DF)[1] == -1 & unlist(char.Species)[1] == -1 & unlist(stats.End)[1] == -1 & unlist(char.Realm)[1] == -1 & unlist(char.Class)[1] == -1 & 
	unlist(char.Level)[1] == -1 & unlist(char.Height)[1] == -1 & unlist(char.Weight)[1] == -1 & unlist(char.Move_Mod)[1] == -1 & unlist(char.Name)[1] == "" & 
	unlist(char.Player)[1] == "" & unlist(resist.Realm)[1] == "" & unlist(adept.Realm)[1] == "" &
    unlist(background_Options.Benefits)[1] == -1 & unlist(skills.OUTPUT)[1] == -1 & unlist(img.Size)[1] == -1) return(-1) 
#returns bonus.Total - A VECTOR CONTAINING THE STAT BONUSES FOR EACH STAT - USED TO CREATE SECOND PAGE.
    stats = stat_Name_DF[,1]
    stats_Fullname = stat_Name_DF[,2]
    
    xTmp <- NULL;xPot <- NULL;xDP <- NULL;xBonus <- NULL;xMisc <- NULL;yTmp <- NULL;yPot <- NULL;yDP <- NULL;yBonus <- NULL;yMisc <- NULL;xRace <- NULL;yRace <- NULL;xTotal <- NULL;
    yTotal <- NULL;bonus.Total <- NULL;xRRStat <- NULL;yRRStat <- NULL;RRStat <- NULL;xRRRace <- NULL;yRRRace <- NULL;RRRace <- NULL;xRRTotal <- NULL;yRRTotal <- NULL;RR.Total <- NULL
    xHP_PP <- NULL;yHP_PP <- NULL;HP_PP <- NULL; 
    xBackgroundOptions <- NULL;yBackgroundOptions <- NULL; backgroundOptions <- NULL;
    
    realms = c("Essence","Channeling","Mentalism")
    realms.Stats = c("Empathy","Intuition","Presence")
    
    #rep(0.258,10)
    xTmp <- rep(0.214,10)
    for( i in 1:10 ) yTmp[i] <- 0.927-i*.019
    xPot <- rep(0.257,10)
    for( i in 1:10 ) yPot[i] <- 0.927-i*.019
    xDP <- rep(0.3,5)
    for( i in 1:5 ) yDP[i] <- 0.927-i*.019
    xBonus <- rep(0.343,10)
    for( i in 1:10 ) yBonus[i] <- 0.927-i*.019
    xRace <- rep(0.386,10)
    for( i in 1:10 ) yRace[i] <- 0.927-i*.019
    race.Bonus <- species_Description_DF[which(species_Description_DF[,1] == char.Species),][2:11]
    xTotal <- rep(0.472,10)
    for( i in 1:10 ){
        yTotal[i] <- 0.927-i*.019
        bonus.Total[i] <- unlist(race.Bonus[i]) + unlist(stats.End[i,4])
    }
nMisc <- NULL; sMisc <- NULL
    if( "Great Strength"  %in% unlist(background_Options.Benefits) ){
        xMisc <- c(xMisc,0.429)
        yMisc <- c(yMisc,yRace[6])
        nMisc <- c(nMisc,"20")
        sMisc <- c(sMisc,ifelse( img.Size == "MED", 5, 14 ))
        bonus.Total[stats=="St"] <- bonus.Total[stats=="St"] + 20
        
    }
    skills.ProfBonus.SkillNames <- unlist(strsplit( char_Profession_Bonus[char_Profession_Bonus[,"Profession"]==char.Class,"Profession.Bonuses"], ";" ) )
    skills.ProfBonus.Bonuses <- unlist(strsplit( char_Profession_Bonus[char_Profession_Bonus[,"Profession"]==char.Class,"Bonuses"], ";" ) )
    xHP_PP <- rep(0.214,2)
    yHP_PP <- 0.7055-0:1*0.0215
    HP_PP[1] <- as.numeric(bonus.Total[stats %in% "Co"]) +  as.numeric(skills.OUTPUT[skills.OUTPUT[,1] == "Body Development","Rank_Bonus"]) + 
                    ifelse( "Body Development" %in% skills.ProfBonus.SkillNames, as.numeric( skills.ProfBonus.Bonuses[skills.ProfBonus.SkillNames == "Body Development"] ), 0 ) + 10
    HP_PP[2] <- as.numeric(bonus.Total[stats %in% char.Realm]) +  as.numeric(skills.OUTPUT[skills.OUTPUT[,1] == "Power Point Development","Rank_Bonus"]) + 
                    ifelse( "Power Point Development" %in% skills.ProfBonus.SkillNames, as.numeric( skills.ProfBonus.Bonuses[skills.ProfBonus.SkillNames == "Power Point Development"] ), 0 ) 
    
    
    xRRStat<- rep(0.3,6)
    for( i in 1:6 ) yRRStat[i] <- 0.653-i*.019
    RRStat <- unlist(c(bonus.Total[8],bonus.Total[9],bonus.Total[10],bonus.Total[1],bonus.Total[1],bonus.Total[3]))
    
    xRRRace<- rep(0.343,6)
    for( i in 1:6 ) yRRRace[i] <- 0.653-i*.019
    RRRace <- c(unlist(species_Description_DF[which(species_Description_DF[,1] == char.Species),12:16]),0)
   
    
    xRRTotal<- rep(0.472,6)
    for( i in 1:6 ){
        yRRTotal[i] <- 0.653-i*.019
        RR.Total[i] <- RRRace[i] + RRStat[i]
    }
    
    
    if( resist.Realm != "" ){
        RR.Total[1:3][realms == resist.Realm] = RR.Total[1:3][realms == resist.Realm] + 40
        xMisc <- c(xMisc, 0.429)
        yMisc <- c(yMisc, 0.653-(which(realms == resist.Realm))*.019)
        nMisc <- c(nMisc, 40)
        sMisc <- c(sMisc,ifelse( img.Size == "MED", 5, 14 ))
    }
    
    if( adept.Realm != "" ){
        RR.Total[1:3][realms == adept.Realm] = RR.Total[1:3][realms == adept.Realm] + 50
        xMisc <- c(xMisc, 0.429)
        yMisc <- c(yMisc, 0.653-(which(realms == adept.Realm))*.019)
        nMisc <- c(nMisc, 50)
        sMisc <- c(sMisc,ifelse( img.Size == "MED", 5, 14 ))
    }

    
max.nChar.Align <- max(nchar(species_Description_DF[,1])) + 1

    
if( nchar(char.Name) < max.nChar.Align ) char.Name.Out <- paste0(char.Name,paste0(rep(" ",max.nChar.Align - nchar(char.Name)),collapse=""), collapse = "")
    xCharName = 0.1837 
    yCharName = 0.9575
    
if( nchar(char.Player) < max.nChar.Align ) char.Player.Out <- paste0(char.Player,paste0(rep(" ",max.nChar.Align - nchar(char.Player)),collapse=""), collapse = "")
    xPlayerName = 0.66
    yPlayerName = 0.9575

    
char.Species.Out <- gsub( "_", " ", char.Species)
if( nchar(char.Species.Out) < max.nChar.Align ) char.Species.Out <- paste0(char.Species.Out,paste0(rep(" ",max.nChar.Align - nchar(char.Species.Out)),collapse=""), collapse = "")
    xSpecies = 0.66
    ySpecies = 0.927
    
    realms = c("Essence","Channeling","Mentalism")
    realms.Stats = c("Empathy","Intuition","Presence")
    
char.Realm.Out <- paste0( realms[realms.Stats %in% stats_Fullname[stats %in% char.Realm]], " (uses ", stats_Fullname[stats %in% char.Realm], ")" )
    if( length(char.Realm.Out) > 1 ){ 
        shortRealmStats <- c("Em","In","Pr")
        char.Realm.Out.Tmp <- char.Realm.Out
        for( i.realm in 1:length(realms.Stats) ) char.Realm.Out.Tmp <- gsub( realms.Stats[i.realm], shortRealmStats[i.realm], char.Realm.Out.Tmp )
        char.Realm.Out <- paste(char.Realm.Out.Tmp,collapse=" & ")
    }
if( nchar(char.Realm.Out) < max.nChar.Align ) char.Realm.Out <- paste0(char.Realm.Out,paste0(rep(" ",max.nChar.Align - nchar(char.Realm.Out)),collapse=""), collapse = "")
    xRealm = 0.66
    yRealm = 0.927-1*.019
    
    char.Class.Out <- gsub("_", " ", char.Class)
if( nchar(char.Class.Out) < max.nChar.Align ) char.Class.Out <- paste0(char.Class.Out,paste0(rep(" ",max.nChar.Align - nchar(char.Class.Out)),collapse=""), collapse = "")
    xClass = 0.66
    yClass = 0.927-2*.019
    
    char.Level.Out = as.character(char.Level)
if( nchar(char.Level.Out) < max.nChar.Align ) char.Level.Out <- paste0(char.Level.Out,paste0(rep(" ",max.nChar.Align - nchar(char.Level.Out)),collapse=""), collapse = "")
    xLevel = 0.66
    yLevel = 0.927-3*0.019

    
    char.Height.Out <- paste(floor(as.numeric(char.Height)/12),"\'",as.numeric(char.Height)%%12,"\"",sep="")
if( nchar(char.Height.Out) < max.nChar.Align ) char.Height.Out <- paste0(char.Height.Out,paste0(rep(" ",max.nChar.Align - nchar(char.Height.Out)),collapse=""), collapse = "")
xHeight <- 0.66
yHeight <- 0.927-5*0.019

    
    char.Weight_Allowance.Out <- as.numeric(char.Weight)/10
if( nchar(char.Weight_Allowance.Out) < max.nChar.Align ) char.Weight_Allowance.Out <- paste0(char.Weight_Allowance.Out,paste0(rep(" ",max.nChar.Align - nchar(char.Weight_Allowance.Out)),collapse=""), collapse = "")
xWeight.Allowance <- 0.66
yWeight.Allowance <- 0.7055

    
    char.Weight.Out <-  paste(char.Weight,"lb.")
if( nchar(char.Weight.Out) < max.nChar.Align ) char.Weight.Out <- paste0(char.Weight.Out,paste0(rep(" ",max.nChar.Align - nchar(char.Weight.Out)),collapse=""), collapse = "")
xWeight <- 0.66
yWeight <- 0.927-6*0.019
    
    
    char.Move.Out <- as.character((mean(c(bonus.Total[2],bonus.Total[7])) + char.Move_Mod))
if( nchar(char.Move.Out) < max.nChar.Align ) char.Move.Out <- paste0(char.Move.Out,paste0(rep(" ",max.nChar.Align - nchar(char.Move.Out)),collapse=""), collapse = "")
xMove <- 0.66
yMove <- 0.7055-1*0.0215

    xBackgroundOptions <- rep(0.14, ifelse( length(background_Options.Benefits[[2]]) > 0, length(background_Options.Benefits[[1]]) + length(background_Options.Benefits[[2]][[1]]), 
                                              length(background_Options.Benefits[[1]]) ) )
    if( length(background_Options.Benefits[[2]]) > 0 ){
        yBackgroundOptions <- c( 0.3275 - 0:(length(background_Options.Benefits[[1]])-1)*0.05, 0.079375 - 0:(length(background_Options.Benefits[[2]][[1]])-1)*0.015 )
        backgroundOptions <- unlist( c( lapply( background_Options.Benefits[[1]], function(x){ if( x[[2]] %in% 6:7 ){ "Stat Increases" } 
                                            else if( x[[2]] == 1 ) { "Secondary Skill Ranks" } else if( x[[2]] == 2 ) { "Primary Skill Ranks" } else { x[[1]][[1]] } } ),
                            unlist(background_Options.Benefits[[2]][[1]]) ) )
    } else{
        yBackgroundOptions <- c( 0.3275 - 0:(length(background_Options.Benefits[[1]])-1)*0.05 )
        backgroundOptions <- unlist( c( lapply( background_Options.Benefits[[1]], function(x){ if( x[[2]] %in% 6:7 ){ "Stat Increases" } 
                                            else if( x[[2]] == 1 ) { "Secondary Skill Ranks" } else if( x[[2]] == 2 ) { "Primary Skill Ranks" } else { x[[1]][[1]] } } ) ) )
    }
    if( "Highly Resistant" %in% backgroundOptions  ) backgroundOptions [ backgroundOptions  == "Highly Resistant" ] = 
        paste0( backgroundOptions [ backgroundOptions  == "Highly Resistant" ], ": ", resist.Realm)
    if( "Exceptionally Enchanted" %in% backgroundOptions  ) backgroundOptions [ backgroundOptions  == "Exceptionally Enchanted" ] = 
        paste0( backgroundOptions [ backgroundOptions  == "Exceptionally Enchanted" ], ": ", adept.Realm)
#    cs.p1<-readPNG( ifelse( img.Size == "NORM", "cs1_NORM.png", ifelse( img.Size == "MED", "cs1_MED.png", "cs1_BIG.png") ) )
    cs.p1.Tmp<-cs.p1
    gc()
    #get size
    h<-dim(cs.p1.Tmp)[1]
    w<-dim(cs.p1.Tmp)[2]
    png("outP1.png", width=w, height=h)
    par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
    plot.new()
    plot.window(0:1, 0:1)
    #fill plot with image
    usr<-par("usr")    
    rasterImage(cs.p1.Tmp, usr[1], usr[3], usr[2], usr[4])
    gc()
    x <- c(xTmp,xPot,xDP,xBonus,xRace,xTotal,xRRStat,xRRRace,xRRTotal,xHP_PP)
    y <- c(yTmp,yPot,yDP,yBonus,yRace,yTotal,yRRStat,yRRRace,yRRTotal,yHP_PP)
    n <- unlist(c( stats.End[,2],stats.End[,3],stats.End[1:5,5],stats.End[,4],race.Bonus,bonus.Total,RRStat,RRRace,RR.Total,HP_PP) )
    s <- rep( ifelse( img.Size == "NORM", 1, ifelse( img.Size == "MED", 5, 14 ) ), length(x) )

    x <- c(x,xMisc)
    y <- c(y,yMisc)
    n <- c(n,nMisc)
    s <- c(s,rep(sMisc))

    x.Left_Justified <- c( xCharName, xPlayerName, xSpecies, xRealm, xClass, xLevel, xHeight, xWeight, xWeight.Allowance, xMove, xBackgroundOptions )
    y.Left_Justified <- c( yCharName, yPlayerName, ySpecies, yRealm, yClass, yLevel, yHeight, yWeight, yWeight.Allowance, yMove, yBackgroundOptions )
    n.Left_Justified <- c( char.Name.Out, char.Player.Out, char.Species.Out, char.Realm.Out, char.Class.Out, char.Level.Out, char.Height.Out, char.Weight.Out, char.Weight_Allowance.Out, 
                                char.Move.Out, backgroundOptions )
    s.Left_Justified <- c( rep( ifelse( img.Size == "NORM", 1.6, ifelse( img.Size == "MED", 8, 17 ) ), 2 ), 
                                rep( ifelse( img.Size == "NORM", 1, ifelse( img.Size == "MED", 5, 14 ) ), length(x.Left_Justified) - 2 - length(backgroundOptions) ),
                                rep( ifelse( img.Size == "NORM", 0.75, ifelse( img.Size == "MED", 3.75, 10.5 ) ), length(backgroundOptions) ))
    s.Left_Justified[ n.Left_Justified == char.Realm.Out ] = ifelse( length(char.Realm) > 1,
                                                                    ifelse( img.Size == "NORM", 1.12, ifelse( img.Size == "MED", 3.5, 9.8 ) ),
                                                                    ifelse( img.Size == "NORM", 1, ifelse( img.Size == "MED", 5, 14 ) ) )
    
    text(x,y, n, cex=s, col = "black")
    text(x.Left_Justified,y.Left_Justified, n.Left_Justified, cex=s.Left_Justified, adj = c(0,0.5), col = "black")
    gc()
    #,col=rgb(.2,.2,.2,.7)
    #close image
    dev.off()
    #}
    cs.p1.Tmp<- NULL
    usr <- NULL
    gc()
    
    return(bonus.Total)
}

#' A SKILLS DATAFRAME CREATOR FUNCTION
#'
#' FUNCTION TO CREATE THE DATA FRAMES FOR RECORDING SKILL INCREASES
#' @param char.Class CHARACTER'S CLASS
#' @param char.Class.FullName CHARACTER'S CLASS'S FULLNAME, IF THEY ARE A MENTALIST MONK OR SOME SUCH
#' @param char.Realm.Name SPELLCASTING REALM CHARACTER USES
#' @param char.Realm SPELLCASTING STAT CHARACTER USES
#' @param background_Options.Benefits LIST CONTAINING ALL BACKGROUND OPTION BENEFITS A PLAYER HAS TAKEN
#' @concept skills 
#' @export
#' @examples
#' create_Skill_Data_Frames()

create_Skill_Data_Frames <-
function(char.Class = -1, char.Class.FullName = -1, char.Realm.Name = -1, char.Realm = -1, background_Options.Benefits = -1){ 

if(unlist(char.Class)[1] == -1 & unlist(char.Class.FullName)[1] == -1 & unlist(char.Realm.Name)[1] == -1 & unlist(char.Realm)[1] == -1 &
	unlist(background_Options.Benefits)[1] == -1) return(-1)         
#returns A LIST WITH THE DATA FRAMES FOR, skills.Increase.Record.DF, background.Skills.Increase.Record.DF, 
#                           skills.Primary.DF, skills.Primary.Stats, skills.Secondary.DF AND skills.Secondary.Stats  
    skills.Increase.Spells.Base <- NULL
    skills.Increase.Spells.Closed <- NULL
    skills.Increase.Spells.Open <- NULL
    skills.Cost.DF <- class_Costs_And_Info[-(which(class_Costs_And_Info[,1] == "Start_Age_Dice")),]
   
    classes <- classesDF[,2]

    #DEAL WITH BEING A LYCANTHROPE (OR NOT!)
    if( !( "Lycanthropy" %in% unlist(background_Options.Benefits) ) ){
        skills.Cost.DF <- skills.Cost.DF[ skills.Cost.DF[,1] != "Control",]
        rownames(skills.Cost.DF) <- NULL
    }
   
    #DEAL WITH KNOWING KUNG FU (OR NOT!)
    if( !( "I Know Kung Fu" %in% unlist(background_Options.Benefits) ) ){
        skills.Cost.DF <- skills.Cost.DF[ ! grepl("Martial Arts:", skills.Cost.DF[,1] ),]
        skills.Cost.DF <- skills.Cost.DF[ ! grepl("Adrenal Defence", skills.Cost.DF[,1] ),]
        rownames(skills.Cost.DF) <- NULL
    } else{
        skills.Cost.DF <- skills.Cost.DF[ ! grepl("Unarmed: Boxing", skills.Cost.DF[,1] ),]
        rownames(skills.Cost.DF) <- NULL
    }
    
    
    
    #FILL IN THE MISSING SPELL LIST OPTIONS ON THE SKILL RECORD DATAFRAME AND SAVE THE RESULT AS THE SKILL RECORD DATAFRAME.
    profession.Spell.Lists.Base <- spell_Lists_Base[,colnames(spell_Lists_Base) %in% char.Class]
    profession.Spell.Lists.Base <- profession.Spell.Lists.Base[profession.Spell.Lists.Base != ""]
    
    profession.Spell.Lists.Closed <- spell_Lists_Closed[,colnames(spell_Lists_Closed) %in% char.Realm.Name]
    if( length(ncol(profession.Spell.Lists.Closed)) ){
        for( i in 1:nrow(profession.Spell.Lists.Closed) ) profession.Spell.Lists.Closed[i,] = paste0( colnames(profession.Spell.Lists.Closed), ": ",profession.Spell.Lists.Closed[i,])
    }
    profession.Spell.Lists.Closed <- profession.Spell.Lists.Closed[profession.Spell.Lists.Closed != ""]
    
    profession.Spell.Lists.Open <- spell_Lists_Open[,colnames(spell_Lists_Open) %in% char.Realm.Name]
    if( length(ncol(profession.Spell.Lists.Open)) ){
        for( i in 1:nrow(profession.Spell.Lists.Open) ) profession.Spell.Lists.Open[i,] = paste0( colnames(profession.Spell.Lists.Open), ": ",profession.Spell.Lists.Open[i,])
    }
    profession.Spell.Lists.Open <- profession.Spell.Lists.Open[profession.Spell.Lists.Open != ""]
    
    
    if( all(is.na( profession.Spell.Lists.Base ) ) ) { skills.Increase.Spells.Base <- NULL
    } else{ skills.Increase.Spells.Base <- paste("Spells: Base Lists: (", na.omit(profession.Spell.Lists.Base) , ")") }
    skills.Increase.Spells.Closed <- paste("Spells: Closed Lists: (", na.omit(profession.Spell.Lists.Closed) , ")")
    skills.Increase.Spells.Open <- paste("Spells: Open Lists: (", na.omit(profession.Spell.Lists.Open) , ")")
    
    skills.Cost.DF.TMP1 <- skills.Cost.DF[1:(which(skills.Cost.DF[,1]=="Spells: Base Lists: (                   )")-1),]
    
    skills.Cost.DF.TMP2 <- rbind( 
                                                skills.Cost.DF[rep( rownames(skills.Cost.DF[skills.Cost.DF=="Spells: Base Lists: (                   )",]) , length(skills.Increase.Spells.Base)), ],
                                                skills.Cost.DF[rep( rownames(skills.Cost.DF[skills.Cost.DF=="Spells: Closed Lists: (                   )",]) , length(skills.Increase.Spells.Closed)), ], 
                                                skills.Cost.DF[rep( rownames(skills.Cost.DF[skills.Cost.DF=="Spells: Open Lists: (                   )",]) , length(skills.Increase.Spells.Open)), ] 
                                            )
    skills.Cost.DF.TMP2[,1] <- c(skills.Increase.Spells.Base, skills.Increase.Spells.Closed, skills.Increase.Spells.Open)        
        
    skills.Cost.DF.TMP3 <- skills.Cost.DF[(which(skills.Cost.DF[,1]=="Spells: Open Lists: (                   )")+1):nrow(skills.Cost.DF),]
    
    skills.Cost.DF <- as.data.frame( rbind(skills.Cost.DF.TMP1, skills.Cost.DF.TMP2, skills.Cost.DF.TMP3) )
    rownames(skills.Cost.DF) <- NULL
    skills.Cost.DF[,1] <- as.character(skills.Cost.DF[,1])
    #SKILL COST FOR THE CHARACTER'S PROFESSION, WITH THE COSTS FOR EACH TIME THEY CAN INCREASE IT IN THEIR OWN COLUMN. GIVES THREE COLUMNS - IF CAN'T INCREASE, COST IS 56. 
    #ALSO BACKGROUND OPTION POINTS, ETC.
    skills.Cost.Char.Class <- t(data.frame( lapply( strsplit(skills.Cost.DF[,char.Class],"/"), function(x){
        if(length(x) <= 3){
            if( any( grepl("-",x) ) ){
                x <- c(56,56,56)
            } else if( any( grepl("[*]",x) ) ){
                as.numeric( x <- rep(x[1],3))
            } else{
                as.numeric( x <- c(x,rep("0",(3-length(x)))) )
            }
        }
    } ) ) )
    
    rownames(skills.Cost.Char.Class) <- NULL
    
    #NUMBER OF TIMES THAT A SKILL CAN BE INCREASED AS A NUMERIC VECTOR. "-" IS 0, "*" IS ANY NUMBER (55 IS MAX), OTHERWISE THE NUMBER OF TIMES A COST IS LISTED.
    numTimesCanIncrease <- apply(skills.Cost.DF[char.Class],1,function(x){
        ifelse( x == "-", 0, ifelse( grepl("[*]",x),55,length(unlist( strsplit(x,"/") )) ) )
    })
   
    skills.Increase.Record.DF <- data.frame(Skills.Name=skills.Cost.DF[,1],Skills.Cost.1=skills.Cost.Char.Class[,1],Skills.Cost.2=skills.Cost.Char.Class[,2],Skills.Cost.3=skills.Cost.Char.Class[,3],
                                                NumTimesIncreased=0,NumTimesCanIncrease = numTimesCanIncrease,TotalRanks = 0, stringsAsFactors = FALSE )
#    skills.Increase.Record.DF[,1] <- as.character(skills.Increase.Record.DF[,1])

    
    #DEAL WITH BACKGROUND OPTION BENEFITS
    background.Skills.Increase.Record.DF <- skills.Increase.Record.DF #VARIABLE TO HOLD INCREASES IN SKILLS FROM BACKGROUND OPTIONS
    
    #DEAL WITH A DARK TEMPTATION
    if( "Dark Temptation" %in% unlist(background_Options.Benefits) ){
        lastSpellRow = max( which( grepl( "Spells: O", skills.Increase.Record.DF[,1] ) ) )
        
        additional.Info <- paste0( "\n\nYou have chosen ", char.Realm.Name, " as your realm for spellcasting.\n")
        spell.Realm.Choice <- choose_Generic( "Realm you want to choose your evil spell list from", c( "Channeling", "Essence", "Mentalism" ), additional.Info ) 
        
        evil.Spells <- spell_Lists_Evil[,colnames(spell_Lists_Evil)==spell.Realm.Choice]
        
        evilSpellList = choose_Generic( "Choose an evil spell list up to level 50", evil.Spells )
        skills.Increase.Record.DF <- as.data.frame( rbind(skills.Increase.Record.DF[1:lastSpellRow,],
                                c(paste0("Spells: Evil: ( ", evilSpellList, " )"), 4, 4, 0, 0, 2, 0),
                                skills.Increase.Record.DF[(lastSpellRow+1):nrow(skills.Increase.Record.DF),]) )
                                
        background.Skills.Increase.Record.DF <- skills.Increase.Record.DF
        background.Skills.Increase.Record.DF[(lastSpellRow+1),"NumTimesIncreased"] = "50"
        
        skills.Cost.DF <- as.data.frame( rbind(skills.Cost.DF[1:lastSpellRow,],
                                unlist( c(paste0("Spells: Evil: ( ", evilSpellList, " )"), ifelse( spell.Realm.Choice == "Channeling", "IN", ifelse( spell.Realm.Choice == "Essence", "EM", "PR" ) ), 
                                        skills.Cost.DF[lastSpellRow,-(1:2)]) ),
                                skills.Cost.DF[(lastSpellRow+1):nrow(skills.Cost.DF),]) )
    }
    
    #DEAL WITH BEING UNUSUALLY ENCHANTED
    if( "Unusually Enchanted" %in% unlist(background_Options.Benefits) ){
        
        if( !( char.Class.FullName %in% classes[(which(classes=="Commoner")+1):(which(classes=="Astrologer"))] ) ){
            skills.Increase.Record.DF[grepl("Spells", gsub("(Spells:).*","\\1",skills.Increase.Record.DF[,1])),2:4] <- round(skills.Increase.Record.DF[grepl("Spells:", gsub("(Spells:).*","\\1",skills.Increase.Record.DF[,1])),2:4]/2L)
        }
        
        cat("You are Unusually Enchanted. You will now pick a spell list to know automatically up to level 10 when you start out.\n")
        if (interactive() ){ readline(prompt=paste0("Press enter to continue"))} else{ cat("Press enter to continue"); readLines("stdin",1)  }
        spells.Subset <- skills.Increase.Record.DF[grepl("Spells:", gsub("(Spells:).*","\\1",skills.Increase.Record.DF[,1])),1] 
        spell.Known <- choose_Generic("Spell List up to level 10",  spells.Subset[!grepl( "Directed",  spells.Subset ) & !grepl( "Evil",  spells.Subset )] )
           
        
        background.Skills.Increase.Record.DF[background.Skills.Increase.Record.DF[,1]==spell.Known,"NumTimesIncreased"] = 10
    }
    
    
    rownames( skills.Increase.Record.DF ) <- NULL
    rownames( background.Skills.Increase.Record.DF ) <- NULL
    #DEFINE THE PRIMARY AND SECONDARY 
    skills.All <- skills.Increase.Record.DF[,1]
    skills.Primary <- c("Maneuver in Armour",
                        skills.All[grepl( gsub("ANY", "", "Weapons: ANY"), skills.All )],
                        skills.All[grepl( gsub("ANY", "", "Unarmed: ANY"), skills.All )],
                        skills.All[grepl( "Spells: Base", skills.All )],
                        skills.All[grepl( "Spells: Closed", skills.All )],
                        skills.All[grepl( "Spells: Open", skills.All )],
                        skills.All[grepl( "Spells: Evil", skills.All )],
                        "Body Development",
                        "Power Point Development",
                        skills.All[grepl( "Directed Spells", skills.All )],
                        "Use Magical Device",
                        skills.All[grepl( "Adrenal Defence", skills.All )],
                        skills.All[grepl( gsub("ANY", "", "Adrenal Moves: ANY"), skills.All )],
                        "Ambush",
                        "Frenzy")
    skills.Secondary <- skills.All[ !(skills.All %in% skills.Primary)] #ALL NON PRIMARY SKILLS ARE SECONDARY SKILLS
    #CREATE DATAFRAMES CONTAINING THE PROFESSION COSTS FOR EACH PRIMARY AND SECONDARY SKILL
    skills.Primary.DF <- data.frame( 1:length(skills.Primary), skills.Primary, stringsAsFactors = FALSE )   
    skills.Secondary.DF <- data.frame( 1:length(skills.Secondary), skills.Secondary, stringsAsFactors = FALSE )
    skills.Primary.Stats <- data.frame( skills.Primary, skills.Cost.DF[skills.Cost.DF[,1] %in% skills.Primary,"Stat"], stringsAsFactors = FALSE )   
    skills.Secondary.Stats <- data.frame( skills.Secondary, skills.Cost.DF[skills.Cost.DF[,1] %in% skills.Secondary,"Stat"], stringsAsFactors = FALSE )
    skills.Primary.Stats[skills.Primary.Stats [,2] == "var",2] =  toupper(paste(char.Realm, collapse="/"))
    
    skills.Increase.Record.DF <- as.data.frame(skills.Increase.Record.DF)
    background.Skills.Increase.Record.DF <- as.data.frame(background.Skills.Increase.Record.DF)
    skills.Increase.Record.DF[,2:ncol(skills.Increase.Record.DF)] <- apply( skills.Increase.Record.DF[,2:ncol(skills.Increase.Record.DF)], 2, as.numeric )
    background.Skills.Increase.Record.DF[,2:ncol(skills.Increase.Record.DF)] <- apply( background.Skills.Increase.Record.DF[,2:ncol(background.Skills.Increase.Record.DF)], 2, as.numeric )
    return( list( skills.Increase.Record.DF, background.Skills.Increase.Record.DF, skills.Primary.DF, skills.Primary.Stats, skills.Secondary.DF, skills.Secondary.Stats ) )
}

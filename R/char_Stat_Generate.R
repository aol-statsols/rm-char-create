#' A STAT GENERATOR FUNCTION
#'
#' FUNCTION TO GENERATE AND ASSIGN ALL STATS THE CHARACTER NEEDS    
#' @param resist.Realm SPELLCASTING REALM CHARACTER IS RESISTANT TO
#' @param adept.Realm SPELLCASTING REALM CHARACTER IS ADEPT WITH
#' @param char.Class.FullName = "Commoner" STRING CONTAINING THE FULL NAME OF THE CHARACTER'S PROFESSION. DEFAULT TO COMMONER
#' @concept output pdf img png character
#' @export
#' @examples
#' char_Stat_Generate()

char_Stat_Generate <-
function( resist.Realm = "", adept.Realm = "", char.Class.FullName = "Commoner" ){ 

if( unlist(resist.Realm)[1] == "" & unlist(adept.Realm)[1] == "" & unlist(char.Class.FullName)[1] == "Commoner" ) return(-1)    
#returns MATRIX CONTAINING THE TEMPORARY AND POTENTIAL STATS OF THE CHARACTER   
 
    classes <- classesDF[,2]
    
    stat.Pot <- NULL
    stat.Roll <- sample(100,10,replace=TRUE)
    while( any(stat.Roll < 20) ){
        stat.Roll[stat.Roll < 20] = sample(100,length( stat.Roll[stat.Roll < 20] ),replace=TRUE)
    }
        
    
    #PLAYER ASSIGNS STATS AND CHOOSES BETWEEN THE ROLLED AND DEFAULT STATS
    sort(stat.Roll)
    default = c(30, 35, 40, 45, 50, 60, 70, 80, 90, 95)
    stats.Compare.DF <- data.frame( Rolled = sort(stat.Roll), Default = default, Difference = sort(stat.Roll) - default ) 
    print( stats.Compare.DF )

    choice = choose_Generic( "column (rolled or default stats) you wish to use for your character.", colnames(stats.Compare.DF)[1:2] )
    if( choice == "Default" ) stat.Roll = stats.Compare.DF[,2]
    stat.Mat <- data.frame(Stat=stat_Name_DF[,1],Tmp=0,stringsAsFactors = FALSE)
    stat.Tmp <- sort(stat.Roll)
    
    stat.Mat <- char_All_Stats_Assign(stat.Tmp,stat.Mat,stat_Name_DF[,1],resist.Realm,adept.Realm,char.Class.FullName)
    #ANY PRIME STATS < 90 ARE RAISED TO 90
    stat.Mat[ stat.Mat[,1] %in% stats_Primary[,which(classes == char.Class.FullName)], 2][ stat.Mat[ stat.Mat[,1] %in% stats_Primary[,which(classes == char.Class.FullName)], 2] < 90 ] = 90
    stat.Tmp <- stat.Mat[,2]
    
    for( i.Stat in 1:10 ){ #GENERATES POTENTIAL STATS
        stat.Tmp.Row = which( potential_Stats_Generator_DF[,"Stat"] == min( potential_Stats_Generator_DF[,"Stat"][ stat.Tmp[i.Stat] <= potential_Stats_Generator_DF[,"Stat"]] ) )
        stat.Pot[i.Stat] = potential_Stats_Generator_DF[stat.Tmp.Row,2] + sum( sample(1:potential_Stats_Generator_DF[stat.Tmp.Row,4],potential_Stats_Generator_DF[stat.Tmp.Row,3],replace=TRUE) )
    }
    return( cbind(stat.Tmp,stat.Pot) )
        
}

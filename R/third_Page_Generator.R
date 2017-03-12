#' A CHARACTER SHEET FUNCTION
#'
#' FUNCTION TO GENERATE THE THIRD, EXTRA PAGE FOR THE CHARACTER SHEET.
#' @param tp.Skills.Other_Gains TRAINING PACKAGE GAINS THAT AREN'T STATS/SKILLS
#' @param tp.Name TRAINING PACKAGE NAME
#' @param background_Options DATAFRAME OF BACKGROUND OPTIONS AVAILABLE
#' @param char.Flaws DATAFRAME CONTAINING ALL FLAWS THE CHARACTER TOOK. DEFAULTS TO EMPTY LIST
#' @param background_Options.Benefits LIST CONTAINING ALL BACKGROUND OPTION BENEFITS A PLAYER HAS TAKEN
#' @param character.Life LIST CONTAINING THE VARIOUS FACTS OF THE CHARACTER'S LIFE
#' @param char.Age.List LIST CONTAINING THE CHARACTER'S AGE AT EACH LEVEL
#' @param resist.Realm STRING SPELLCASTING REALM CHARACTER IS RESISTANT TO
#' @param adept.Realm STRING SPELLCASTING REALM CHARACTER IS ADEPT WITH
#' @param img.Size STRING CONTAINING THE SIZE - "BIG" "MED" - OF THE IMAGE DESIRED.
#' @concept page 
#' @export
#' @examples
#' third_Page_Generator()

third_Page_Generator <-
function(tp.Skills.Other_Gains = -1,  tp.Name = -1,  background_Options = -1,  char.Flaws = list(),  background_Options.Benefits = -1,  
character.Life = -1,  char.Age.List = -1,  resist.Realm = -1,  adept.Realm = -1,  img.Size = -1){ 

if(unlist(tp.Skills.Other_Gains)[1] == -1 & unlist(tp.Name)[1] == -1 & unlist(background_Options)[1] == -1 &
	length(char.Flaws) == 0 & unlist(background_Options.Benefits)[1] == -1 & unlist(character.Life)[1] == -1 &
	unlist(char.Age.List)[1] == -1 & unlist(resist.Realm)[1] == -1 & unlist(adept.Realm)[1] == -1 & unlist(img.Size) == -1 ) return(-1)
#returns:                       NULL

background.Benefits <- list()

    for(i in 1:length(background_Options.Benefits[[1]])){
        if( background_Options.Benefits[[1]][[i]][[2]] %in% 11:16 ){
            background.Benefits[[length(background.Benefits)+1]] <- c( background_Options[(11:16)[( 11:16 == background_Options.Benefits[[1]][[i]][[2]] )],2], 
                                                                        background_Options.Benefits[[1]][[i]][[1]] )
        } else{
            background.Benefits[[length(background.Benefits)+1]] <- background_Options[ (1:10)[( 1:10 == background_Options.Benefits[[1]][[i]][[2]] )], 2]
        }
    }
    

other.TP.Benefits <- tp.Skills.Other_Gains

    y.background.Benefits <- NULL
    y.char.Flaws <- NULL
    y.other.TP.Benefits <- NULL
    y.RecreateCommand <- NULL
    y.Character.Life <- NULL
    n.Character.Life <- NULL
    
x <- NULL; x.Section <- NULL
y <- NULL; x.Section <- NULL
n <- NULL; x.Section <- NULL
    s <- NULL; s.Section <- NULL
j = 0; k = 0; l = 0;
    
    xStart <- 0.000001
    
    yStart = 0.975
    yDiff <- 0.01925

if( length( background.Benefits ) ){
n.background.Benefits  <- NULL
        
        for( bens.i in 1:length( background.Benefits ) ){
            if( length( background.Benefits[[bens.i]] ) == 3 ){
                n.background.Benefits  <- c( n.background.Benefits , paste0(background.Benefits[[bens.i]][1], " - ", background.Benefits[[bens.i]][2], ":"), background.Benefits [[bens.i]][3] ) 
            } else if( length( background.Benefits[[bens.i]] ) == 2 ){
                n.background.Benefits  <- c( n.background.Benefits , paste0(background.Benefits[[bens.i]][1], ": ", background.Benefits[[bens.i]][2]) )
            } else{
                n.background.Benefits  <- c( n.background.Benefits , paste0("Background Option Benefit: ", background.Benefits[[bens.i]] ) )
            }
        }
        
        if( "Special Abilities - Highly Resistant:" %in% n.background.Benefits  ) n.background.Benefits [ n.background.Benefits  == "Special Abilities - Highly Resistant:" ] = 
            paste0( n.background.Benefits [ n.background.Benefits  == "Special Abilities - Highly Resistant:" ], " ", resist.Realm, ":")
        if( "Special Abilities - Exceptionally Enchanted" %in% n.background.Benefits  ) n.background.Benefits [ n.background.Benefits  == "Special Abilities - Exceptionally Enchanted" ] = 
            paste0( n.background.Benefits [ n.background.Benefits  == "Special Abilities - Exceptionally Enchanted" ], " ", adept.Realm, ":")
x.background.Benefits = rep(xStart,length(n.background.Benefits))
for(j in 1:length(n.background.Benefits)) y.background.Benefits[j] = yStart-j*yDiff
#n.background.Benefits =  unlist( lapply ( background.Benefits, paste, collapse = ": " ) )
cex.background.Benefits = rep(ifelse( img.Size == "MED", 4, 12.25 ),length(n.background.Benefits ))

j = j + 4

x <- c(x, xStart, x.background.Benefits)
y <- c(y, yStart+yDiff, y.background.Benefits)
n <- c(n, "Background Option Benefits", n.background.Benefits)
        s <- c(s, ifelse( img.Size == "MED", 6, 14 ), cex.background.Benefits)
}

#if( !is.null(char.Flaws) & nrow(char.Flaws) ){
if( !is.null(char.Flaws) & length(char.Flaws) > 0 ){
n.char.Flaws <- NULL
        
        for( flaws.i in 1:length(char.Flaws[[1]]) ) n.char.Flaws <- c( n.char.Flaws, paste0(char.Flaws[[1]][flaws.i],": ", char.Flaws[[2]][flaws.i]) )
        n.char.Flaws <- gsub( ";", ",", n.char.Flaws)
#n.char.Flaws = apply( char.Flaws, 1, paste, collapse = ": ")
x.char.Flaws = rep(xStart,length(n.char.Flaws))
for(k in 1:length(n.char.Flaws)) y.char.Flaws[k] = yStart-(j+k+1)*yDiff
cex.char.Flaws = rep(ifelse( img.Size == "MED", 4, 8 ),length(n.char.Flaws))



k = k + 4
x <- c(x, xStart, x.char.Flaws)
y <- c(y, yStart - (j)*yDiff, y.char.Flaws)
n <- c(n, "Flaws", n.char.Flaws)
        s <- c(s, ifelse( img.Size == "MED", 6, 14 ), cex.char.Flaws)
}

    if( tp.Name != "" ){
        if( length( other.TP.Benefits ) ){
            x.other.TP.Benefits = rep(xStart,( length(other.TP.Benefits) + 1 ))
            for(l in 1:(length(other.TP.Benefits) + 1)) y.other.TP.Benefits[l] = yStart-(j+k+l+1)*yDiff
            n.other.TP.Benefits = c( paste("Training Package:", as.character(tp.Name) ), paste( "Training Package Benefit:", other.TP.Benefits ) )
            cex.other.TP.Benefits = rep( ifelse( img.Size == "MED", 4, 8 ),(length(other.TP.Benefits)+1))
            
            l = l + 4 
            
            x <- c(x, xStart, x.other.TP.Benefits)
            y <- c(y, yStart-(j+k)*yDiff, y.other.TP.Benefits)
            n <- c(n, "Training Package Benefits", n.other.TP.Benefits)
            s <- c(s, ifelse( img.Size == "MED", 6, 14 ), cex.other.TP.Benefits)
        }
}
    n.Character.Life = c( n.Character.Life, paste0( paste( "You were child number:", unlist(character.Life[[2]]), collapse = "    ")," of ", unlist(character.Life[[1]])) )
    n.Character.Life = c( n.Character.Life, paste( "Your family status growing up was:", unlist(character.Life[[11]]), collapse = "    ") )
if( unlist(character.Life[[11]]) == "Fostered" ){
n.Character.Life = c( n.Character.Life, paste( "Your biological parents\' status is:", unlist(character.Life[[3]])[1], collapse = "    ") )
n.Character.Life = c( n.Character.Life, paste( "Your foster parents\' status is:", unlist(character.Life[[3]])[2], collapse = "    ") )
} else{
n.Character.Life = c( n.Character.Life, paste( "Your parents\' status is:", unlist(character.Life[[3]]), collapse = "    ") )
}
    n.Character.Life = c( n.Character.Life, paste( "The clanhead is your following relation:", unlist(character.Life[[4]]), collapse = "    ") )
    n.Character.Life = c( n.Character.Life, paste0( paste( "Your status in the clan is:", unlist(character.Life[[5]]), collapse = "    "),
                                                        ". ", unlist(character.Life[[6]]) ) )
    n.Character.Life = c( n.Character.Life, paste0( paste( "Your parents' social class was:", unlist(character.Life[[7]]), collapse = "    ") , 
                                                            ". Their occupation was: ", unlist(character.Life[[8]] ), "." ) )
    n.Character.Life = c( n.Character.Life, paste0( paste( "Your character started out age:", unlist(character.Life[[9]]), collapse = "    "), 
                                                                ". They are currently age: ", unlist(char.Age.List[[length(char.Age.List)]]), "." ) )
    if( length(character.Life[[10]][[1]]) == 1 ){
        n.Character.Life = c( n.Character.Life, paste( "Your contact's social class is:", unlist(character.Life[[10]][[1]]), collapse = "    ") )
        n.Character.Life = c( n.Character.Life, paste( "Your contact's friendliness is:", unlist(character.Life[[10]][[2]]), collapse = "    ") )
        n.Character.Life = c( n.Character.Life, paste( "Your contact's loyalty score is:", unlist(character.Life[[10]][[3]]), collapse = "    ") )
    } else{
        n.Character.Life = c( n.Character.Life, paste( "Your contacts' social class are:", paste(unlist(character.Life[[10]][[1]]), collapse = "    ") , collapse = "    ") )
        n.Character.Life = c( n.Character.Life, paste( "Your contacts' friendliness are:", paste(unlist(character.Life[[10]][[2]]), collapse = "    ") , collapse = "    ") )
        n.Character.Life = c( n.Character.Life, paste( "Your contacts' loyalty score are:", paste(unlist(character.Life[[10]][[3]]), collapse = "    ") , collapse = "    ") )
    }

    x.Character.Life = rep( xStart, (length(n.Character.Life)) )
    for( cl in 1:( length(n.Character.Life) ) ) y.Character.Life[cl] = yStart-(j+k+l+cl+1)*yDiff    
    cex.Character.Life = rep( ifelse( img.Size == "MED", 4, 8 ),length(n.Character.Life))

    x <- c(x, xStart, x.Character.Life)
    y <- c(y, yStart-(j+k+l)*yDiff, y.Character.Life)
    n <- c(n, "Life So Far", n.Character.Life)
    s <- c(s, ifelse( img.Size == "MED", 6, 14 ), cex.Character.Life)
    
    xync.DF <- format_fixer( x, y, n, s )
    
    x <- as.numeric( xync.DF[,1] )
    y <- as.numeric( xync.DF[,2] )
    n <- as.character( xync.DF[,3] )
    s <- as.numeric( xync.DF[,4] )
  
    x.Section <- x[n == "Background Option Benefits" | n == "Flaws" | n == "Training Package Benefits" | n == "Life So Far" ]
    y.Section <- y[n == "Background Option Benefits" | n == "Flaws" | n == "Training Package Benefits" | n == "Life So Far" ] 
    n.Section <- gsub("", "", n[n == "Background Option Benefits" | n == "Flaws" | n == "Training Package Benefits" | n == "Life So Far" ] )
    s.Section <- s[n == "Background Option Benefits" | n == "Flaws" | n == "Training Package Benefits" | n == "Life So Far" ]
    
    x.Section <- rep(0.5,length(x.Section))
    
    x <- x[!( n == "Background Option Benefits" | n == "Flaws" | n == "Training Package Benefits" | n == "Life So Far" )]
    y <- y[!( n == "Background Option Benefits" | n == "Flaws" | n == "Training Package Benefits" | n == "Life So Far" )] 
    s <- s[!( n == "Background Option Benefits" | n == "Flaws" | n == "Training Package Benefits" | n == "Life So Far" )]
    n <- n[!( n == "Background Option Benefits" | n == "Flaws" | n == "Training Package Benefits" | n == "Life So Far" )]
    
    (numPages = ifelse( y[length(y)] >= 0, 1, ifelse( (y[length(y)] + y.Section[1]) > 0, 2, 3 ) ))
    
    for( page.i in 1:numPages ){
#        cs.p3<-readPNG( ifelse( img.Size == "NORM", "cs3_NORM.png", ifelse( img.Size == "MED", "cs3_MED.png", "cs3_BIG.png") ) )
        cs.p3.Tmp<-cs.p3
        gc()
        #get size
        h<-dim(cs.p3.Tmp)[1]
        w<-dim(cs.p3.Tmp)[2]
        png( paste0("outP",(2+page.i),".png"), width=w, height=h)
        par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
        plot.new()
        plot.window(0:1, 0:1)
        #fill plot with image
        usr<-par("usr")    
        rasterImage(cs.p3.Tmp, usr[1], usr[3], usr[2], usr[4])
        gc()
        if( !is.null(x) ) text(x[y>0], y[y>0], n[y>0], cex = s[y>0], adj = c(0,0.5), col = "black")
        if( length(x.Section) > 0 ) text(x.Section[y.Section>0], y.Section[y.Section>0], n.Section[y.Section>0], cex = s.Section[y.Section>0], col = "black")
        
        x <- x[y<0]
        n <- n[y<0]
        s <- s[y<0]
        x.Section <- x.Section[y.Section<0]
        n.Section <- n.Section[y.Section<0]
        s.Section <- s.Section[y.Section<0]
        
        y <- y[y<0] + (y[1]-y[y<0][1])
        y.Section <- y.Section[y.Section<0] + (y.Section[1]-y.Section[y.Section<0][1])
        
        gc()
        #,col=rgb(.2,.2,.2,.7)
        #close image
        dev.off()
        #}
        usr <- NULL
        cs.p3.Tmp<- NULL
        gc()
    }    
}

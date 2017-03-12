#' A FORMATTING FUNCTION
#'
#' FUNCTION THAT FIXES THE FORMAT FOR LINES THAT WOULD GO PAST THE END OF THE PAGE.
#' @param x X POSITION OF THE TEXT
#' @param y Y POSITION OF THE TEXT
#' @param text.In ALL TEXT THAT THE CHARACTER SHEET WILL HAVE ON PAGE 3.
#' @param cex.In SIZE OF THE TEXT
#' @concept format output
#' @export
#' @examples
#' format_fixer()

format_fixer <-
function(x = -1,y = -1,text.In = -1,cex.In = -1){ 

if(unlist(x)[1] == -1 & unlist(y)[1] == -1 & unlist(text.In)[1] == -1 & unlist(cex.In)[1] == -1) return(-1) 
#returns DATAFRAME WITH NEW x,y,n,s VALUES
    x.Out <- NULL
    y.Out <- NULL
    text.Out <- NULL
    cex.Out <- NULL
   
    text.Out.List <- list()
    for(text.In.i in 1:length(text.In)){
    
        #text.i = paste0( "        ", text.In[text.In.i] )
        text.i = text.In[text.In.i]
        
            
        #text.Fit <- ifelse( img.Size == "MED", 143, 12 ) 
        text.Fit <- 137
        
        if( nchar( text.i ) > text.Fit ){
            newStrings <- NULL
            text.Out.i <- NULL
            
            newStrings.Number <- 1 + nchar(text.i)%/%text.Fit
            #newStrings.Nchar <-floor(  nchar(text.i)/newStrings.Number )
            newStrings.Nchar <- text.Fit - 8
            
            for( newStrings.Number.i in 1:newStrings.Number ){
                newStrings.End <- ifelse( nchar(text.i) > text.Fit, newStrings.Nchar, nchar( text.i ) )
                
                if( substr(text.i,newStrings.End,newStrings.End) != " " && substr(text.i,newStrings.End+1,newStrings.End+1) != " " && newStrings.End != nchar(text.i) ){
                
                    if( newStrings.Nchar + nchar( gsub( "(*)( )(.*)", "\\1", substr( text.i, newStrings.End+1, nchar(text.i) ) ) ) > text.Fit  ){
                        newStrings.End <- newStrings.End - nchar( gsub( "(.*)( )(*)", "\\3", substr( text.i, 1, newStrings.End ) ) )
                        newStrings <- c( newStrings, paste0( substr( text.i, 1, newStrings.End ) ) )
                        text.i <-  substr( text.i, newStrings.End + 1, nchar( text.i ) ) 
                    } else{
                        newStrings.End = newStrings.End + nchar( gsub( "(*)( )(.*)", "\\1", substr( text.i, newStrings.End+1, nchar(text.i) ) ) )
                        newStrings <- c( newStrings, substr( text.i, 1, newStrings.End ) )
                        text.i <-  paste0( substr( text.i, newStrings.End + 1, nchar( text.i ) ) ) 
                    }
                    
                } else{
                    newStrings <- c( newStrings, substr( text.i, 1, newStrings.End ) )
                    text.i <- ifelse( newStrings.End != nchar(text.i), paste0(substr( text.i, newStrings.End + 1, nchar( text.i ) ) ), substr( text.i, newStrings.End + 1, nchar( text.i ) ) ) 
                }
                
                if( substr(text.i,1,1) == " " && substr(text.i,1,8) != "        " ) text.i = substr( text.i, 2, nchar(text.i) )
            }
            
            if( nchar(text.i) > 0 ) newStrings <- c(newStrings,text.i)
            
            newStrings <- c(newStrings,"")
            
            text.Out.List[[text.In.i]] <- newStrings
             
        } else{
            text.Out.List[[text.In.i]] <- text.i
        } 
        
    }
    
    for( text.Out.i in 1:length(text.Out.List) ){
        
        if( length(text.Out.List[[text.Out.i]]) == 1 ){
            
            x.Out <- c(x.Out, x[text.Out.i])
            y.Out <- c(y.Out, y[text.Out.i])
            cex.Out <- c(cex.Out, cex.In[text.Out.i])
            text.Out <- c(text.Out,unlist(text.Out.List[[text.Out.i]]))
            
        } else{
            
            if( text.Out.i == 1 ){
            
                x.Out <- rep( x[1], ( length(text.Out.List[[text.Out.i]]) ) )
                
                y[2:length(y)] = y[2:length(y)] - 0.01925*( length(text.Out.List[[text.Out.i]]) - 1 )
                y.Out <- rep( y[1], length(text.Out.List[[text.Out.i]]) ) - 0.01925*( seq( 1, length(text.Out.List[[text.Out.i]]) ) - 1 )
                
                cex.Out <- rep( cex.In[1], length(text.Out.List[[text.Out.i]]) )
                
                text.Out <- text.Out.List[[text.Out.i]]
                
            } else if ( text.Out.i > 1 & text.Out.i < length(text.Out.List) ){
            
                x.Out <- c( x.Out, rep( x[text.Out.i], length(text.Out.List[[text.Out.i]]) ) )
                
                y[(text.Out.i+1):length(y)] = y[(text.Out.i+1):length(y)] - 0.01925*( length(text.Out.List[[text.Out.i]]) - 1 )
                y.Out <- c( y.Out, rep( y[text.Out.i], length(text.Out.List[[text.Out.i]]) )-0.01925*( seq( 1, length(text.Out.List[[text.Out.i]]) ) - 1 ) )
                
                cex.Out <- c( cex.Out, rep( cex.In[text.Out.i], length(text.Out.List[[text.Out.i]]) ) )
                
                text.Out <- c(text.Out,text.Out.List[[text.Out.i]])
                
            } else if ( text.Out.i == length(text.Out.List) ){
            
                x.Out <- c( x.Out, rep( x[text.Out.i], length(text.Out.List[[text.Out.i]]) ) )
                
                y.Out <- c( y.Out, rep( y[text.Out.i], length(text.Out.List[[text.Out.i]]) )-0.01925*( seq( 1, length(text.Out.List[[text.Out.i]]) ) - 1 ) )
                
                cex.Out <- c( cex.Out, rep( cex.In[text.Out.i], ( length(text.Out.List[[text.Out.i]]) ) ) )
                
                text.Out <- c(text.Out,text.Out.List[[text.Out.i]])
                
            }
            
        }
        
    }
  
    return( data.frame( x = x.Out, y = y.Out, n = text.Out, c = cex.Out ) )
}

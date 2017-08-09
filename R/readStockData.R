readStockData <-
function(stockList){     
   n <- length(stockList)
   stockData <- matrix(NA,nrow=n,ncol=5)
   colnames(stockData) <- c("quote","vol","beta","div","divYield")
   symbols <- names <- abbrs <- character(n)
   parseHTML <- function(x)
   {
     x <- paste(strsplit(x,"&auml;")[[1]],collapse="\u00E4")   
     x <- paste(strsplit(x,"&ouml;")[[1]],collapse="\u00F6")
     x <- paste(strsplit(x,"&Auml;")[[1]],collapse="\u00C4")
     x <- paste(strsplit(x,"&Ouml;")[[1]],collapse="\u00D6")
     x <- paste(strsplit(x,"&aring;")[[1]],collapse="\u00E5")
     x <- paste(strsplit(x,"&Aring;")[[1]],collapse="\u00C5")
     x <- paste(strsplit(x,"&#039;")[[1]],collapse="\u0027")
     x
   }
   for(i in 1:n){
     print(i)
     #data <- readLines(paste("http://www.kauppalehti.fi/",stockList[i],sep=""))
     data <- readLines(paste("https://www.kauppalehti.fi/",stockList[i],sep=""))
     r0 <- paste(data,collapse=" ")
     #r0 <- iconv(r0,"latin-9","UTF-8")
     r0 <- iconv(r0,"latin1","UTF-8")#New
     r0 <- parseHTML(r0) #New
     r0a <- strsplit(r0,split="<h1>")[[1]][2]
     parLeft <- gregexpr("(",r0a,fixed=TRUE)[[1]][1]
     parRight <- gregexpr(")",r0a,fixed=TRUE)[[1]][1]
     names[i] <- substr(r0a,1,parLeft-2)
     abbrs[i] <- substr(r0a,parLeft+1,parRight-1)
     
     r1 <- strsplit(r0a,split="P\u00E4\u00E4t\u00F6skurssi")[[1]][2]
     r1a <- strsplit(r1,split="\">")[[1]][2]
     endPoint <- gregexpr("<", r1a)[[1]][1]-1
     quote <- as.numeric(substr(r1a,1,endPoint))
     
     r2 <- strsplit(r1,split="Volatiliteetti")[[1]][2]
     startPoint <- gregexpr("\">",r2)[[1]][3]+2
     endPoint <- gregexpr("%",r2)[[1]][3]-1
     vol <- as.numeric(substr(r2,startPoint,endPoint))
     
     r3 <- strsplit(r2,split="Beta")[[1]][2]
     startPoint <- gregexpr("\">",r2)[[1]][3]+2
     endPoint <- gregexpr("</",r2)[[1]][4]-2
     beta <- as.numeric(substr(r2,startPoint,endPoint))
     
     
     r4 <- strsplit(r3,split="Selite")[[1]][2]
     r4a <- strsplit(r4,split="<td>")[[1]][2]
     r4b <- strsplit(r4,split="\">")[[1]][2]
     endPointA <- gregexpr("<",r4a)[[1]][1]-1
     endPointB <- gregexpr("<",r4b)[[1]][1]-1
     #div <-  scan(text=substr(r4a,1,endPoint),dec=",")
     divChar <- substr(r4a,1,endPointA)
     divChar1 <- gsub(" ", "", divChar, fixed = TRUE)#Removing spaces
     divChar2 <- gsub(",",".",divChar1, fixed = TRUE)#Changing decimal character
     div <- as.numeric(divChar2)
     
     divYieldChar <- substr(r4b,1,endPointB)
     divYieldChar1 <- gsub(",",".",divYieldChar, fixed = TRUE)
     if(divYieldChar1=="-") divYieldChar1 <-"0" 
     divYield <- as.numeric(divYieldChar1)
     
     r5 <- strsplit(r4,split="yntitunnus")[[1]][2]
     r5a <- strsplit(r5,split="\">")[[1]][2]
     endPoint <- gregexpr("<",r5a,fixed=TRUE)[[1]][1]-1
     symbols[i] <- substr(r5a,1,endPoint)
         
     stockData[i,]<- c(quote,vol,beta,div,divYield)
   }
   rownames(stockData)<- symbols  
   na.omit(data.frame(names,abbrs,stockData,stringsAsFactors=FALSE))#Added
}

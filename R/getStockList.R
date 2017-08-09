getStockList <-
function(markets="XHEL",listIds="largecap")
{ 
  MARKETS <- paste(markets,collapse="&markets=")
  LISTS <- paste(listIds,collapse="&listIds=")  
  url <- paste("https://www.kauppalehti.fi/5/i/porssi/porssikurssit/lista.jsp?reverse=false&order=alpha&markets=",MARKETS,"&volume=cur&psize=50&listIds=",LISTS,"&gics=0&refresh=0&currency=euro",sep="")
  stock.list <- readLines(url)  
  result0 <- paste(stock.list,collapse=" ")
  #result0 <- iconv(result0,"latin-9","UTF-8")
  result0 <- iconv(result0,"latin1","UTF-8")#New
  result0 <- strsplit(result0,split="main content")[[1]][-1]#New May 9, 2017
  result1 <- strsplit(result0,split="<td><a href=\"/")[[1]][-1]
  n <- length(result1)
  endPoints <- gregexpr("\"",result1)
  lst <- character(n)
  for(i in 1:n)
    lst[i] <- substr(result1[i],1,endPoints[[i]][1]-1)
  lst
}

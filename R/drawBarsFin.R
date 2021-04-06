drawBarsFin <-
function(data,pop,regions,start="2020-06-01",end="last",measure="new_cases",atop=TRUE,perMillion=FALSE,drawMean=TRUE,bars=TRUE){
  val <- NULL
  Alue <- NULL
  val_per_million <- NULL
  Region <- NULL
  val_smoothed_per_million <- NULL
  val_smoothed <- NULL
  total_cases_per_million <- NULL
  total_cases <- NULL
  
  if(perMillion | drawMean | !bars | length(regions) ==1) atop <- FALSE
  data$date <- as.Date(data$Aika)
  dataP <- data %>% filter(!is.na(val)& Alue %in% regions) %>% group_by(Alue) %>% 
    mutate(val_smoothed=rollapply(val,width=7,mean,fill=NA), total_cases=cumsum(val), Region=Alue)
  
  if(perMillion)
    for(region in pop$Alue){
      dataP[dataP$Alue==region,"total_cases_per_million"] <- 1e6*dataP[dataP$Alue==region,"total_cases"]/
        pop[pop$Alue==region,"val"]
      dataP[dataP$Alue==region,"val_per_million"] <- 1e6*dataP[dataP$Alue==region,"val"]/
        pop[pop$Alue==region,"val"] 
      dataP[dataP$Alue==region,"val_smoothed_per_million"] <- 1e6*dataP[dataP$Alue==region,"val_smoothed"]/
        pop[pop$Alue==region,"val"]
    }
  
  if(!is.null(start)) dataP <- dataP %>% filter(date >= start)
  if(end!="last") dataP <- dataP %>% filter(date <= end)
  if(measure=="new_cases"){
    if(perMillion)
      p <- ggplot(dataP,aes(x=date,y=val_per_million,fill=Region))+ylab("New cases per million inhabitants") else
      p <- ggplot(dataP,aes(x=date,y=val,fill=Region))+ylab("New cases")
  
      p <- p + scale_x_date(name="Date",breaks = date_breaks("months"),labels = date_format("%b"))
  
    if(atop){
      p <- p + geom_bar(stat="identity")
    }else{
      if(bars){
        p <- p + geom_bar(stat="identity", position=position_dodge())
      }
      if(drawMean){
        if(perMillion){
          if(length(regions) > 1)
            p <- p + geom_line(aes(x=date,y=val_smoothed_per_million,colour=Region),size=1) else
            p <- p + geom_line(aes(x=date,y=val_smoothed_per_million),size=1)
        }else{
          if(length(regions) > 1)
            p <- p + geom_line(aes(x=date,y=val_smoothed,colour=Region),size=1) else
            p <- p + geom_line(aes(x=date,y=val_smoothed),size=1)
        }
      }  
    }
  } else
  if(measure=="total_cases"){
    if(perMillion)
      p <- ggplot(dataP,aes(x=date,y=total_cases_per_million,fill=Region))+ylab("Total cases per million inhabitants") else
        p <- ggplot(dataP,aes(x=date,y=total_cases,fill=Region))+ylab("Total cases")
      
      p <- p + scale_x_date(name="Date",breaks = date_breaks("months"),labels = date_format("%b"))
      
      if(atop){
        p <- p + geom_bar(stat="identity")
      }else{
        if(bars){
          p <- p + geom_bar(stat="identity", position=position_dodge())
        }
        if(drawMean){
          if(perMillion){
            if(length(regions) > 1)
              p <- p + geom_line(aes(x=date,y=total_cases_per_million,colour=Region),size=1) else
                p <- p + geom_line(aes(x=date,y=total_cases_per_million),size=1)
          }else{
            if(length(regions) > 1)
              p <- p + geom_line(aes(x=date,y=total_cases,colour=Region),size=1) else
                p <- p + geom_line(aes(x=date,y=total_cases),size=1)
          }
        }  
      }
    
  }  
  p
}

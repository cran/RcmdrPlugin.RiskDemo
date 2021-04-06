drawTests <-
function(data,countries,start="2020-06-01",end="last",measure="new_tests",atop=TRUE,perThousand=FALSE,drawMean=TRUE,bars=TRUE,log=FALSE){
  new_tests <- NULL
  Country <- NULL
  new_tests_smoothed <- NULL
  new_tests_per_thousand <- NULL
  new_tests_smoothed_per_thousand <- NULL
  total_tests <- NULL
  total_tests_per_thousand <- NULL
  
  if(perThousand | drawMean | !bars | length(countries) ==1) atop <- FALSE
  data <- data %>% filter(!is.na(new_tests) & location %in% countries)
  data$date <- as.Date(data$date)
  dataP <- data %>% 
    mutate(Country=location, new_tests = ifelse(new_tests>0,new_tests,0)) %>% 
    select(Country,date,new_tests,new_tests_smoothed,new_tests_per_thousand,new_tests_smoothed_per_thousand, total_tests, total_tests_per_thousand)
  if(!is.null(start)) dataP <- dataP %>% filter(date >= start)
  if(end!="last") dataP <- dataP %>% filter(date <= end)
  
  if(measure=="new_tests"){
    if(perThousand)
      p <- ggplot(dataP,aes(x=date,y=new_tests_per_thousand,fill=Country))+ylab("New tests per thousand inhabitants") else
      p <- ggplot(dataP,aes(x=date,y=new_tests,fill=Country))+ylab("New tests")  
    p <- p + scale_x_date(name="Date",breaks = date_breaks("months"),labels = date_format("%b"))
    if(log) p <- p + scale_y_continuous(trans="log10")
    
    if(atop){
      p <- p + geom_bar(stat="identity")
    }else{
      if(bars){
        p <- p + geom_bar(stat="identity", position=position_dodge())
      }
      if(drawMean){
        if(perThousand){
          if(length(countries) > 1)
            p <- p + geom_line(aes(x=date,y=new_tests_smoothed_per_thousand,colour=Country),size=1) else
            p <- p + geom_line(aes(x=date,y=new_tests_smoothed_per_thousand),size=1)  
        }else{
          if(length(countries) > 1)
            p <- p + geom_line(aes(x=date,y=new_tests_smoothed,colour=Country),size=1) else
            p <- p + geom_line(aes(x=date,y=new_tests_smoothed),size=1)
        }
      }
    }  
  }else if(measure=="total_tests"){
    if(perThousand)
      p <- ggplot(dataP,aes(x=date,y=total_tests_per_thousand,fill=Country))+ylab("Total tests per thousand inhabitants") else
      p <- ggplot(dataP,aes(x=date,y=total_tests,fill=Country))+ylab("Total tests")  
    p <- p + scale_x_date(name="Date",breaks = date_breaks("months"),labels = date_format("%b"))
    if(log) p <- p + scale_y_continuous(trans="log10")
    if(atop){
      p <- p + geom_bar(stat="identity")
    }else{
      if(bars){
        p <- p + geom_bar(stat="identity", position=position_dodge())
      }
      if(drawMean){
        if(perThousand){
          if(length(countries) > 1)
            p <- p + geom_line(aes(x=date,y=total_tests_per_thousand,colour=Country),size=1) else
            p <- p + geom_line(aes(x=date,y=total_tests_per_thousand),size=1)  
          }else{
            if(length(countries) > 1)
              p <- p + geom_line(aes(x=date,y=total_tests,colour=Country),size=1) else
              p <- p + geom_line(aes(x=date,y=total_tests),size=1)
          }
        }
      }  
  }
  p
}

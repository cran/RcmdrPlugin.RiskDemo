drawPositiveRate <-
function(data,countries,start="2020-06-01",end="last",measure="positive_rate",curve=TRUE,bars=FALSE,log=FALSE){
  Country <- NULL
  tests_per_case <- NULL
  positive_rate <- NULL 
  
  data <- data %>% filter(!is.na(positive_rate) & location %in% countries)
  data$date <- as.Date(data$date)
  dataP <- data %>% 
    mutate(Country=location) %>% 
    select(Country,date,positive_rate,tests_per_case)
  
  if(!is.null(start)) dataP <- dataP %>% filter(date >= start)
  if(end!="last") dataP <- dataP %>% filter(date <= end)
  if(measure=="tests_per_case"){
    p <- ggplot(dataP,aes(x=date,y=tests_per_case))+ylab("New tests per case")
    } else{
      p <- ggplot(dataP,aes(x=date,y=100*positive_rate))+ylab("Positive rate (%)")  
    }
    if(log) p <- p + scale_y_continuous(n.breaks=10,trans="log10") else
      p <- p + scale_y_continuous(n.breaks=10)
    if(curve){if(length(countries)>1)
      p <- p + geom_line(aes(colour=Country),size=1) else
      p <- p + geom_line(size=1)  
    }
      
    if(bars){
      if(length(countries)>1) 
        p <- p + geom_bar(aes(fill=Country),stat="identity", position=position_dodge()) else
        p <- p + geom_bar(stat="identity", width=1)  
    }
    p + scale_x_date(name="Date",breaks = date_breaks("months"),labels = date_format("%b"))
}

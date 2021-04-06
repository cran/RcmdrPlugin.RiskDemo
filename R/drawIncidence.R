drawIncidence <-
function(data,countries,start="2020-06-01",end="last",weeks=2,log=TRUE){
  Country <- NULL
  incidence <- NULL
  new_cases_per_million <- NULL
  
  data <- data %>% filter(!is.na(new_cases_per_million)) %>% 
    mutate(Country=location,new_cases_per_million= ifelse(new_cases_per_million>0,new_cases_per_million,0))
  data$date <- as.Date(data$date)
  data <- data %>% group_by(Country) %>% mutate(incidence=0.1*rollapply(new_cases_per_million,7*weeks,sum,align='right',fill=NA))
  dataP <- data %>% filter(Country %in% countries) %>%
    select(Country,date,incidence) 
  if(!is.null(start)) dataP <- dataP %>% filter(date >=start)
  if(end!="last") dataP <- dataP %>% filter(date <= end)
  last <- dataP  %>% summarise(date=last(date),incidence=last(incidence))
  ord <- order(last$incidence,decreasing=TRUE)
  dataP <- dataP %>% mutate(Country=factor(Country,levels=last$Country[ord]))
  
  p <- ggplot(dataP,aes(x=date,y=incidence,group=Country,color=Country))+
    geom_line() + ggtitle(paste0("Cases per 100 000 inhabitants in ",weeks," weeks")) +
    ylab("Incidence") +
    scale_x_date(name="",breaks = date_breaks("months"),labels = date_format("%b"))+
    geom_text(aes(x=date+10,y=repel(incidence,log),label=round(incidence,1)),data=last)
  if(log){
    p <- p + scale_y_continuous(breaks=c(1,2,5,10,20,50,100,200,500,1000), 
                                    trans="log")}
  p
}

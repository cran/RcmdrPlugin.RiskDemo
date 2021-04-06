drawIncidenceFin <-
function(data,pop,regions,start="2020-06-01",end="last",weeks=2,
                             includeAllRegions=TRUE,log=TRUE){
  val <- NULL
  Alue <- NULL
  incidence <- NULL
  Aika <- NULL
  
  data$Aika <- as.Date(data$Aika)
  KA <- "Kaikki Alueet"
  dataP <- data %>% filter(!is.na(val)) %>% group_by(Alue) %>% mutate(incidence=100000.0*rollapply(val,7*weeks,sum,align='right',fill=NA)) %>%
    mutate(incidence=ifelse(incidence==0,NA,incidence)) 
  
  if(!is.null(start)) dataP <- dataP %>% filter(Aika >=start)
  if(end!="last") dataP <- dataP %>% filter(Aika <= end)
  regionsAll <- unique(dataP$Alue)
  
  for(region in regionsAll){
    dataP[dataP$Alue==region,"incidence"] <- dataP[dataP$Alue==region,"incidence"]/pop[pop$Alue==region,"val"] 
  }
  last <- dataP %>% filter(Alue %in% regions) %>% group_by(Alue) %>% summarise(Aika=last(Aika),incidence=last(incidence))
  ord <- order(last$incidence,decreasing=TRUE)
  
  if(includeAllRegions){
    data1 <- dataP %>% filter(Alue %in% c(KA,regions))  %>% 
      mutate(Alue=factor(Alue,levels=c(KA,last$Alue[ord])))
  } else{
    data1 <- dataP %>% filter(Alue %in% regions)  %>% 
      mutate(Alue=factor(Alue,levels=last$Alue[ord]))
  }
    
  last1 <- data1 %>% group_by(Alue) %>% summarise(Aika=last(Aika),incidence=last(incidence))

  
  p <- ggplot(data1,aes(x=Aika,y=incidence,group=Alue,color=Alue))  + guides(color=guide_legend(title="Region"))+
    ggtitle(paste0("Finland: Cases per 100 000 inhabitants in ",weeks," weeks"))+ 
    geom_line() + 
    scale_x_date(name="",breaks = date_breaks("months"),labels = date_format("%b"))+
    geom_text(aes(x=Aika+10,y=repel(incidence,log),label=round(incidence,1)),data=last1)
  
  if(log){
    p <- p + scale_y_continuous(breaks=c(1,2,5,10,20,50,100,200), 
                                      trans="log")
  }  
  
  if(includeAllRegions){
    p + scale_color_manual(values=1:(length(ord)+1))} else{
    p + scale_color_manual(values=2:(length(ord)+1))  
  }
      
}

drawBars <-
function(data,countries,start="2020-06-01",end="last",measure="new_cases",atop=TRUE,perMillion=FALSE,drawMean=TRUE,bars=TRUE){
  
  new_cases_per_million <- NULL
  Country <- NULL
  new_cases <- NULL
  new_cases_smoothed_per_million <- NULL
  new_cases_smoothed <- NULL
  new_deaths_per_million <- NULL
  new_deaths <- NULL
  new_deaths_smoothed_per_million <- NULL
  new_deaths_smoothed <- NULL
  total_deaths_per_million <- NULL
  total_deaths <- NULL
  total_cases_per_million <- NULL
  total_cases <- NULL
  hosp_patients_per_million <- NULL
  hosp_patients <- NULL
  icu_patients_per_million <- NULL
  icu_patients <- NULL
  reproduction_rate <- NULL
  
  if(perMillion | drawMean | !bars | length(countries) ==1) atop <- FALSE
  
  data$date <- as.Date(data$date)
  if(!is.null(start)) data <- data %>% filter(date >= start)
  if(end!="last") data <- data %>% filter(date <= end)
  
  if(measure=="new_cases"){
    data <- data %>% filter(!is.na(new_cases) & location %in% countries)
    dataP <- data %>%  
      mutate(Country=location, new_cases = ifelse(new_cases>0,new_cases,0), 
             new_cases_per_million = ifelse(new_cases_per_million>0,new_cases_per_million,0))
  
    if(perMillion)
      p <- ggplot(dataP,aes(x=date,y=new_cases_per_million,fill=Country))+ylab("New cases per million inhabitants") else
      p <- ggplot(dataP,aes(x=date,y=new_cases,fill=Country))+ylab("New cases")  
    p <- p + scale_x_date(name="Date",breaks = date_breaks("months"),labels = date_format("%b"))
    if(atop){
      p <- p + geom_bar(stat="identity")
    }else{
      if(bars){
        p <- p + geom_bar(stat="identity", position=position_dodge())
      }
      if(drawMean){
        if(perMillion){
          if(length(countries) > 1)
            p <- p + geom_line(aes(x=date,y=new_cases_smoothed_per_million,colour=Country),size=1) else
            p <- p + geom_line(aes(x=date,y=new_cases_smoothed_per_million),size=1)  
        }else{
          if(length(countries) > 1)
            p <- p + geom_line(aes(x=date,y=new_cases_smoothed,colour=Country),size=1) else
            p <- p + geom_line(aes(x=date,y=new_cases_smoothed),size=1)
        }
      }
    }  
  }else if(measure=="new_deaths"){
    data <- data %>% filter(!is.na(new_deaths) & location %in% countries)
    dataP <- data %>% 
      mutate(Country=location, new_deaths = ifelse(new_deaths>0,new_deaths,0))
    
    if(perMillion)
      p <- ggplot(dataP,aes(x=date,y=new_deaths_per_million,fill=Country))+ylab("New deaths per million inhabitants") else
        p <- ggplot(dataP,aes(x=date,y=new_deaths,fill=Country))+ylab("New deaths")  
    p <- p + scale_x_date(name="Date",breaks = date_breaks("months"),labels = date_format("%b"))
    if(atop & length(countries) > 1){
      p <- p + geom_bar(stat="identity")
    }else{
      if(bars){
        p <- p + geom_bar(stat="identity", position=position_dodge())
      }
      if(drawMean){
        if(perMillion){
          if(length(countries) > 1)
            p <- p + geom_line(aes(x=date,y=new_deaths_smoothed_per_million,colour=Country),size=1) else
              p <- p + geom_line(aes(x=date,y=new_deaths_smoothed_per_million),size=1)  
        }else{
          if(length(countries) > 1)
            p <- p + geom_line(aes(x=date,y=new_deaths_smoothed,colour=Country),size=1) else
              p <- p + geom_line(aes(x=date,y=new_deaths_smoothed),size=1)
        }
      }
    }  
  }else if(measure=="total_deaths"){
    data <- data %>% filter(!is.na(total_deaths) & location %in% countries)
    dataP <- data %>%  
      mutate(Country=location, total_deaths = ifelse(total_deaths>0,total_deaths,0))
    
    if(perMillion)
      p <- ggplot(dataP,aes(x=date,y=total_deaths_per_million,fill=Country))+ylab("Total deaths per million inhabitants") else
        p <- ggplot(dataP,aes(x=date,y=total_deaths,fill=Country))+ylab("Total deaths")  
    p <- p + scale_x_date(name="Date",breaks = date_breaks("months"),labels = date_format("%b"))
    if(atop & length(countries) > 1){
      p <- p + geom_bar(stat="identity")
    }else{
      if(bars){
        p <- p + geom_bar(stat="identity", position=position_dodge())
      }
      if(drawMean){
        if(perMillion){
          if(length(countries) > 1)
            p <- p + geom_line(aes(x=date,y=total_deaths_per_million,colour=Country),size=1) else
              p <- p + geom_line(aes(x=date,y=total_deaths_per_million),size=1)  
        }else{
          if(length(countries) > 1)
            p <- p + geom_line(aes(x=date,y=total_deaths,colour=Country),size=1) else
              p <- p + geom_line(aes(x=date,y=total_deaths),size=1)
        }
      }
    }  
  }else if(measure=="total_cases"){
    data <- data %>% filter(!is.na(total_cases) & location %in% countries)
    dataP <- data %>% 
      mutate(Country=location, total_cases = ifelse(total_cases>0,total_cases,0))
    
    if(perMillion)
      p <- ggplot(dataP,aes(x=date,y=total_cases_per_million,fill=Country))+ylab("Total cases per million inhabitants") else
        p <- ggplot(dataP,aes(x=date,y=total_cases,fill=Country))+ylab("Total cases")  
    p <- p + scale_x_date(name="Date",breaks = date_breaks("months"),labels = date_format("%b"))
    if(atop & length(countries) > 1){
      p <- p + geom_bar(stat="identity")
    }else{
      if(bars){
        p <- p + geom_bar(stat="identity", position=position_dodge())
      }
      if(drawMean){
        if(perMillion){
          if(length(countries) > 1)
            p <- p + geom_line(aes(x=date,y=total_cases_per_million,colour=Country),size=1) else
              p <- p + geom_line(aes(x=date,y=total_cases_per_million),size=1)  
        }else{
          if(length(countries) > 1)
            p <- p + geom_line(aes(x=date,y=total_cases,colour=Country),size=1) else
              p <- p + geom_line(aes(x=date,y=total_cases),size=1)
        }
      }
    }  
  }else if(measure=="hosp_patients"){
    data <- data %>% filter(!is.na(hosp_patients) & location %in% countries)
    dataP <- data %>%  
      mutate(Country=location, hosp_patients = ifelse(hosp_patients>0,hosp_patients,0))
    
    if(perMillion)
      p <- ggplot(dataP,aes(x=date,y=hosp_patients_per_million,fill=Country))+ylab("Hospitalized patients per million inhabitants") else
        p <- ggplot(dataP,aes(x=date,y=hosp_patients,fill=Country))+ylab("Hospitalized patients")  
    p <- p + scale_x_date(name="Date",breaks = date_breaks("months"),labels = date_format("%b"))
    if(atop & length(countries) > 1){
      p <- p + geom_bar(stat="identity")
    }else{
      if(bars){
        p <- p + geom_bar(stat="identity", position=position_dodge())
      }
      if(drawMean){
        if(perMillion){
          if(length(countries) > 1)
            p <- p + geom_line(aes(x=date,y=hosp_patients_per_million,colour=Country),size=1) else
              p <- p + geom_line(aes(x=date,y=hosp_patients_per_million),size=1)  
        }else{
          if(length(countries) > 1)
            p <- p + geom_line(aes(x=date,y=hosp_patients,colour=Country),size=1) else
              p <- p + geom_line(aes(x=date,y=hosp_patients),size=1)
        }
      }
    }  
  }else if(measure=="icu_patients"){
    data <- data %>% filter(!is.na(icu_patients) & location %in% countries)
    dataP <- data %>% 
      mutate(Country=location, icu_patients = ifelse(icu_patients>0,icu_patients,0))
    
    if(perMillion)
      p <- ggplot(dataP,aes(x=date,y=icu_patients_per_million,fill=Country))+ylab("ICU patients per million inhabitants") else
        p <- ggplot(dataP,aes(x=date,y=icu_patients,fill=Country))+ylab("ICU patients")  
    p <- p + scale_x_date(name="Date",breaks = date_breaks("months"),labels = date_format("%b"))
    if(atop & length(countries) > 1){
      p <- p + geom_bar(stat="identity")
    }else{
      if(bars){
        p <- p + geom_bar(stat="identity", position=position_dodge())
      }
      if(drawMean){
        if(perMillion){
          if(length(countries) > 1)
            p <- p + geom_line(aes(x=date,y=icu_patients_per_million,colour=Country),size=1) else
              p <- p + geom_line(aes(x=date,y=icu_patients_per_million),size=1)  
        }else{
          if(length(countries) > 1)
            p <- p + geom_line(aes(x=date,y=icu_patients,colour=Country),size=1) else
              p <- p + geom_line(aes(x=date,y=icu_patients),size=1)
        }
      }
    }  
  }else if(measure=="case_fatality_rate"){
    data <- data %>% filter(!is.na(total_deaths) & location %in% countries)
    dataP <- data %>%  
      mutate(Country=location, total_deaths = ifelse(total_deaths>0,total_deaths,0), total_cases = ifelse(total_cases>0,total_cases,0))

    p <- ggplot(dataP,aes(x=date,y=100*total_deaths/total_cases,fill=Country))+ylab("Case fatality rate (%)")  
    p <- p + scale_x_date(name="Date",breaks = date_breaks("months"),labels = date_format("%b"))+
      scale_y_continuous(n.breaks=10)

      if(bars){
        p <- p + geom_bar(stat="identity", position=position_dodge())
      }
      if(drawMean){
          if(length(countries) > 1)
            p <- p + geom_line(aes(x=date,y=100*total_deaths/total_cases,colour=Country),size=1) else
              p <- p + geom_line(aes(x=date,y=100*total_deaths/total_cases),size=1)
      }
  }else if(measure=="reproduction_rate"){
    data <- data %>% filter(!is.na(reproduction_rate) & location %in% countries)
    dataP <- data %>% 
      mutate(Country=location, reproduction_rate = ifelse(reproduction_rate>0,reproduction_rate,0))
    
    p <- ggplot(dataP,aes(x=date,y=reproduction_rate,fill=Country))+ylab("Reproduction rate (R)")  
    p <- p + scale_x_date(name="Date",breaks = date_breaks("months"),labels = date_format("%b"))+
      scale_y_continuous(n.breaks=10)
    
    if(bars){
      p <- p + geom_bar(stat="identity", position=position_dodge())
    }
    if(drawMean){
      if(length(countries) > 1)
        p <- p + geom_line(aes(x=date,y=reproduction_rate,colour=Country),size=1) else
          p <- p + geom_line(aes(x=date,y=reproduction_rate),size=1)+
            geom_hline(yintercept=1,linetype="dashed",col="red")
    }
  }
    
  
  p
}

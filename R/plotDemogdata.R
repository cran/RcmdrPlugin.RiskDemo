plotDemogdata <-
function () {
  defaults <- list (initial.x = 10, initial.type="functions", initial.datatype="rate",
           initial.total = 1, initial.female=0, initial.male=0,
           initial.ages = "0:110",initial.years = "1878:2015",initial.trans="TRUE")
  dialog.values <- getDialog ("plotDemogdata", defaults)  
  initializeDialog(title = gettextRcmdr("Plot demographic data"))
  #xBox <- variableListBox(top, Numeric(), title = gettextRcmdr("Variable (pick one)"),initialSelection = varPosn(dialog.values$initial.x, "numeric"))
  
  agesVariable <- tclVar(dialog.values$initial.ages)
  yearsVariable <- tclVar(dialog.values$initial.years)
  transVariable <- tclVar(dialog.values$initial.trans)
  typeVariable <- tclVar(dialog.values$initial.type)
  datatypeVariable <- tclVar(dialog.values$initial.datatype)
  totalVariable <- tclVar(dialog.values$initial.total)
  femaleVariable <- tclVar(dialog.values$initial.female)
  maleVariable <- tclVar(dialog.values$initial.male)
  
  countries <- c("Australia","Austria","Belarus","Belgia","Bulgaria","Canada",
                 "Chile","Czech Republic","Denmark","Estonia","Finland","France",
                 "Germany","Greece","Hungary","Iceland","Ireland","Israel","Italy",
                 "Japan", "Latvia","Lithuania","Luxenbourg","Netherlands",
                 "New Zealand","Norway","Poland","Portugal","Russia","Slovakia",
                 "Slovenia","Spain","Sweden","Switzerland","Taiwan","U.K.","U.S.A.",
                 "Ukraine")
  
  dataFrame <- tkframe(top)
  xBox <- variableListBox(dataFrame, countries, title = gettextRcmdr("Country (pick one or more)"),initialSelection =  dialog.values$initial.x,
               selectmode="multiple")
  onOK <- function() {
    #Get the list of countries
    x <- getSelection(xBox)
    nCountries <- length(x)
    if (nCountries == 0) {
      errorCondition(recall = plotDemogdata, message = gettextRcmdr("You must select at least one country."))
      return()
    }
    xi <- numeric(nCountries)
    for(i in 1:nCountries) xi[i] <- which(x[i]==countries)

    type <- tclvalue(typeVariable)
    datatype <- tclvalue(datatypeVariable)
    if(type=="time") xlabel <- "Time" else xlabel <- "Age"
    total <- as.numeric(tclvalue(totalVariable))
    female <- as.numeric(tclvalue(femaleVariable))
    male <- as.numeric(tclvalue(maleVariable))
    Series <- c("Total","Female","Male")[as.logical(c(total,female,male))]
    series1 <- "total"
    nSeries <- total+female+male
    if(total==1){
       if(female==1){
          series2 <- "female"
          if(male==1){ series3 <- "male"}
       }else
       if(male==1){series2 <- "male"}
    } else
    if(female==1) {
       series1 <- "female"
       if(male==1){series2 <- "male"}
    }else
    if(male==1)
        {series1 <- "male"}
    ages <- tclvalue(agesVariable)
    years <- tclvalue(yearsVariable)
    trans <- tclvalue(transVariable)
    putDialog ("plotDemogdata", list (initial.x = xi-1, initial.type=type,
            initial.datatype = datatype, initial.total = total, initial.female=female,
            initial.male=male, initial.ages = ages, initial.years = years,
            initial.trans=trans))
    closeDialog()
    doItAndPrint('data(countries.mort)')
    doItAndPrint(paste('plot(countries.mort[[',xi[1],']],plot.type="',type,'",series="',
            series1,'",datatype="',datatype,'",ages=c(',ages,'),years=c(',years,
            '),transform=',trans,',xlab="',xlabel,'")', sep = ""))
    if(nSeries>1) {doItAndPrint(paste('lines(countries.mort[[',xi[1],']],plot.type="',type,'",series="',
            series2,'",datatype="',datatype,'",ages=c(',ages,'),years=c(',years,
            '),transform=',trans,',xlab="',xlabel,'",lty=2)', sep = ""))}
    if(nSeries==3) {doItAndPrint(paste('lines(countries.mort[[',xi[1],']],plot.type="',type,'",series="',
            series3,'",datatype="',datatype,'",ages=c(',ages,'),years=c(',years,
            '),transform=',trans,',xlab="',xlabel,'",lty=3)', sep = ""))}

    if(nSeries>1)
       if(type=="functions"){
         if(datatype=="rate"){legend("bottomright",legend=Series,lty=1:nSeries)}else
           {legend("bottomleft",legend=Series,lty=1:nSeries)}
       }else{
          if(datatype=="rate"){legend("topright",legend=Series,lty=1:nSeries)}else
            {legend("topleft",legend=Series,lty=1:nSeries)}
       }
    if(nCountries>1 && nSeries==1){
       for(i in 2:nCountries){
           doItAndPrint(paste('lines(countries.mort[[',xi[i],']],plot.type="',type,'",series="',
            series1,'",datatype="',datatype,'",ages=c(',ages,'),years=c(',years,
            '),transform=',trans,',xlab="',xlabel,'",lty=',i,')', sep = ""))
       }
       if(type=="functions"){
          if(datatype=="rate"){legend("bottomright",legend=x,lty=1:nCountries)}else
             {legend("bottomleft",legend=x,lty=1:nCountries)}
       }else{
          if(datatype=="rate"){legend("topright",legend=x,lty=1:nCountries)}else
             {legend("topleft",legend=x,lty=1:nCountries)}
       }
    }
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "plot.demogdata", reset = "plotDemogdata", apply = "plotDemogdata")
  optionsFrame <- tkframe(top)
  leftFrame <- tkframe(optionsFrame)
  typeFrame <- tkframe(leftFrame)
  datatypeFrame <- tkframe(leftFrame)
  seriesFrame <- tkframe(optionsFrame)
   
  radioButtons(leftFrame, name = "type", buttons = c("functions", 
      "time"), values = c("functions", "time"), 
      labels = gettextRcmdr(c("Age", "Time")), 
      title = gettextRcmdr("As function of     "),
      initialValue = dialog.values$initial.type)
  radioButtons(leftFrame, name = "datatype", buttons = c("rate", 
      "pop"), values = c("rate", "pop"), 
      labels = gettextRcmdr(c("Death rate", "Exposure")), 
      title = gettextRcmdr("Data type     "),
      initialValue = dialog.values$initial.datatype)

  checkBoxes(window=optionsFrame,frame="seriesFrame", boxes=c("total","female","male"),
             initialValues=c(dialog.values$initial.total,dialog.values$initial.female,
             dialog.values$initial.male),
             labels=gettextRcmdr(c("Total","Female","Male")),title="Series")
 
  rightFrame <- tkframe(optionsFrame)
  agesFrame <- tkframe(rightFrame)
  agesVariable <- tclVar(dialog.values$initial.ages)
  agesField <- ttkentry(agesFrame, width = "10", 
                              textvariable = agesVariable)
                              
  yearsFrame <- tkframe(rightFrame)
  yearsVariable <- tclVar(dialog.values$initial.years)
  yearsField <- ttkentry(yearsFrame, width = "10", 
            textvariable = yearsVariable)

  transFrame <- tkframe(rightFrame)
  radioButtons(rightFrame, name = "trans", buttons = c("yes",
      "no"), values = c("TRUE", "FALSE"),
      labels = gettextRcmdr(c("Yes", "No")),
      title = gettextRcmdr("Take logarithm ?     "),
      initialValue = dialog.values$initial.trans)


  
  tkgrid(getFrame(xBox), sticky = "nw")

  #tkgrid(labelRcmdr(seriesFrame, text = "Series",fg="blue"), sticky = "w", padx=c(10, 0))

  tkgrid(labelRcmdr(rightFrame, text = "Subset",fg="blue"), sticky = "w", padx=c(10, 0))
  tkgrid(labelRcmdr(agesFrame, text = gettextRcmdr("Ages: ")), 
         agesField, sticky = "w", padx=c(10, 0))
  tkgrid(agesFrame, sticky = "w")
  
  tkgrid(labelRcmdr(yearsFrame, text = gettextRcmdr("Years: ")), 
         yearsField, sticky = "w", padx=c(10, 0))
  tkgrid(yearsFrame, sticky = "w")
  tkgrid(transFrame, sticky = "w", padx=c(10,0),pady=c(10,0))

  tkgrid(typeFrame, sticky = "w")
  tkgrid(datatypeFrame, sticky = "w", pady=c(10,0))
  tkgrid(leftFrame, seriesFrame,rightFrame, sticky = "nw")
  #tkgrid(seriesFrame, rightFrame, sticky = "nw")
  tkgrid(dataFrame, optionsFrame, sticky="w")
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  tkgrid.configure(agesField, sticky = "e")
  dialogSuffix()
}

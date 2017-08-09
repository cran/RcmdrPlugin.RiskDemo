viewDemogdata <-
function () {
  defaults <- list (initial.x = 10, initial.datatype="rate",
           initial.series = "total", 
           initial.ages = "0:110",initial.years = "1878:2015",initial.out="fin")
  dialog.values <- getDialog ("viewDemogdata", defaults)  
  initializeDialog(title = gettextRcmdr("Choose demographic data"))
  #xBox <- variableListBox(top, Numeric(), title = gettextRcmdr("Variable (pick one)"),initialSelection = varPosn(dialog.values$initial.x, "numeric"))
  
  
  datatypeVariable <- tclVar(dialog.values$initial.datatype)
  seriesVariable <- tclVar(dialog.values$initial.series)
  agesVariable <- tclVar(dialog.values$initial.ages)
  yearsVariable <- tclVar(dialog.values$initial.years)
  outVariable <- tclVar(dialog.values$initial.out)
  
  countries <- c("Australia","Austria","Belarus","Belgia","Bulgaria","Canada",
                 "Chile","Czech Republic","Denmark","Estonia","Finland","France",
                 "Germany","Greece","Hungary","Iceland","Ireland","Israel","Italy",
                 "Japan", "Latvia","Lithuania","Luxenbourg","Netherlands",
                 "New Zealand","Norway","Poland","Portugal","Russia","Slovakia",
                 "Slovenia","Spain","Sweden","Switzerland","Taiwan","U.K.","U.S.A.",
                 "Ukraine")
  
  dataFrame <- tkframe(top)
  xBox <- variableListBox(dataFrame, countries, title = gettextRcmdr("Country (pick one)"),initialSelection =  dialog.values$initial.x)
  onOK <- function() {
    x <- getSelection(xBox)
    xi <- which(x==countries)
    if (length(x) == 0) {
      errorCondition(recall = viewDemogdata, message = gettextRcmdr("You must select a variable."))
      return()
    }
    datatype <- tclvalue(datatypeVariable)
    series <- as.character(tclvalue(seriesVariable))
    ages <- tclvalue(agesVariable)
    years <- tclvalue(yearsVariable)
    out <- tclvalue(outVariable)
    
    putDialog ("viewDemogdata", list (initial.x = xi-1, initial.datatype= datatype, initial.series = series, initial.ages = ages, initial.years = years,initial.out =out))
    closeDialog()
    doItAndPrint('data(countries.mort)')
    dData <- paste('countries.mort[[',xi,']]',sep="")
    doItAndPrint(paste(out,' <- extract.years(extract.ages(',dData,',ages=c(',ages,
         '),combine.upper=FALSE),years=c(',years,'))',sep=""))
    
    doItAndPrint(paste('View(',out,'$',datatype,
          '$', series,',title="',out,': ',x,' ',
          series,' ',datatype,'")',sep = ""))
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "View", reset = "viewDemogdata", apply = "viewDemogdata")
  optionsFrame <- tkframe(top)
  leftFrame <- tkframe(optionsFrame)
  datatypeFrame <- tkframe(leftFrame)
  seriesFrame <- tkframe(optionsFrame)
   
  radioButtons(leftFrame, name = "datatype", buttons = c("rate", 
      "pop"), values = c("rate", "pop"), 
      labels = gettextRcmdr(c("Death rate", "Exposure")), 
      title = gettextRcmdr("Data type     "),
      initialValue = dialog.values$initial.datatype)          
  radioButtons(optionsFrame, name = "series", buttons = c("total", 
      "female", "male"), values = c("total", "female", "male"), 
      labels = gettextRcmdr(c("Total", "Female", "Male")), 
      title = gettextRcmdr("Series"),
      initialValue = dialog.values$initial.series)
 
  rightFrame <- tkframe(optionsFrame)
  agesFrame <- tkframe(rightFrame)
  agesVariable <- tclVar(dialog.values$initial.ages)
  agesField <- ttkentry(agesFrame, width = "10", 
                              textvariable = agesVariable)
                              
  yearsFrame <- tkframe(rightFrame)
  yearsVariable <- tclVar(dialog.values$initial.years)
  yearsField <- ttkentry(yearsFrame, width = "10", 
            textvariable = yearsVariable)
  
  outFrame <- tkframe(rightFrame)
  outVariable <- tclVar(dialog.values$initial.out)
  outField <- ttkentry(outFrame, width = "20", 
            textvariable = outVariable)
  
  
  tkgrid(getFrame(xBox), sticky = "nw")
  tkgrid(labelRcmdr(rightFrame, text = "Subset",fg="blue"), sticky = "w", padx=c(10, 0))
  tkgrid(labelRcmdr(agesFrame, text = gettextRcmdr("Ages: ")), 
         agesField, sticky = "w", padx=c(10, 0))
  tkgrid(agesFrame, sticky = "w")
  
  tkgrid(labelRcmdr(yearsFrame, text = gettextRcmdr("Years: ")), 
         yearsField, sticky = "w", padx=c(10, 0))
  tkgrid(yearsFrame, sticky = "w")
  tkgrid(datatypeFrame, sticky = "w")
  
  tkgrid(labelRcmdr(rightFrame, text = "Output object",fg="blue"), sticky = "w", 
           padx=c(10, 0),pady=c(5,0))
  tkgrid(labelRcmdr(outFrame, text = gettextRcmdr("Name: ")), 
         outField, sticky = "w", padx=c(10, 0))
  tkgrid(outFrame, sticky = "w")
  
  
  
  tkgrid(leftFrame, seriesFrame,rightFrame, sticky = "nw")
  #tkgrid(seriesFrame, rightFrame, sticky = "nw")
  tkgrid(dataFrame, optionsFrame, sticky="w")
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  tkgrid.configure(agesField, sticky = "e")
  dialogSuffix()
}

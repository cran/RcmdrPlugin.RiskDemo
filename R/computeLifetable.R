computeLifetable <-
function () {
  defaults <- list (initial.input = 'fin', initial.out="fin.lt",
           initial.series = "total", 
           initial.ages = "0:110",initial.years = "1878:2015",initial.print=0,
           initial.plot=0)
  dialog.values <- getDialog ("computeLifetable", defaults)  
  initializeDialog(title = gettextRcmdr("Compute life table"))
  #xBox <- variableListBox(top, Numeric(), title = gettextRcmdr("Variable (pick one)"),initialSelection = varPosn(dialog.values$initial.x, "numeric"))
  inputVariable <- tclVar(dialog.values$initial.input)
  outVariable <- tclVar(dialog.values$initial.out)
  seriesVariable <- tclVar(dialog.values$initial.series)
  agesVariable <- tclVar(dialog.values$initial.ages)
  yearsVariable <- tclVar(dialog.values$initial.years)
  printVariable <- tclVar(dialog.values$initial.print)
  plotVariable <- tclVar(dialog.values$initial.plot)
  
  onOK <- function() {
    input <- tclvalue(inputVariable)
    out <- tclvalue(outVariable)
    series <- tclvalue(seriesVariable) 
    ages <- tclvalue(agesVariable)
    years <- tclvalue(yearsVariable) 
    print <- tclvalue(printVariable)
    plot <- tclvalue(plotVariable)
    
    putDialog ("computeLifetable", list (initial.input=input, initial.out= out, initial.series = series, initial.ages = ages, initial.years = years, initial.print=print,initial.plot=plot))
    closeDialog()
    #dData <- paste('countries.mort[[',xi,']]',sep="")
    doItAndPrint('data(fin)')
    
    years <- paste('c(',years,')[c(',years,') %in% ',input,'$year]',sep="")
    doItAndPrint(paste(out,' <- lifetable(',input,',series="',series,'",years=',years,',ages=c(',ages,'))',sep=""))
    
    if(print==1){
      doItAndPrint(paste('print(',out,')',sep=""))
    }
    if(plot==1){
      doItAndPrint(paste('plot(',out,')',sep=""))
    }    
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "lifetable", reset = "computeLifetable", 
          apply = "computeLifetable")
 
  dataFrame <- tkframe(top)
  seriesFrame <- tkframe(top)
  rightFrame <- tkframe(top)
  checkboxFrame <- tkframe(dataFrame)
 
  checkBoxes(frame="checkboxFrame", boxes=c("print","plot"), 
             initialValues=c(dialog.values$initial.print,dialog.values$initial.plot),  
             labels=gettextRcmdr(c("Print lifetable?","Plot life expectancies?")))
 
  inputFrame <- tkframe(dataFrame)
  inputVariable <- tclVar(dialog.values$initial.input)
  inputField <- ttkentry(inputFrame, width = "20", 
            textvariable = inputVariable)
    
  outFrame <- tkframe(dataFrame)
  outVariable <- tclVar(dialog.values$initial.out)
  outField <- ttkentry(outFrame, width = "20", 
            textvariable = outVariable)
  
  
           
  radioButtons(top, name = "series", buttons = c("total", 
      "female", "male"), values = c("total", "female", "male"), 
      labels = gettextRcmdr(c("Total", "Female", "Male")), 
      title = gettextRcmdr("Series"),
      initialValue = dialog.values$initial.series)
 
  
  agesFrame <- tkframe(rightFrame)
  agesVariable <- tclVar(dialog.values$initial.ages)
  agesField <- ttkentry(agesFrame, width = "10", 
                              textvariable = agesVariable)
                              
  yearsFrame <- tkframe(rightFrame)
  yearsVariable <- tclVar(dialog.values$initial.years)
  yearsField <- ttkentry(yearsFrame, width = "10", 
            textvariable = yearsVariable)
  
  
  tkgrid(labelRcmdr(dataFrame, text = "Input object",fg="blue"), sticky = "w", 
           padx=c(10, 0),pady=c(5,0))
  tkgrid(labelRcmdr(inputFrame, text = gettextRcmdr("Name: ")), 
         inputField, sticky = "w", padx=c(10, 0))          
  tkgrid(inputFrame, sticky = "w")
           
  tkgrid(labelRcmdr(dataFrame, text = "Output object",fg="blue"), sticky = "w", 
           padx=c(10, 0),pady=c(5,0))
  tkgrid(labelRcmdr(outFrame, text = gettextRcmdr("Name: ")), 
         outField, sticky = "w", padx=c(10, 0))
  tkgrid(outFrame, sticky = "w")
  
  tkgrid(labelRcmdr(rightFrame, text = "Subset",fg="blue"), sticky = "w", padx=c(0, 0))
  tkgrid(labelRcmdr(agesFrame, text = gettextRcmdr("Ages: ")), 
         agesField, sticky = "w", padx=c(0, 0))
  tkgrid(agesFrame, sticky = "w")
  
  tkgrid(labelRcmdr(yearsFrame, text = gettextRcmdr("Years: ")), 
         yearsField, sticky = "w", padx=c(0, 0))
  tkgrid(yearsFrame, sticky = "w")
  
  
  tkgrid(dataFrame, seriesFrame,rightFrame, sticky = "nw")
  tkgrid(checkboxFrame, sticky = "w", padx=c(10,0), pady=c(10,0))

  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  dialogSuffix()
}

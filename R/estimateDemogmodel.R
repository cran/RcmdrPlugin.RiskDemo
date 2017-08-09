estimateDemogmodel <-
function () {
  defaults <- list (initial.input = 'fin', initial.out="fin.lca", initial.method="lca",
           initial.series = "total", 
           initial.ages = "0:100",initial.years = "1950:2015",initial.res=0,initial.print=0,
           initial.plot=0)
  dialog.values <- getDialog ("estimateDemogmodel", defaults)  
  initializeDialog(title = gettextRcmdr("Estimating a demographic model"))
  #xBox <- variableListBox(top, Numeric(), title = gettextRcmdr("Variable (pick one)"),initialSelection = varPosn(dialog.values$initial.x, "numeric"))
  inputVariable <- tclVar(dialog.values$initial.input)
  outVariable <- tclVar(dialog.values$initial.out)
  methodVariable <- tclVar(dialog.values$initial.method)
  seriesVariable <- tclVar(dialog.values$initial.series)
  agesVariable <- tclVar(dialog.values$initial.ages)
  yearsVariable <- tclVar(dialog.values$initial.years)
  resVariable <- tclVar(dialog.values$initial.res)
  printVariable <- tclVar(dialog.values$initial.print)
  plotVariable <- tclVar(dialog.values$initial.plot)
  
  onOK <- function() {
    input <- tclvalue(inputVariable)
    out <- tclvalue(outVariable)
    method <- tclvalue(methodVariable)
    series <- tclvalue(seriesVariable) 
    ages <- tclvalue(agesVariable)
    years <- tclvalue(yearsVariable)
    res <- tclvalue(resVariable) 
    print <- tclvalue(printVariable)
    plot <- tclvalue(plotVariable)
    
    putDialog ("estimateDemogmodel", list (initial.input=input, initial.out= out,
    initial.method=method, initial.series = series, initial.ages = ages, initial.years = years, initial.res=res, initial.print=print,initial.plot=plot))
    closeDialog()
    doItAndPrint('data(fin')
    #dData <- paste('countries.mort[[',xi,']]',sep="")
    
    years <- paste('c(',years,')[c(',years,') %in% ',input,'$year]',sep="")
 
    if(method=="lca"){
       doItAndPrint(paste(out,' <- lca(',input,',series="',series,'",years=',years,
       ',ages=c(',ages,'),interpolate=TRUE)',sep=""))}else
    if(method=="bms"){
       doItAndPrint(paste(out,' <- bms(',input,',series="',series,'",years=',years,
       ',ages=c(',ages,'),interpolate=TRUE)',sep=""))}else
    if(method=="fdm"){
       doItAndPrint(paste(out,' <- fdm(',input,',series="',series,
       '",ages=c(',ages,'))',sep=""))
    }
  
    if(res==1){
       doItAndPrint(paste('plot(residuals(',out,'))',sep=""))
    }
    
    if(print==1 && method=="fdm"){
      doItAndPrint(paste('summary(',out,')',sep=""))
    }
    if(plot==1 && (method=="lca"||method=="bms")){
      if(res==1){
         doItAndPrint("dev.new()")
      }
      doItAndPrint(paste('plot(',out,')',sep=""))
    }    
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "lca", reset = "estimateDemogmodel", 
          apply = "estimateDemogmodel")
 
  dataFrame <- tkframe(top)
  checkboxFrame <- tkframe(dataFrame)
  methodFrame <- tkframe(top)
  seriesFrame <- tkframe(top)
  rightFrame <- tkframe(top)
  
  checkBoxes(frame="checkboxFrame", boxes=c("res","plot","print"), 
             initialValues=c(dialog.values$initial.res,dialog.values$initial.plot,
             dialog.values$initial.print),  
             labels=gettextRcmdr(c("Plot residuals ?","Plot model (Lee Carter)?","Print summary (functional model)?")))
 
  inputFrame <- tkframe(dataFrame)
  inputVariable <- tclVar(dialog.values$initial.input)
  inputField <- ttkentry(inputFrame, width = "20", 
            textvariable = inputVariable)
    
  outFrame <- tkframe(dataFrame)
  outVariable <- tclVar(dialog.values$initial.out)
  outField <- ttkentry(outFrame, width = "20", 
            textvariable = outVariable)
  
  radioButtons(top, name = "method", buttons = c("lca", 
      "bms", "fdm"), values = c("lca", "bms", "fdm"), 
      labels = gettextRcmdr(c("Lee Carter (standard)", "Lee Carter (BMS methodology)", "Functional model")), 
      title = gettextRcmdr("Method"),
      initialValue = dialog.values$initial.method)
            
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
  
  tkgrid(labelRcmdr(rightFrame, text = "Subset",fg="blue"), sticky = "w", padx=c(10, 0))
  tkgrid(labelRcmdr(agesFrame, text = gettextRcmdr("Ages: ")), 
         agesField, sticky = "w", padx=c(10, 0))
  tkgrid(agesFrame, sticky = "w")
  
  tkgrid(labelRcmdr(yearsFrame, text = gettextRcmdr("Years: ")), 
         yearsField, sticky = "w", padx=c(10, 0))
  tkgrid(yearsFrame, sticky = "w")
  
  
  tkgrid(dataFrame, methodFrame, seriesFrame,rightFrame, sticky = "nw")
  tkgrid(checkboxFrame, sticky = "w", padx=c(10,0), pady=c(10,0))

  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  dialogSuffix()
}

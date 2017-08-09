forecastPopulation <-
function () {
  defaults <- list (initial.input = 'fin', initial.fcast="fin.fcast",
           initial.out="fin.pcast",
           initial.type = "functions",
           initial.trans ="FALSE",
           initial.ages = "0:100",initial.years = "1990+0:5*10")
  dialog.values <- getDialog ("forecastPopulation", defaults)
  initializeDialog(title = gettextRcmdr("Making population forecast"))
  
  inputVariable <- tclVar(dialog.values$initial.input)
  fcastVariable <- tclVar(dialog.values$initial.fcast)
  outVariable <- tclVar(dialog.values$initial.out)
  typeVariable <- tclVar(dialog.values$initial.type)
  transVariable <- tclVar(dialog.values$initial.trans)
  agesVariable <- tclVar(dialog.values$initial.ages)
  yearsVariable <- tclVar(dialog.values$initial.years)
  
  onOK <- function() {
    input <- tclvalue(inputVariable)
    fcast <- tclvalue(fcastVariable)
    out <- tclvalue(outVariable)
    type <- tclvalue(typeVariable)
    trans <- tclvalue(transVariable)
    ages <- tclvalue(agesVariable)
    years <- tclvalue(yearsVariable)

    putDialog ("forecastPopulation", list (initial.input=input, initial.fcast=fcast,
            initial.out= out, initial.type = type, initial.trans=trans,initial.ages = ages,
            initial.years = years))
    closeDialog()
    doItAndPrint('data(fin)')
    doItAndPrint('data(fin.fcast)')
    #dData <- paste('countries.mort[[',xi,']]',sep="")

    #years <- paste('c(',years,')[c(',years,') %in% ',input,'$year]',sep="")
    #doItAndPrint(paste(out,' <- lifetable(',input,',series="',series,'",years=',years,',ages=c(',ages,'))',sep=""))

    fcastObject <- eval(parse(text=fcast))
    yearsObject <- eval(parse(text=paste('c(',years,')',sep="")))
    xlmin <- min(yearsObject)
    xlmax <- max(yearsObject)
    inputObject <- eval(parse(text=input))
    series <- names(fcastObject$rate)[1]

    doItAndPrint(paste(out,' <- pop.pred(',input,',',fcast,')',sep=""))

    if(any(yearsObject %in% inputObject$year)){
      if(type=="functions"){
        doItAndPrint(paste('plot(',input,',plot.type="',type,'",series="',
            series,'",transform=',trans,',datatype="pop",ages=c(',ages,'),years=c(',years,
            '),xlab="Age")', sep = ""))
      }else{
      doItAndPrint(paste('plot(',input,',plot.type="',type,'",series="',
            series,'",transform=',trans,',datatype="pop",ages=c(',ages,'),years=c(',years,
            '),xlab="Year",xlim=c(',xlmin,',',xlmax,'))', sep = ""))
      }
      doItAndPrint(paste('lines(',out,',plot.type="',type,'",series="',
            series,'",transform=',trans,',datatype="pop",ages=c(',ages,'),years=c(',years,
            '),lty=2)', sep = ""))
    }else{
      doItAndPrint(paste('plot(',out,',plot.type="',type,'",series="',
            series,'",transform=',trans,',datatype="pop",ages=c(',ages,'),years=c(',years,
            '),lty=2)', sep = ""))
    }
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "pop.pred", reset = "forecastPopulation",
          apply = "forecastPopulation")

  dataFrame <- tkframe(top)
  leftFrame  <- tkframe(top)
  rightFrame <- tkframe(top)

  typeFrame <- tkframe(leftFrame)
  transFrame <- tkframe(leftFrame)
  
  
  inputFrame <- tkframe(dataFrame)
  inputVariable <- tclVar(dialog.values$initial.input)
  inputField <- ttkentry(inputFrame, width = "20",
            textvariable = inputVariable)

  fcastFrame <- tkframe(dataFrame)
  fcastVariable <- tclVar(dialog.values$initial.fcast)
  fcastField <- ttkentry(fcastFrame, width = "20",
            textvariable = fcastVariable)

  outFrame <- tkframe(dataFrame)
  outVariable <- tclVar(dialog.values$initial.out)
  outField <- ttkentry(outFrame, width = "20",
            textvariable = outVariable)

  radioButtons(leftFrame, name = "type", buttons = c("functions",
      "time"), values = c("functions", "time"),
      labels = gettextRcmdr(c("Age", "Time")),
      title = gettextRcmdr("As function of     "),
      initialValue = dialog.values$initial.type)

  radioButtons(leftFrame, name = "trans", buttons = c("yes",
      "no"), values = c("TRUE", "FALSE"),
      labels = gettextRcmdr(c("Yes", "No")),
      title = gettextRcmdr("Take logarithm ?     "),
      initialValue = dialog.values$initial.trans)



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

  tkgrid(labelRcmdr(dataFrame, text = "Forecast object",fg="blue"), sticky = "w",
           padx=c(10, 0),pady=c(5,0))
  tkgrid(labelRcmdr(fcastFrame, text = gettextRcmdr("Name: ")),
         fcastField, sticky = "w", padx=c(10, 0))
  tkgrid(fcastFrame, sticky = "w")

  tkgrid(labelRcmdr(dataFrame, text = "Output object",fg="blue"), sticky = "w",
           padx=c(10, 0),pady=c(5,0))
  tkgrid(labelRcmdr(outFrame, text = gettextRcmdr("Name: ")),
         outField, sticky = "w", padx=c(10, 0))
  tkgrid(outFrame, sticky = "w")

  tkgrid(typeFrame, sticky = "w")
  tkgrid(transFrame, sticky = "w")

  tkgrid(labelRcmdr(rightFrame, text = "Subset",fg="blue"), sticky = "w", padx=c(0, 0))
  tkgrid(labelRcmdr(agesFrame, text = gettextRcmdr("Ages: ")),
         agesField, sticky = "w", padx=c(0, 0))
  tkgrid(agesFrame, sticky = "w")

  tkgrid(labelRcmdr(yearsFrame, text = gettextRcmdr("Years: ")),
         yearsField, sticky = "w", padx=c(0, 0))
  tkgrid(yearsFrame, sticky = "w")


  tkgrid(dataFrame, leftFrame,rightFrame, sticky = "nw")

  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  dialogSuffix()
}

incidenceDialogFin <-
function () {
    defaults <- list (initial.year1 = 1, initial.month1 = 5, initial.day1 = 0,
                      initial.year2 = 0, initial.month2 = NULL, initial.day2 = NULL, 
                      initial.x = 0, initial.weeks = "2", initial.log = "TRUE")
    dialog.values <- getDialog ("incidenceDialogFin", defaults)
    initializeDialog(title = gettextRcmdr("Plotting incidence curves with Finnish data"))
    
    weeksVariable <- tclVar(dialog.values$initial.weeks)
    logVariable <- tclVar(dialog.values$initial.log)
    
    years <- c("None",2020:2024)
    months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    days <- 1:31
    
    utils::data(popRegionsFin)
    utils::data(dataCovidFin)
    regions <- c("All regions",popRegionsFin$Alue[1:21])
    mainFrame <- tkframe(top)
    regionFrame <- tkframe(mainFrame)
    
    xBox <- variableListBox(regionFrame, regions, title = gettextRcmdr("Region (pick one or more)"),initialSelection =  dialog.values$initial.x,
                            selectmode="multiple")
    
    optionsFrame <- tkframe(mainFrame)
    dataFrame <- tkframe(top)
    
    year1Box <- variableListBox(dataFrame, years,
                                title = gettextRcmdr("Year"),initialSelection =  dialog.values$initial.year1)
    month1Box <- variableListBox(dataFrame, months,
                                 title = gettextRcmdr("Month"),
                                 initialSelection = dialog.values$initial.month1)
    day1Box <- variableListBox(dataFrame, days,
                               title = gettextRcmdr("Day"),
                               initialSelection = dialog.values$initial.day1)
    
    year2Box <- variableListBox(dataFrame, years,
                                title = gettextRcmdr("Year"),initialSelection =  dialog.values$initial.year2)
    month2Box <- variableListBox(dataFrame, months,
                                 title = gettextRcmdr("Month"),
                                 initialSelection = dialog.values$initial.month2)
    day2Box <- variableListBox(dataFrame, days,
                               title = gettextRcmdr("Day"),
                               initialSelection = dialog.values$initial.day2)
    
    onOK <- function() {
      #Get the list of countries
      x <- getSelection(xBox)
      xc <- paste0("c('",paste0(x,collapse="','"),"')")
      nRegions <- length(x)
      if (nRegions == 0) {
        errorCondition(recall = incidenceDialogFin, message = gettextRcmdr("You must select at least one region."))
        return()
      }
      includeAllRegions <- ifelse(x[1]=="All regions","TRUE","FALSE")
      xi <- numeric(nRegions)
      for(i in 1:nRegions) xi[i] <- which(x[i]==regions)
      
      
      year1 <- getSelection(year1Box)
      year1.i <- (0:12)[year1==years]
      month1 <- (1:12)[getSelection(month1Box)==months]
      month1.i <- month1-1
      day1 <- getSelection(day1Box)
      day1.i <- day1-1
      
      year2 <- getSelection(year2Box)
      year2.i <- (0:12)[year2==years]
      month2 <- (1:12)[getSelection(month2Box)==months]
      month2.i <- month2-1
      day2 <- getSelection(day2Box)
      day2.i <- day2-1
      
      date1 <- paste(year1,month1,day1,sep="-")
      date2 <- paste(year2,month2,day2,sep="-")
      
      weeks <- tclvalue(weeksVariable)
      log <- tclvalue(logVariable)
      
      putDialog("incidenceDialogFin",list (initial.year1 = year1.i, initial.month1 = month1.i,
                                         initial.day1 = day1.i, initial.year2 = year2.i, initial.month2 = month2.i,
                                         initial.day2 = day2.i, initial.x = xi-1,initial.weeks = weeks,
                                         initial.log = log))
      closeDialog()
      
      if(year1!="None" && year2!="None"){
        doItAndPrint(paste0("drawIncidenceFin(data=dataCovidFin, pop=popRegionsFin, regions=",xc,",start='",date1,"',end='",date2,"',weeks=",weeks,
                            ",log=",log,",includeAllRegions=",includeAllRegions,")"))}else
                              if (year1!="None"){
                                doItAndPrint(paste0("drawIncidenceFin(data=dataCovidFin, pop=popRegionsFin, regions=",xc,",start='",date1,"',weeks=",weeks,",log=",log,
                                                    ",includeAllRegions=",includeAllRegions,")"))}else
                              if (year2!="None"){
                                                        doItAndPrint(paste0("drawIncidenceFin(data=dataCovidFin, pop=popRegionsFin, regions=",xc,",start=NULL, end='",date2,"',weeks=",weeks,",log=",log,
                                                                            ",includeAllRegions=",includeAllRegions,")"))}else                        
                                {
                                  doItAndPrint(paste0("drawIncidenceFin(data=dataCovidFin, pop=popRegionsFin, regions=",xc,",start=NULL, weeks=",weeks,", log=",log,", includeAllRegions=",includeAllRegions,")"))
                                }
      
      tkdestroy(top)
      tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = "drawIncidenceFin", reset = "incidenceDialogFin", apply = "incidenceDialogFin")
    
    weeksFrame <- tkframe(optionsFrame)
    logFrame <- tkframe(optionsFrame)
    radioButtons(optionsFrame, name = "weeks", buttons = c("oneweek", 
                                                           "twoweeks"), values = c("1","2"), 
                 labels = gettextRcmdr(c("1 week", "2 weeks")), 
                 title = gettextRcmdr("Lag    "),
                 initialValue = dialog.values$initial.weeks)
    radioButtons(optionsFrame, name = "log", buttons = c("yes", 
                                                         "no"), values = c("TRUE", "FALSE"), 
                 labels = gettextRcmdr(c("Yes", "No")), 
                 title = gettextRcmdr("Take logarithm?"),
                 initialValue = dialog.values$initial.log)
    
    tkgrid(getFrame(xBox), sticky = "nw")
    #tkgrid(mainFrame, sticky="w",pady=c(0,10))
    tkgrid(weeksFrame, padx=c(20,0),pady=c(0,10),sticky="w")
    tkgrid(logFrame, padx=c(20,0),sticky="w")
    
    tkgrid(regionFrame, optionsFrame)
    tkgrid(mainFrame, sticky = "w")
    
    tkgrid(labelRcmdr(dataFrame, text="First date",fg="red"),sticky="w")
    tkgrid(getFrame(year1Box), getFrame(month1Box),
           getFrame(day1Box), sticky = "nw",pady=c(0,10))
    
    tkgrid(labelRcmdr(dataFrame, text="Second date (optional)",fg="red"),sticky="w")
    tkgrid(getFrame(year2Box), getFrame(month2Box),
           getFrame(day2Box), sticky = "nw")
    tkgrid(dataFrame, sticky="w")
    
    tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
    #tkgrid.configure(volPortfField, sticky = "e")
    dialogSuffix()
  }

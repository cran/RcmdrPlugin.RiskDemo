forecastDialogFin <-
function () {
    defaults <- list (initial.year1 = 1, initial.month1 = 5, initial.day1 = 0,
                      initial.year2 = 0, initial.month2 = NULL, initial.day2 = NULL, 
                      initial.x = 0, initial.R = TRUE, initial.forecast = TRUE,initial.log="TRUE",
                      initial.interval=95, initial.horizon = 30)
    dialog.values <- getDialog ("forecastDialogFin", defaults)
    initializeDialog(title = gettextRcmdr("Forecasting covid cases with Finnish data"))
    
    RVariable <- tclVar(dialog.values$initial.R)
    forecastVariable <- tclVar(dialog.values$initial.forecast)
    logVariable <- tclVar(dialog.values$initial.log)
    intervalVariable <- tclVar(dialog.values$initial.interval)
    horizonVariable <- tclVar(dialog.values$initial.horizon)
    
    years <- c("None",2020:2022)
    months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    days <- 1:31
    
    utils::data(popRegionsFin)
    utils::data(dataCovidFin)
    regions <- c("All regions",popRegionsFin$Alue[1:21])
    mainFrame <- tkframe(top)
    regionFrame <- tkframe(mainFrame)
    
    xBox <- variableListBox(regionFrame, regions, title = gettextRcmdr("Region (pick one)"),initialSelection =  dialog.values$initial.x)
    
    optionsFrame <- tkframe(mainFrame)
    #rightFrame <- tkframe(mainFrame)
    
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
      x.i <- (0:21)[x==regions]
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
      
      R <- as.logical(as.numeric(tclvalue(RVariable)))
      forecast <- as.logical(as.numeric(tclvalue(forecastVariable)))                
      log <- tclvalue(logVariable)
      interval <- tclvalue(intervalVariable)
      horizon <- tclvalue(horizonVariable)
      
      putDialog("forecastDialogFin",list (initial.year1 = year1.i, initial.month1 = month1.i,
                                           initial.day1 = day1.i, initial.year2 = year2.i, initial.month2 = month2.i,
                                           initial.day2 = day2.i, initial.x = x.i,initial.R = R,
                                           initial.forecast = forecast,initial.log=log,
                                          initial.interval=interval,initial.horizon=horizon))
      closeDialog()
      
      if(forecast){
        if(year1!="None" && year2!="None"){
          doItAndPrint(paste0("plotForecast(data=dataCovidFin, region='",x,"',start='",date1,"',end='",date2,
                            "',np=",horizon,",predInt=",interval,"/100,log=",log,")"))
        }else if (year1!="None"){
          doItAndPrint(paste0("plotForecast(data=dataCovidFin, region='",x,"',start='",date1,
                              "',np=",horizon,",predInt=",interval,"/100,log=",log,")"))
        }else if (year2!="None"){
            doItAndPrint(paste0("plotForecast(data=dataCovidFin, region='",x,"',start=NULL, end='",date2,
                                "',np=",horizon,",predInt=",interval,"/100,log=",log,")"))
        }else{
          doItAndPrint(paste0("plotForecast(data=dataCovidFin, region='",x,
                              "', start=NULL, np=",horizon,",predInt=",interval,"/100,log=",log,")"))
        }
      }  
      if(R){
        if(forecast) dev.new()  
        if(year1!="None" && year2!="None"){
          doItAndPrint(paste0("plotR(data=dataCovidFin, region='",x,"',start='",date1,"',end='",date2,
                              "',confInt=",interval,"/100)"))
        }else if (year1!="None"){
          doItAndPrint(paste0("plotR(data=dataCovidFin, region='",x,"',start='",date1,
                              "',confInt=",interval,"/100)"))
        }else if (year2!="None"){
          doItAndPrint(paste0("plotR(data=dataCovidFin, region='",x,"',start=NULL, end='",date2,
                              "',confInt=",interval,"/100)"))
        }else{
          doItAndPrint(paste0("plotR(data=dataCovidFin, region='",x,"',start=NULL, confInt=",interval,"/100)"))
        }
      }
      tkdestroy(top)
      tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = "plotForecast", reset = "forecastDialogFin", apply = "forecastDialogFin")
    
    RFrame <- tkframe(optionsFrame)
    checkBoxes(window=optionsFrame,frame="RFrame", boxes=c("R","forecast"),
               initialValues=c(dialog.values$initial.R,dialog.values$initial.forecast),
               labels=gettextRcmdr(c("Effective reproduction number (R)","Forecast")),title="Options")
    
    logFrame <- tkframe(optionsFrame)
    radioButtons(optionsFrame, name = "log", buttons = c("yes", 
                                                         "no"), values = c("TRUE", "FALSE"), 
                 labels = gettextRcmdr(c("Yes", "No")), 
                 title = gettextRcmdr("Logarithmize forecast?"),
                 initialValue = dialog.values$initial.log)
    
    rightFrame <- tkframe(optionsFrame)
    intervalFrame <- tkframe(rightFrame)
    intervalVariable <- tclVar(dialog.values$initial.interval)
    intervalField <- ttkentry(intervalFrame,width="6",textvariable=intervalVariable)
    
    horizonFrame <- tkframe(rightFrame)
    horizonVariable <- tclVar(dialog.values$initial.horizon)
    horizonField <- ttkentry(horizonFrame,width="6",textvariable=horizonVariable)
    
    tkgrid(getFrame(xBox), sticky = "nw")
    #tkgrid(mainFrame, sticky="w",pady=c(0,10))
    tkgrid(RFrame, padx=c(20,0),pady=c(0,10),sticky="w")
    tkgrid(rightFrame, padx=c(20,0),pady=c(0,10),sticky="w")
    tkgrid(logFrame, padx=c(20,0),sticky="w")
    
    
    tkgrid(labelRcmdr(intervalFrame, text = gettextRcmdr("Confidence interval (%)")), 
           intervalField, sticky = "e", padx=c(10, 10))
    tkgrid(intervalFrame, sticky = "w")
    
    tkgrid(labelRcmdr(horizonFrame, text = gettextRcmdr("Forecast horizon (days)")), 
           horizonField, sticky = "e", padx=c(10, 14))
    tkgrid(horizonFrame, sticky = "w")
    
    
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

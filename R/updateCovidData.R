updateCovidData <-
function () {
    defaults <- list (initial.intname = 'dataCovid', initial.finname = 'dataCovidFin', initial.popname = 'popRegionsFin', initial.int = TRUE, initial.fin = TRUE, 
                      initial.pop = TRUE)
    dialog.values <- getDialog ("updateCovidData", defaults)  
    initializeDialog(title = gettextRcmdr("Updating COVID-19 data"))
    
    intnameVariable <- tclVar(dialog.values$initial.intname)
    finnameVariable <- tclVar(dialog.values$initial.finname)
    popnameVariable <- tclVar(dialog.values$initial.popname)
    intVariable <- tclVar(dialog.values$initial.int)
    finVariable <- tclVar(dialog.values$initial.fin)
    popVariable <- tclVar(dialog.values$initial.pop)
    
    
    checkBoxFrame <- tkframe(top)
    nameFrame <- tkframe(top)
    
    checkBoxes(frame="checkBoxFrame", boxes=c("int","fin","pop"), 
               initialValues=c(dialog.values$initial.int, dialog.values$initial.fin, 
                               dialog.values$initial.pop), labels=gettextRcmdr(c("International data", "Finnish data","Finnish population data")))
    
    onOK <- function() {
      
      
      intname <- tclvalue(intnameVariable)
      finname <- tclvalue(finnameVariable)
      popname <- tclvalue(popnameVariable)
      int <- tclvalue(intVariable)
      fin <- tclvalue(finVariable)
      pop <- tclvalue(popVariable)
      
      
      putDialog ("updateCovidData",list (initial.intname = intname, initial.finname = finname, initial.popname = popname, initial.int = int, initial.fin = fin, 
                                       initial.pop = pop))
      if(int=="1"){
        doItAndPrint(paste0(intname,' <- data.table::fread("https://covid.ourworldindata.org/data/owid-covid-data.csv")'))
        doItAndPrint(paste0(intname,'$date <- as.Date(',intname,'$date)'))
      }
      if(fin=="1"){
        doItAndPrint(paste0(finname,' <- read.csv2("https://sampo.thl.fi/pivot/prod/fi/epirapo/covid19case/fact_epirapo_covid19case.csv?row=hcdmunicipality2020-445222&column=dateweek20200101-508804L")'))
        doItAndPrint(paste0(finname,'$Aika <- as.Date(',finname,'$Aika)'))                 
      } 
      if(pop=="1")
           doItAndPrint(paste0(popname,' <- read.csv2("https://sampo.thl.fi/pivot/prod/fi/epirapo/covid19case/fact_epirapo_covid19case.csv?row=hcdmunicipality2020-445222&column=measure-445344")'))

      closeDialog()
      tkdestroy(top)
      tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = "updateCovidData", reset = "updateCovidData", apply = "updateCovidData")
    
    intnameFrame <- tkframe(nameFrame)
    finnameFrame <- tkframe(nameFrame)
    popnameFrame <- tkframe(nameFrame)
    
    intnameVariable <- tclVar(dialog.values$initial.intname)
    intnameField <- ttkentry(intnameFrame,width="20",textvariable=intnameVariable)
    finnameVariable <- tclVar(dialog.values$initial.finname)
    finnameField <- ttkentry(finnameFrame,width="20",textvariable=finnameVariable)
    popnameVariable <- tclVar(dialog.values$initial.popname)
    popnameField <- ttkentry(popnameFrame,width="20",textvariable=popnameVariable)
    
    tkgrid(labelRcmdr(intnameFrame, text = gettextRcmdr("Name of international data:            ")), 
           intnameField, sticky = "w")
    tkgrid(labelRcmdr(finnameFrame, text = gettextRcmdr("Name of Finnish data:                         ")), 
           finnameField, sticky = "w")
    tkgrid(labelRcmdr(popnameFrame, text = gettextRcmdr("Name of Finnish population data: ")), 
           popnameField, sticky = "w")
    
    tkgrid(intnameFrame, sticky = "w")
    tkgrid(finnameFrame, sticky = "w")
    tkgrid(popnameFrame, sticky = "w")
    
    tkgrid(checkBoxFrame,nameFrame,sticky="w")
    
    tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
    #tkgrid.configure(timeField, sticky = "e")
    dialogSuffix()
  }

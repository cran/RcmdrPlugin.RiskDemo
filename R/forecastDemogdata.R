forecastDemogdata <-
function () {
  defaults <- list (initial.input = 'fin.lca', initial.out="fin.fcast",
           initial.jumpchoice = "fit", 
           initial.h = 50,initial.level = 80,initial.func=0,
           initial.comp=0)
  dialog.values <- getDialog ("forecastDemogdata", defaults)  
  initializeDialog(title = gettextRcmdr("Forecast demographic data"))
  #xBox <- variableListBox(top, Numeric(), title = gettextRcmdr("Variable (pick one)"),initialSelection = varPosn(dialog.values$initial.x, "numeric"))
  inputVariable <- tclVar(dialog.values$initial.input)
  outVariable <- tclVar(dialog.values$initial.out)
  jumpchoiceVariable <- tclVar(dialog.values$initial.jumpchoice)
  hVariable <- tclVar(dialog.values$initial.h)
  levelVariable <- tclVar(dialog.values$initial.level)
  funcVariable <- tclVar(dialog.values$initial.func)
  compVariable <- tclVar(dialog.values$initial.comp)
  
  
  onOK <- function() {
    input <- tclvalue(inputVariable)
    out <- tclvalue(outVariable)
    jumpchoice <- tclvalue(jumpchoiceVariable) 
    h <- tclvalue(hVariable)
    level <- tclvalue(levelVariable) 
    func <- tclvalue(funcVariable)
    comp <- tclvalue(compVariable)
    
    putDialog ("forecastDemogdata", list (initial.input=input, initial.out= out, 
    initial.jumpchoice = jumpchoice, initial.h = h, initial.level = level, initial.func=func,initial.comp=comp))
    closeDialog()
    
    doItAndPrint('data(fin.lca)')
    doItAndPrint(paste(out,' <- forecast(',input,',h=',h,',jumpchoice="',jumpchoice,
    '",level=',level,')',sep=""))
    
    if(func==1){
      doItAndPrint(paste('plot(',out,')',sep=""))
    }
    if(comp==1){
      if(func==1){
         doItAndPrint("dev.new()")
      }
      doItAndPrint(paste('plot(',out,',plot.type="component")',sep=""))
    } 
       
    tkdestroy(top)
    tkfocus(CommanderWindow())
  
  }
  OKCancelHelp(helpSubject = "forecast.lca", reset = "forecastDemogdata", 
          apply = "forecastDemogdata")
 
  dataFrame <- tkframe(top)
  jumpchoiceFrame <- tkframe(top)
  rightFrame <- tkframe(top)
  checkboxFrame <- tkframe(rightFrame)
 
  checkBoxes(window=rightFrame,frame="checkboxFrame", boxes=c("func","comp"), 
             initialValues=c(dialog.values$initial.func,dialog.values$initial.comp),  
             labels=gettextRcmdr(c("Plot function?","Plot components?")))
 
  inputFrame <- tkframe(dataFrame)
  inputVariable <- tclVar(dialog.values$initial.input)
  inputField <- ttkentry(inputFrame, width = "20", 
            textvariable = inputVariable)
    
  outFrame <- tkframe(dataFrame)
  outVariable <- tclVar(dialog.values$initial.out)
  outField <- ttkentry(outFrame, width = "20", 
            textvariable = outVariable)
           
  radioButtons(top, name = "jumpchoice", buttons = c("fit", 
      "actual"), values = c("fit", "actual"), 
      labels = gettextRcmdr(c("Fit", "Actual")), 
      title = gettextRcmdr("Jump method"),
      initialValue = dialog.values$initial.jumpchoice)
 
  
  hFrame <- tkframe(rightFrame)
  hVariable <- tclVar(dialog.values$initial.h)
  hField <- ttkentry(hFrame, width = "10", 
                              textvariable = hVariable)
                              
  levelFrame <- tkframe(rightFrame)
  levelVariable <- tclVar(dialog.values$initial.level)
  levelField <- ttkentry(levelFrame, width = "10", 
            textvariable = levelVariable)
  
  
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
  
  tkgrid(labelRcmdr(rightFrame, text = "Options",fg="blue"), sticky = "w")
  tkgrid(labelRcmdr(hFrame, text = gettextRcmdr("Forecast horizon:                ")), 
         hField, sticky = "w", padx=c(0, 0))
  tkgrid(hFrame, sticky = "w")
  
  tkgrid(labelRcmdr(levelFrame, text = gettextRcmdr("Forecast interval level (%): ")), 
         levelField, sticky = "w", pady=c(0, 10))
  tkgrid(levelFrame, sticky = "w")
  
  tkgrid(labelRcmdr(rightFrame, text = "Plots",fg="blue"), sticky = "w")
  tkgrid(checkboxFrame, sticky = "w")
  
  tkgrid(dataFrame, jumpchoiceFrame,rightFrame, sticky = "nw")
  

  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  dialogSuffix()
}

loadStockData <-
function () {
  defaults <- list (initial.data = 'sData', initial.largecap = TRUE, initial.midcap = FALSE, 
                    initial.smallcap = FALSE)
  dialog.values <- getDialog ("loadStockData", defaults)  
  initializeDialog(title = gettextRcmdr("Loading stock data"))
  
  dataVariable <- tclVar(dialog.values$initial.data)
  largecapVariable <- tclVar(dialog.values$initial.largecap)
  midcapVariable <- tclVar(dialog.values$initial.midcap)
  smallcapVariable <- tclVar(dialog.values$initial.smallcap)
  
  markets <- "XHEL"
  lists <- character()
  
  
  dataFrame <- tkframe(top)
  checkBoxFrameLeft <- tkframe(top)
  
  checkBoxes(frame="checkBoxFrameLeft", boxes=c("largecap","midcap","smallcap"), 
             initialValues=c(dialog.values$initial.largecap, dialog.values$initial.midcap, 
                             dialog.values$initial.smallcap), labels=gettextRcmdr(c("Large", "Middle","Small")))
  
  
  
  onOK <- function() {
    data <- tclvalue(dataVariable)
    largecap <- tclvalue(largecapVariable)
    midcap <- tclvalue(midcapVariable)
    smallcap <- tclvalue(smallcapVariable)
    
    if (largecap == "1")
      lists <- c(lists,"largecap")  
    if (midcap == "1")
      lists <- c(lists,"midcap")  
    if (smallcap == "1")
      lists <- c(lists,"smallcap") 
    
    
    putDialog ("loadStockData",list (initial.data = data,initial.largecap = largecap,
                                     initial.midcap=midcap, initial.smallcap=smallcap))
    
    doItAndPrint(paste("lst <- getStockList(markets=c('",paste(markets,collapse=','),
                       "'),listIds=c('",paste(lists,collapse=','), "'))\n",data,"<- readStockData(lst)", 
                       sep = ""))
    closeDialog()
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "getStockList", reset = "loadStockData", apply = "loadStockData")
  dataFrame <- tkframe(top)
  dataVariable <- tclVar(dialog.values$initial.data)
  dataField <- ttkentry(dataFrame,width="20",textvariable=dataVariable)
  
  tkgrid(labelRcmdr(dataFrame, text = gettextRcmdr("Name of loaded data: ")), 
         dataField, sticky = "e")
  tkgrid(dataFrame, sticky = "w")
  tkgrid(checkBoxFrameLeft, sticky="w")
  
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  #tkgrid.configure(timeField, sticky = "e")
  dialogSuffix()
}

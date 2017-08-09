bondDialog <-
function () {
  defaults <- list (initial.year1 = 1, initial.month1 = 8, initial.day1 = 5,
                    initial.year2 = 0, initial.month2 = NULL, initial.day2 = NULL,
                    initial.yield = 1,initial.forward=1,initial.AAA=1,initial.all=0)
  dialog.values <- getDialog ("bondDialog", defaults)
  initializeDialog(title = gettextRcmdr("Drawing a yield curve"))
  
  yieldVariable <- tclVar(dialog.values$initial.yield)
  forwardVariable <- tclVar(dialog.values$initial.forward)
  AAAVariable <- tclVar(dialog.values$initial.AAA)
  allVariable <- tclVar(dialog.values$initial.all)

  years <- c("None",2004:2017)
  months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  days <- 1:31
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

    yield <- as.logical(as.numeric(tclvalue(yieldVariable)))
    forward <- as.logical(as.numeric(tclvalue(forwardVariable)))
    AAA <- as.logical(as.numeric(tclvalue(AAAVariable)))
    all <- as.logical(as.numeric(tclvalue(allVariable)))


    date1 <- paste(year1,month1,day1,sep="-")
    date2 <- paste(year2,month2,day2,sep="-")

    putDialog ("bondDialog",list (initial.year1 = year1.i, initial.month1 = month1.i,
                    initial.day1 = day1.i, initial.year2 = year2.i, initial.month2 = month2.i,
                    initial.day2 = day2.i,initial.yield = yield,initial.forward=forward,
                    initial.AAA=AAA,initial.all=all))
    closeDialog()
    doItAndPrint('data(params)')
    if(year1!="None" && year2!="None"){
       doItAndPrint(paste("bondCurve(date1='",date1,"',date2='",date2,"',yield=",yield,",forward=",
       forward,",AAA=",AAA,",all=",all,",params)", sep = ""))}else
    if (year1!="None"){
       doItAndPrint(paste("bondCurve(date1='",date1,"',date2=NULL,yield=",yield,",forward=",
       forward,",AAA=",AAA,",all=",all,",params)", sep = ""))
      }
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "bondCurve", reset = "bondDialog", apply = "bondDialog")
  mainFrame <- tkframe(top)
  leftFrame <- tkframe(mainFrame)
  rightFrame <- tkframe(mainFrame)


  checkBoxes(frame="leftFrame", boxes=c("yield","forward"),
             initialValues=c(dialog.values$initial.yield, dialog.values$initial.forward),
             labels=gettextRcmdr(c("Yield curve ?", "Forward curve ?")))

  checkBoxes(frame="rightFrame", boxes=c("AAA","all"),
             initialValues=c(dialog.values$initial.AAA, dialog.values$initial.all),
             labels=gettextRcmdr(c("AAA bonds ?", "All bonds ?")))

  tkgrid(labelRcmdr(dataFrame, text="First date",fg="red"),sticky="w")
  tkgrid(getFrame(year1Box), getFrame(month1Box),
           getFrame(day1Box), sticky = "nw",pady=c(0,10))

  tkgrid(labelRcmdr(dataFrame, text="Second date (optional)",fg="red"),sticky="w")
  tkgrid(getFrame(year2Box), getFrame(month2Box),
           getFrame(day2Box), sticky = "nw")

  tkgrid(dataFrame, sticky="w")

  tkgrid(leftFrame, rightFrame, sticky = "nw")

  tkgrid(mainFrame, sticky="w")
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  #tkgrid.configure(volPortfField, sticky = "e")
  dialogSuffix()
}

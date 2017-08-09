bondCalc <-
function () {
  defaults <- list (initial.year1 = 0, initial.month1 = 0, initial.day1 = 0,
                    initial.year2 = NULL, initial.month2 = NULL, initial.day2 = NULL,
                    initial.rate = 2.25,initial.n=2,initial.yield=0.79,initial.price=100)
  dialog.values <- getDialog ("bondCalc", defaults)
  initializeDialog(title = gettextRcmdr("Calculating bond yield or price"))

  years <- c(2000:2050)
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
    year1.i <- (0:50)[year1==years]
    month1 <- (1:12)[getSelection(month1Box)==months]
    month1.i <- month1-1
    day1 <- getSelection(day1Box)
    day1.i <- day1-1

    year2 <- getSelection(year2Box)
    year2.i <- (0:50)[year2==years]
    month2 <- (1:12)[getSelection(month2Box)==months]
    month2.i <- month2-1
    day2 <- getSelection(day2Box)
    day2.i <- day2-1

    rate <- tclvalue(rateVariable)
    yield <-  tclvalue(yieldVariable)
    rateN <- as.numeric(rate)/100
    yieldN <- as.numeric(yield)/100
    price <- tclvalue(priceVariable)
    n <- tclvalue(nVariable)


    date1 <- paste(year1,month1,day1,sep="-")
    date2 <- paste(year2,month2,day2,sep="-")

    putDialog ("bondCalc",list (initial.year1 = year1.i, initial.month1 = month1.i,
                    initial.day1 = day1.i, initial.year2 = year2.i, initial.month2 = month2.i,
                    initial.day2 = day2.i,initial.rate = rate,initial.n=n,initial.yield=yield,
                    initial.price=price))
    closeDialog()

    if(!yield==""){
       doItAndPrint(paste("bondPrice(buyDate='",date1,"',matDate='",date2,"',rateCoupon=",
                 rateN,",yieldToMat=",yieldN,",nPay=",n,")",sep=""))
       doItAndPrint(paste("bondFigure(buyDate='",date1,"',matDate='",date2,"',rateCoupon=",
                  rateN,",yieldToMat=",yieldN,",nPay=",n,")",sep=""))
                 }else
    if(!price==""){
      doItAndPrint(paste("solveYield(buyDate='",date1,"',matDate='",date2,"',rateCoupon=",rateN,",
                 bondPr=",price,",nPay=",n,")",sep=""))
      doItAndPrint(paste("bondFigure(buyDate='",date1,"',matDate='",date2,"',rateCoupon=",
                  rateN,",bondPr=",price,",nPay=",n,")",sep=""))
                 }
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "bondCalc", reset = "bondCalc", apply = "bondCalc")
  mainFrame <- tkframe(top)

  tkgrid(labelRcmdr(dataFrame, text="Settlement date",fg="red"),sticky="w")
  tkgrid(getFrame(year1Box), getFrame(month1Box),
           getFrame(day1Box), sticky = "nw",pady=c(0,10))

  tkgrid(labelRcmdr(dataFrame, text="Maturity date",fg="red"),sticky="w")
  tkgrid(getFrame(year2Box), getFrame(month2Box),
           getFrame(day2Box), sticky = "nw",pady=c(0,10))

  tkgrid(dataFrame, sticky="w")

  rateFrame <- tkframe(mainFrame)
  rateVariable <- tclVar(dialog.values$initial.rate)
  rateField <- ttkentry(rateFrame,width="6",textvariable=rateVariable)

  nFrame <- tkframe(mainFrame)
  nVariable <- tclVar(dialog.values$initial.n)
  nField <- ttkentry(nFrame,width="6",textvariable=nVariable)

  yieldFrame <- tkframe(mainFrame)
  yieldVariable <- tclVar(dialog.values$initial.yield)
  yieldField <- ttkentry(yieldFrame,width="6",textvariable=yieldVariable)

  priceFrame <- tkframe(mainFrame)
  priceVariable <- tclVar(dialog.values$initial.price)
  priceField <- ttkentry(priceFrame,width="6",textvariable=priceVariable)

  tkgrid(labelRcmdr(rateFrame, text = gettextRcmdr("Annual coupon rate (%)")),
         rateField, sticky = "e", padx=c(10, 20))
  tkgrid(rateFrame, sticky = "w")

  tkgrid(labelRcmdr(nFrame, text = gettextRcmdr("Coupon payments per year")),
         nField, sticky = "e", padx=c(10, 0))
  tkgrid(nFrame, sticky = "w")



  tkgrid(labelRcmdr(mainFrame, text="Fill either yield to maturity or flat price",
           fg="red"),sticky="w",padx=c(10,10))

  tkgrid(labelRcmdr(yieldFrame, text = gettextRcmdr("Yield to maturity (%)     ")),
         yieldField, sticky = "e", padx=c(10, 20))
  tkgrid(yieldFrame, sticky = "w")

  tkgrid(labelRcmdr(priceFrame, text = gettextRcmdr("Flat price (% of par)      ")),
         priceField, sticky = "e", padx=c(10, 20))
  tkgrid(priceFrame, sticky = "w")

  tkgrid(mainFrame, sticky="w")
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  #tkgrid.configure(volPortfField, sticky = "e")
  dialogSuffix()
}

portfolioOpt <-
function () {
  defaults <- list (initial.yield = NULL, initial.vol = NULL, initial.beta = NULL, 
                    initial.volPortf = 10,initial.interval=12,initial.number=5,initial.amount=100,
                    initial.riskfree=0,initial.interest=1,initial.bor=0,initial.A=10)
  dialog.values <- getDialog ("portfolioOpt", defaults)  
  initializeDialog(title = gettextRcmdr("Determining optimal portfolio"))
  
  volPortfVariable <- tclVar(dialog.values$initial.volPortf)
  intervalVariable <- tclVar(dialog.values$initial.interval)
  numberVariable <- tclVar(dialog.values$initial.number)
  amountVariable <- tclVar(dialog.values$initial.amount)
  riskfreeVariable <- tclVar(dialog.values$initial.riskfree)
  borVariable <- tclVar(dialog.values$initial.bor)
  interestVariable <- tclVar(dialog.values$initial.interest)
  AVariable <- tclVar(dialog.values$initial.A)
  
  dataFrame <- tkframe(top)
  yieldBox <- variableListBox(dataFrame, Numeric(), 
        title = gettextRcmdr("Expected yield variable(%)"),
        initialSelection = varPosn(dialog.values$initial.yield, "numeric"))
  volBox <- variableListBox(dataFrame, Numeric(), 
         title = gettextRcmdr("Volatility variable (%)"),
         initialSelection = varPosn(dialog.values$initial.vol, "numeric"))
  betaBox <- variableListBox(dataFrame, Numeric(), 
         title = gettextRcmdr("Beta variable (%)"),
         initialSelection = varPosn(dialog.values$initial.beta, "numeric"))
 
  
  
  onOK <- function() {
    yield <- getSelection(yieldBox)
    vol <- getSelection(volBox)
    beta <- getSelection(betaBox)
    volPortf <- tclvalue(volPortfVariable)
    interval <- tclvalue(intervalVariable)
    number <- tclvalue(numberVariable)
    amount <- tclvalue(amountVariable)
    riskfree <- tclvalue(riskfreeVariable)
    bor <- tclvalue(borVariable)
    interest <- tclvalue(interestVariable)
    A <- tclvalue(AVariable)
    
    putDialog ("portfolioOpt",list (initial.yield = yield, initial.vol = vol,initial.beta = beta, 
                    initial.volPortf = volPortf,initial.interval=interval,initial.number=number,
                    initial.amount=amount,initial.riskfree=riskfree,
                    initial.interest=interest,initial.bor=bor,initial.A=A))
    closeDialog()
    doItAndPrint(paste("with(",ActiveDataSet(),",drawFigure(symbol=rownames(",ActiveDataSet(),
          "),yield=",yield,
          ",vol=",vol,",beta=",beta,",r=",interest,",total=",amount,
          ",indexVol=",volPortf,",nStocks=",number,",balanceInt=",interval,",A=",A,
          ",riskfree=",riskfree,",bor=",bor,"))", sep = ""))
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "portfOptim", reset = "portfolioOpt", apply = "portfolioOpt")
  mainFrame <- tkframe(top)
  leftFrame <- tkframe(mainFrame)
  rightFrame <- tkframe(mainFrame)
  
  checkBoxes(frame="rightFrame", boxes=c("riskfree","bor"), 
             initialValues=c(dialog.values$initial.riskfree, dialog.values$initial.bor), 
             labels=gettextRcmdr(c("Include risk-free investment ?", "Allow borrowing ?")))
  #checkBoxes(frame="rightFrame", boxes=c("riskfree"), 
  #           initialValues=c(dialog.values$initial.riskfree), 
  #           labels=gettextRcmdr(c("Include risk-free investment ?")))
  
  
  volPortfFrame <- tkframe(leftFrame)
  volPortfVariable <- tclVar(dialog.values$initial.volPortf)
  volPortfField <- ttkentry(volPortfFrame,width="6",textvariable=volPortfVariable)
  
  intervalFrame <- tkframe(leftFrame)
  intervalVariable <- tclVar(dialog.values$initial.interval)
  intervalField <- ttkentry(intervalFrame,width="6",textvariable=intervalVariable)
  
  numberFrame <- tkframe(leftFrame)
  numberVariable <- tclVar(dialog.values$initial.number)
  numberField <- ttkentry(numberFrame,width="6",textvariable=numberVariable)
  
  amountFrame <- tkframe(leftFrame)
  amountVariable <- tclVar(dialog.values$initial.amount)
  amountField <- ttkentry(amountFrame,width="6",textvariable=amountVariable)
  
  interestFrame <- tkframe(rightFrame)
  interestVariable <- tclVar(dialog.values$initial.interest)
  interestField <- ttkentry(interestFrame,width="6",textvariable=interestVariable)
  
  AFrame <- tkframe(rightFrame)
  AVariable <- tclVar(dialog.values$initial.A)
  AField <- ttkentry(AFrame,width="6",textvariable=AVariable)
   
  tkgrid(getFrame(yieldBox), getFrame(volBox), 
           getFrame(betaBox), sticky = "nw")
  tkgrid(labelRcmdr(dataFrame, text=""))         
  tkgrid(dataFrame, sticky="w")
  
  tkgrid(labelRcmdr(volPortfFrame, text = gettextRcmdr("Portfolio index volatility (%)   ")), 
         volPortfField, sticky = "e", padx=c(10, 10))
  tkgrid(volPortfFrame, sticky = "w")
  
  tkgrid(labelRcmdr(intervalFrame, text = gettextRcmdr("Rebalancing interval (months)")), 
         intervalField, sticky = "e", padx=c(10, 10))
  tkgrid(intervalFrame, sticky = "w")
  
  tkgrid(labelRcmdr(numberFrame, text = gettextRcmdr("Number of risky investments   ")), 
         numberField, sticky = "e", padx=c(10, 10))
  tkgrid(numberFrame, sticky = "w")
  
  tkgrid(labelRcmdr(amountFrame, text = gettextRcmdr("Total investment (\u20AC)                 ")), 
         amountField, sticky = "e", padx=c(10, 10))
  tkgrid(amountFrame, sticky = "w")
  
  tkgrid(labelRcmdr(interestFrame, text = gettextRcmdr("Risk-free interest rate (%)")), 
         interestField, sticky = "e")
  tkgrid(interestFrame, sticky = "w")
  
  #tkgrid(labelRcmdr(AFrame, text=""))
  tkgrid(labelRcmdr(AFrame, text = gettextRcmdr("Risk aversion coefficient  ")), 
         AField, sticky = "e")
  tkgrid(AFrame, sticky = "w")
  
  tkgrid(leftFrame, rightFrame, sticky = "nw")
  tkgrid(mainFrame, sticky="w")
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  tkgrid.configure(volPortfField, sticky = "e")
  dialogSuffix()
}

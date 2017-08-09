stockPrice <-
function () {
    defaults <- list (initial.div = 1, initial.k = 12, initial.g = 10,
                      initial.ROE = 50,initial.b=20,initial.riskless=5,initial.MP=8,
                      initial.beta=90)
    dialog.values <- getDialog ("stockPrice", defaults)  
    initializeDialog(title = gettextRcmdr("Determining stock price"))
    
    onOK <- function() {
      div <- tclvalue(divVariable)
      k <- tclvalue(kVariable)
      g <- tclvalue(gVariable)
      ROE <- tclvalue(ROEVariable)
      b <- tclvalue(bVariable)
      riskless <- tclvalue(risklessVariable)
      MP <- tclvalue(MPVariable)
      beta <- tclvalue(betaVariable)
      
      putDialog ("stockPrice",list (initial.div = div, initial.k = k,initial.g = g,
                                    initial.ROE = ROE,initial.b=b,initial.riskless=riskless,initial.MP=MP,
                                    initial.beta=beta))
      closeDialog()
      if((!k=="")&&(!g=="")){
        doItAndPrint(paste("stock.price(dividend=c(",div,"),k=",k,",g=",g,")", sep = ""))}else
          if((!k=="")&&(g=="")){
            doItAndPrint(paste("stock.price(dividend=c(",div,"),k=",k,",ROE=",ROE,",b=",b,")", sep = ""))}else
              if((k=="")&&(!g=="")){
                doItAndPrint(paste("stock.price(dividend=c(",div,"),g=",g,",riskFree=",riskless,
                                   ",marketPremium=",MP,",beta=",beta,")", sep = ""))}else{
                                     doItAndPrint(paste("stock.price(dividend=c(",div,"),ROE=",ROE,",b=",b,",riskFree=",riskless,
                                                        ",marketPremium=",MP,",beta=",beta,")", sep = ""))
                                   }
      
      tkdestroy(top)
      tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = "stock.price", reset = "stockPrice", apply = "stockPrice")
    
    mainFrame <- tkframe(top)
    leftFrame <- tkframe(mainFrame)
    rightFrame <- tkframe(mainFrame)
    
    divFrame <- tkframe(mainFrame)
    divVariable <- tclVar(dialog.values$initial.div)
    divField <- ttkentry(divFrame,width="40",textvariable=divVariable)
    
    kFrame <- tkframe(leftFrame)
    kVariable <- tclVar(dialog.values$initial.k)
    kField <- ttkentry(kFrame,width="6",textvariable=kVariable)
    
    risklessFrame <- tkframe(leftFrame)
    risklessVariable <- tclVar(dialog.values$initial.riskless)
    risklessField <- ttkentry(risklessFrame,width="6",textvariable=risklessVariable)
    
    MPFrame <- tkframe(leftFrame)
    MPVariable <- tclVar(dialog.values$initial.MP)
    MPField <- ttkentry(MPFrame,width="6",textvariable=MPVariable)
    
    betaFrame <- tkframe(leftFrame)
    betaVariable <- tclVar(dialog.values$initial.beta)
    betaField <- ttkentry(betaFrame,width="6",textvariable=betaVariable)
    
    gFrame <- tkframe(rightFrame)
    gVariable <- tclVar(dialog.values$initial.g)
    gField <- ttkentry(gFrame,width="6",textvariable=gVariable)
    
    ROEFrame <- tkframe(rightFrame)
    ROEVariable <- tclVar(dialog.values$initial.ROE)
    ROEField <- ttkentry(ROEFrame,width="6",textvariable=ROEVariable)
    
    bFrame <- tkframe(rightFrame)
    bVariable <- tclVar(dialog.values$initial.b)
    bField <- ttkentry(bFrame,width="6",textvariable=bVariable)
    
    
    tkgrid(labelRcmdr(divFrame, text = gettextRcmdr("Future dividends (\u20AC)                      ")),
           divField, sticky = "w", padx=c(10, 0))
    tkgrid(divFrame,sticky= "nw")
    
    
    tkgrid(labelRcmdr(leftFrame, text="Fill either k or the boxes below it",fg="red"),sticky="w", padx=c(10, 0),pady=c(10,10))
    tkgrid(labelRcmdr(kFrame, text = gettextRcmdr("Risk adjusted return k (%)")),
           kField, sticky = "e", padx=c(10, 0))
    tkgrid(kFrame, sticky = "w")
    
    tkgrid(labelRcmdr(risklessFrame, text = gettextRcmdr("Riskless rate r (%)             ")),
           risklessField, sticky = "e", padx=c(10, 0))
    tkgrid(risklessFrame, sticky = "w")
    
    
    tkgrid(labelRcmdr(MPFrame, text = gettextRcmdr("Market risk premium (%)  ")),
           MPField, sticky = "w", padx=c(10, 0))
    tkgrid(MPFrame, sticky = "w")
    
    
    tkgrid(labelRcmdr(betaFrame, text = gettextRcmdr("Beta (%)                            ")),
           betaField, sticky = "w", padx=c(10, 0))
    tkgrid(betaFrame, sticky = "w")
    
    tkgrid(labelRcmdr(rightFrame, text="Fill either g or the boxes below it",fg="red"),sticky="w", padx=c(10, 0), pady=c(10,10))
    
    tkgrid(labelRcmdr(gFrame, text = gettextRcmdr("Growth rate g (%)             ")),
           gField, sticky = "e", padx=c(10, 0))
    tkgrid(gFrame, sticky = "w")
    
    tkgrid(labelRcmdr(ROEFrame, text = gettextRcmdr("Return on equity ROE (%) ")),
           ROEField, sticky = "e", padx=c(10, 0))
    tkgrid(ROEFrame, sticky = "w")
    
    
    tkgrid(labelRcmdr(bFrame, text = gettextRcmdr("Plowback ratio b (%)          ")),
           bField, sticky = "w", padx=c(10, 0))
    tkgrid(bFrame, sticky = "w")
    
    
    tkgrid(leftFrame, rightFrame, sticky = "nw")
    tkgrid(mainFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
    #tkgrid.configure(timeField, sticky = "e")
    dialogSuffix()
  }

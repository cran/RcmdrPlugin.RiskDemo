ruinDialog <-
function () {
  defaults <- list (initial.lambda = 100, initial.alpha = 1, initial.beta = 0.1,
                    initial.U0 = 1000,initial.eps="",initial.theta=1.25,initial.T0=10,
                    initial.nsim=10,initial.Tup=10)
  dialog.values <- getDialog ("ruinDialog", defaults)
  initializeDialog(title = gettextRcmdr("Illustrating classical ruin theory"))
  
  
  lambdaVariable <- tclVar(dialog.values$initial.lambda)
  alphaVariable <- tclVar(dialog.values$initial.alpha)
  betaVariable <- tclVar(dialog.values$initial.beta)
  U0Variable <- tclVar(dialog.values$initial.U0)
  epsVariable <- tclVar(dialog.values$initial.eps)
  thetaVariable <- tclVar(dialog.values$initial.theta)
  T0Variable <- tclVar(dialog.values$initial.T0)
  nsimVariable <- tclVar(dialog.values$initial.nsim)
  TupVariable <- tclVar(dialog.values$initial.Tup)
  ans <- NULL
  
  onOK <- function() {
    lambda <- tclvalue(lambdaVariable)
    alpha <- tclvalue(alphaVariable)
    beta <- tclvalue(betaVariable)
    U0 <- tclvalue(U0Variable)
    eps <- tclvalue(epsVariable)
    theta <- tclvalue(thetaVariable)
    T0 <- tclvalue(T0Variable)
    nsim <- tclvalue(nsimVariable)
    Tup <- tclvalue(TupVariable)

    putDialog ("ruinDialog",list (initial.lambda = lambda, initial.alpha = alpha,
                    initial.beta = beta, initial.U0 = U0,initial.eps=eps,
                    initial.theta=theta,initial.T0=T0,
                    initial.nsim=nsim,initial.Tup=Tup))
    closeDialog()
    if(T0==""){
      if(eps==""){
      doItAndPrint(paste("(ans <- computeRuin(U0=",U0,",theta=",theta,"/100,alpha=",
                              alpha,",beta=",beta,"))",sep=""))}else if(theta==""){
      doItAndPrint(paste("(ans <- computeRuin(U0=",U0,",eps=",eps,"/100,alpha=",
                              alpha,",beta=",beta,"))",sep=""))}else if(U0==""){
      doItAndPrint(paste("(ans <- computeRuin(theta=",theta,"/100,eps=",eps,"/100,alpha=",
                              alpha,",beta=",beta,"))",sep=""))}

    }else{
        if(eps==""){doItAndPrint(paste("(ans <- computeRuinFinite(T0=",T0,",U0=",U0,",theta=",theta,
                    "/100,lambda=",lambda,",alpha=",alpha,",beta=",beta,"))",sep=""))
        }else if(theta==""){doItAndPrint(paste("(ans <- computeRuinFinite(T0=",T0,",U0=",U0,",eps=",eps,
                    "/100,lambda=",lambda,",alpha=",alpha,",beta=",beta,"))",sep=""))
        }else if(U0==""){
            doItAndPrint(paste("(ans <- computeRuinFinite(T0=",T0,",eps=",eps,"/100,theta=",theta,
                    "/100,lambda=",lambda,",alpha=",alpha,",beta=",beta,"))",sep=""))}
    }
    #if(theta!="" && U0!=""){
    #  doItAndPrint(paste("drawRuin(nsim=",nsim,",Tup=",Tup,",U0=",U0,",theta=",theta,
    #                "/100,lambda=",lambda,",alpha=",alpha,",beta=",beta,")",sep=""))}
    doItAndPrint(paste("drawRuin(nsim=",nsim,",Tup=",Tup,",U0=",ans$initialCapital,",theta=",ans$safetyLoading,
                       ",lambda=",lambda,",alpha=",alpha,",beta=",beta,")",sep=""))
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "computeRuin", reset = "ruinDialog", apply = "ruinDialog")

  mainFrame <- tkframe(top)
  leftFrame <- tkframe(mainFrame)
  rightFrame <- tkframe(mainFrame)

  lambdaFrame <- tkframe(leftFrame)
  lambdaVariable <- tclVar(dialog.values$initial.lambda)
  lambdaField <- ttkentry(lambdaFrame,width="6",textvariable=lambdaVariable)

  gammaFrame <- tkframe(rightFrame)
  alphaFrame <- tkframe(gammaFrame)
  alphaVariable <- tclVar(dialog.values$initial.alpha)
  alphaField <- ttkentry(alphaFrame,width="6",textvariable=alphaVariable)

  betaFrame <- tkframe(gammaFrame)
  betaVariable <- tclVar(dialog.values$initial.beta)
  betaField <- ttkentry(betaFrame,width="6",textvariable=betaVariable)

  T0Frame <- tkframe(leftFrame)
  T0Variable <- tclVar(dialog.values$initial.T0)
  T0Field <- ttkentry(T0Frame,width="6",textvariable=T0Variable)

  U0Frame <- tkframe(leftFrame)
  U0Variable <- tclVar(dialog.values$initial.U0)
  U0Field <- ttkentry(U0Frame,width="6",textvariable=U0Variable)

  epsFrame <- tkframe(leftFrame)
  epsVariable <- tclVar(dialog.values$initial.eps)
  epsField <- ttkentry(epsFrame,width="6",textvariable=epsVariable)

  thetaFrame <- tkframe(leftFrame)
  thetaVariable <- tclVar(dialog.values$initial.theta)
  thetaField <- ttkentry(thetaFrame,width="6",textvariable=thetaVariable)

  nsimFrame <- tkframe(rightFrame)
  nsimVariable <- tclVar(dialog.values$initial.nsim)
  nsimField <- ttkentry(nsimFrame,width="6",textvariable=nsimVariable)

  TupFrame <- tkframe(rightFrame)
  TupVariable <- tclVar(dialog.values$initial.Tup)
  TupField <- ttkentry(TupFrame,width="6",textvariable=TupVariable)

  tkgrid(labelRcmdr(lambdaFrame, text="Model parameters for compound Poisson distribution",fg="blue"),sticky="w", padx=c(10, 10), pady=c(10,10), columnspan = 2)
  tkgrid(labelRcmdr(gammaFrame, text="Claim sizes are assumed to be gamma-distributed",fg="blue"),sticky="w", pady=c(10,10),columnspan=2)

  tkgrid(labelRcmdr(lambdaFrame, text = gettextRcmdr("Claim intensity (lambda)")),
         lambdaField, sticky = "w", padx=c(10, 100))
  tkgrid(lambdaFrame,sticky= "nw")

  tkgrid(labelRcmdr(alphaFrame, text = gettextRcmdr("Shape (alpha) ")),
         alphaField, sticky = "w")

  tkgrid(labelRcmdr(betaFrame, text = gettextRcmdr("Rate (beta) ")),
         betaField, sticky = "w")
  tkgrid(alphaFrame,betaFrame,sticky="w")
  tkgrid(gammaFrame,sticky= "w")

  tkgrid(labelRcmdr(leftFrame, text="Time horizon",fg="blue"),sticky="w" , padx=c(10, 10), pady=c(10,10))

  tkgrid(labelRcmdr(T0Frame, text = gettextRcmdr("Time horizon (years)      ")),
         T0Field, sticky = "w", padx=c(10, 0))
  tkgrid(T0Frame, sticky = "w")

  tkgrid(labelRcmdr(leftFrame, text="Fill two of the following:",fg="red"),sticky="w", padx=c(10, 0),pady=c(10,10))
  tkgrid(labelRcmdr(U0Frame, text = gettextRcmdr("Initial capital (\u20AC)             ")),
         U0Field, sticky = "e", padx=c(10, 0))
  tkgrid(U0Frame, sticky = "w")

  tkgrid(labelRcmdr(thetaFrame, text = gettextRcmdr("Safety loading (%)          ")),
         thetaField, sticky = "w", padx=c(10, 0))
  tkgrid(thetaFrame, sticky = "w")

  tkgrid(labelRcmdr(epsFrame, text = gettextRcmdr("Ruin probability (%)       ")),
         epsField, sticky = "e", padx=c(10, 0))
  tkgrid(epsFrame, sticky = "w")


  tkgrid(labelRcmdr(rightFrame, text="Figure parameters",fg="blue"),sticky="w", pady=c(90,10))



  tkgrid(labelRcmdr(nsimFrame, text = gettextRcmdr("Number of simulation paths  ")),
         nsimField, sticky = "w")
  tkgrid(nsimFrame, sticky = "w")


  tkgrid(labelRcmdr(TupFrame, text = gettextRcmdr("Time horizon (years)               ")),
         TupField, sticky = "w")
  tkgrid(TupFrame, sticky = "w")

  tkgrid(leftFrame, rightFrame, sticky = "nw")
  tkgrid(mainFrame, sticky="w")
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  #tkgrid.configure(timeField, sticky = "e")
  dialogSuffix()
}

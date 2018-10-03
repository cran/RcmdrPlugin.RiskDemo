plot.lca <-
function (x, ...) 
{
  x$basis <- cbind(x$ax, x$bx)
  x$coeff <- cbind(rep(1, length(x$kt)), x$kt)
  colnames(x$basis) <- c("mean", "bx")
  if (x$adjust != "none") 
    xlab <- "kt (adjusted)"
  else xlab <- "kt"
  ftsa::plot.ftsm(x=x, components=1, xlab1 = "Age", ylab1 = "bx", xlab2 ="Year", ylab2=xlab, mean.lab = "ax", 
                  ...)
}

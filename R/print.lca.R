print.lca <-
function (x, ...) 
{
  cat("Lee-Carter analysis\n")
  cat(paste("\nCall:", deparse(x$call), "\n"))
  cat(paste("\nAdjustment method:", x$adjust))
  cat(paste("\nRegion:"), x$label)
  cat(paste("\nYears in fit:", min(x$year), "-", max(x$year)))
  cat(paste("\nAges in fit:", min(x$age), "-", max(x$age), 
            "\n"))
  cat(paste("\nPercentage variation explained: ", round(x$varprop * 
                                                          100, 1), "%\n", sep = ""))
}

#takes in an aov as in return_aov from ezANOVA
#could smart pick out of an ezANOVA object, maybe even forcing a return_aov = TRUE and re-evaluating?
#handle aliases of sources on call?
#It would be nice if there were a general "clean name" attribute in use
anovaSourceTable <- function (aov.res, interaction.replace=" x ") {
  aov.summary <- summary(aov.res)
  takeall <- NULL
  for (i in 1:length(aov.summary)) {
    takeall <- rbind(takeall,as.data.frame(aov.summary[[i]][[1]]))
  }
  resid.rows.logic <- grepl("Residuals",rownames(takeall))
  resid.rows <- takeall[resid.rows.logic,]
  source.names <- row.names(takeall)
  source.names[resid.rows.logic] <- gsub("Error: ","",names(aov.summary))
  source.names <- str_trim(gsub(":",interaction.replace, source.names))
  rownames(takeall) <- source.names
  return(takeall)
}
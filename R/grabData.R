#' Let plotUMAPly handle Seurat, SingleCellExperiment, or data.frame objects
#' 
#' @param x         an object (a data.frame, RcppML::nmf, SCE, or Seurat) 
#' @param use       name of dimred to use, if x is not a df or matrix ("UMAP")
#' @param color_by  column name of metadata (or x itself) to color by ("study")
#' @param label_by  column name of metadata (or of x itself) to label by (NULL)
#' @param dims      what dimred columns (or plain old columns) in x to use (1:6)
#' 
#' @return          a data.frame
#' 
#' @export
grabData <- function(x, use="UMAP", color_by="group", label_by=NULL, dims=1:2) {

  dat <- whatData(x, use=use, color_by=color_by, label_by=label_by, dims=dims)

  if (dat$what == "unknown") {

    stop("Unknown input data format")

  } else { 
  
    rd <- switch(dat$what, 
                 nmf=cbind(dat$rd, dat$cd),
                 data.frame=cbind(dat$rd, dat$cd),
                 SingleCellExperiment=cbind(dat$rd, dat$cd),
                 Seurat=cbind(dat$rd, dat$cd))

  }

  if (names(rd)[1] == "V1") names(rd)[1:3] <- c("X", "Y", color_by)
  rd$label <- paste0(rownames(rd), "(", rd[, color_by], ")")
  if (!is.null(label_by)) rd$label <- factor(rd[, label_by])
  return(rd)

}

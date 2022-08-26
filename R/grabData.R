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
grabData <- function(x, use="UMAP", color_by="study", label_by=NULL, dims=1:6) {

  dat <- whatData(x, use=use, color_by=color_by, label_by=label_by, dims=dims)

  if (dat$what == "unknown") {

    stop("Unknown input data format")

  } else { 
  
    switch(dat$what, 
           nmf=cbind(dat$rd, dat$cd),
           data.frame=cbind(dat$rd, dat$cd),
           SingleCellExperiment=cbind(dat$rd, dat$cd),
           Seurat=cbind(dat$rd, dat$cd))

  }

}

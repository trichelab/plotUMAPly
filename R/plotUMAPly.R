#' toss a 3D UMAP plot up on the interwebs with plotly 
#'
#' TODO
#' 1) add lines for observation-observation (cell) interactions (e.g. ligands)
#' 2) add switching capability (data points / colors) (Ember has done this now)
#'
#' @param x         an object (a data.frame, matrix, SCE, or Seurat) 
#' @param use       name of dimred to use, if x is not a df or matrix ("UMAP")
#' @param color_by  column name of metadata (or x itself) to color by ("study")
#' @param label_by  column name of metadata (or of x itself) to label by (NULL)
#' @param dims      what dimred columns (or plain old columns) in x to use (1:3)
#' @param static    use cached palette(s) of the appropriate size? (TRUE)
#' @param shuffle   shuffle the colors of the cached palette(s)? (FALSE) 
#' @param ...       other stuff passed to plotly
#' 
#' @return          nothing much; a static webpage is created as a side effect.
#' 
#' @import plotly
#'
#' @export
plotUMAPly <- function(x, use="UMAP", color_by="study", label_by=NULL, dims=1:3, static=TRUE, shuffle=FALSE, ...) { 

  d <- 3
  n <- nrow(x)  
  m <- length(dims)
  g <- length(color_by)
  traces <- numTraces(m, d, g)

  # "why so slow? wah!"
  if (traces > 500) { 
    warning("Preparing to plot ", traces, " traces with ", n, " points each...")
  }

  # depending on what kind of object x is, import accordingly 
  rd <- grabData(x, use=use, color_by=color_by, label_by=label_by, dims=dims)
  rd[, color_by] <- factor(cd[, color_by])
  
  # need to discuss with Ember
  names(rd) <- c("X", "Y", "Z", "group")

  # probably refactor this too, most likely into grabData 
  rd$label <- paste0(rownames(rd), "(", rd$group, ")")
  if (!is.null(label_by)) rd$label <- factor(cd[, label_by])

  rd <- cbind(rd, cd) # in case we want to switch
 
  pal <- choosePalette(nlevels(rd$group), static=static, shuffle=shuffle)
  names(pal) <- levels(rd$group)
  
  p <- plot_ly(rd, 
               text = ~ label, 
               hoverinfo = "text", 
               type = "scatter3d",
               color = ~ group, 
               colors = pal)
  axes <- lapply(list(x=1, y=2, z=3), cleanAxis, embed=use)

  # this gets a little hairy:
  # 
  # suppose we allow up to 6 dimensions to be pulled, 
  # with up to 6 metadata columns that we can switch between, 
  # and we plot all of these in three dimensions. Then we have
  # 
  # choose(6, 3) * factorial(3) * 6 == 720 traces to generate 
  #
  # The one saving grace is that only a single trace is active at a time...
  #
  p <- layout(add_markers( p,
                           x = ~X, 
                           y = ~Y, 
                           z = ~Z,
                           ...),
              scene = axes)
  config(p, displayModeBar = FALSE)

}


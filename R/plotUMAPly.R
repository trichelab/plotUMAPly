#' toss a 3D UMAP plot up on the interwebs with plotly 
#'
#' TODO
#' 1) add lines for observation-observation (cell) interactions (e.g. ligands)
#' 2) add hooks for crosstalk so two embeddings can be shown at the same time
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
  if (traces > 100) { 
    warning("Preparing to plot ", traces, " traces with ", n, " points each...")
  }

  # depending on what kind of object x is, import accordingly 
  rd <- grabData(x, use=use, color_by=color_by, label_by=label_by, dims=dims)
  
  # need to discuss with Ember
  if (length(dims) > 3) {
    warning("currently we only support 3 dimensions; this will change soon")
  }

  names(rd) <- c("X", "Y", "Z", "group")
  rd[, color_by] <- factor(cd[, color_by])
 
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

# to merge
plotUMAPly.ember <- function(x, use = "NMF_UMAP", color_by = "study", label_by = NULL, dims_by = 1:3, ...) { 
  
  # interop yuck
  if (is(x, "SingleCellExperiment")) {
    rd <- data.frame(reducedDim(x, use)[,dims_by])
    cd <- colData(x)
  } else if (is(x, "Seurat")) {
    # class Seurat is defined in package SeuratObject
    # this is why I can't bring myself to import Seurat
    rd <- as.data.frame(x@reductions[[use]]@cell.embeddings[,dims_by]) # uuughh
    cd <- x@meta.data
  }
  
  #create list of factor metadata to use if not specified
  if (is.null(color_by_list)) {
    color_by_list <- colnames(cd[sapply(cd, is.factor)])
  }
  
  #prep rd dataframe with group and label columns
  names(rd) <- c("X", "Y", "Z")
  rd[, c("group", "plot_label")] <- NA
  
  #add metadata to rd for switching
  rd <- cbind(rd, cd)
  
  #create empty plot to add traces to
  p <- plot_ly()
  
  #create color palette
  #adds total number of levels needed
  palettelevels <- 0
  for (i in 1:length(color_by_list)){
    palettelevels <- palettelevels + nlevels(rd[,unlist(color_by_list[i])])
  }
  
  seed <- c("#ff0000", "#0000ff", "#00ff00")
  pal <- createPalette(palettelevels, seed, prefix = "color")
  names(pal) <- levels(rd$group)
  
  #list of booleans for trace visibility toggle
  visibility_list <- as.list(rep(FALSE, palettelevels))
  #counter for tracking where visibility needs to change 
  vis_start_counter <- 1
  
  dropdown_list <- list()
  
  
  #######
  #trace creation loop
  for (item in 1:length(color_by_list)){
    #get name of current metadata
    color_by <- unlist(color_by_list[item])
    
    #set current trace as group and label
    rd$group <- factor(rd[,color_by])
    rd$plot_label <- paste0(rownames(rd), "(", rd$group, ")")
    
    #create option for selection dropdown
    #set up visibility_list
    visibility_list[] <- FALSE
    visibility_list[vis_start_counter:
                      (vis_start_counter+nlevels(rd$group)-1)] <- TRUE
    vis_start_counter <- vis_start_counter + nlevels(rd$group)
    
    #build dropdown item
    dropdown_entry <- list(
      method = 'restyle',
      args = list("visible", visibility_list),
      label = unlist(color_by_list[item]))
    #add item to dropdown_list
    dropdown_list <- append(dropdown_list, list(dropdown_entry))
    
    #create trace, only 1st trace starts visible
    p <- p %>% add_trace(rd,
                         mode = "markers",
                         x = rd$X,
                         y = rd$Y,
                         z = rd$Z,
                         text = rd$plot_label, 
                         hoverinfo = "text", 
                         type = "scatter3d",
                         color = rd$group,
                         colors = pal,
                         visible = (item == 1))
    

  }
  
  #######
   
  axes <- lapply(list(x=1, y=2, z=3), .clean_axis, embed="UMAP")
  config(p, displayModeBar = FALSE)
  
  #update menus to add dropdown
  #do not show menu if single metadata option
  if(length(dropdown_list) > 1){
    p <- layout(p, updatemenus = list(
      list(
        y = 0.9,
        buttons = dropdown_list
      )
    )
  )
 }
  else {p <- layout(p)}
}

# helper fn
.clean_axis <- function(i, embed="axis") {
  list(title=paste0(embed, i),
       showticklabels = FALSE,
       zeroline = FALSE,
       showline = FALSE,
       showgrid = FALSE)
}

### $Id: plot.kohonen.R 7 2011-02-25 11:18:24Z ron.wehrens@gmail.com $
### Version 2.0.5: added parameter heatkeywidth (suggestion by Henning
### Rust). Especially useful for multiple plots in one figure.

### Parameter 'main' is mentioned explicitly since in most cases the
### default leaves open ugly space above the plot. We explicitly put
### it 1.2 units above the top row using 'text', if this is within the
### par("usr") range. Else, we use the standard 'title' command.

### Addition 04/11/07: keepMargins, codeRendering and whatmap arguments.
### Adapted for version 2.0: April 11, 2007
### Added quality plot, August 30 2007.
### Added default titles, August 31 2007.

### Modified Aug 16, 2013 by Alan Boyle
### Added hexagonal plotting

### Modified 2015/2016 by Adam Diehl
### Added new plot types and heatkey options


"plot.kohonen" <- function (x,
                            type = c("codes", "changes", "counts",
                              "dist.neighbours", "mapping", "property",
                              "quality", "classes", "property2", "propblend",
			      "somclasses"),
                            classif = NULL,
                            labels = NULL, pchs = NULL, main = NULL,
                            palette.name = NULL, palette.name2 = NULL,
			    ncolors,
                            bgcol=NULL, zlim = NULL, heatkey = TRUE,
                            heatkey2 = TRUE, property, property2, contin,
			    contin2, whatmap = NULL,
                            codeRendering = NULL, keepMargins = FALSE,
                            scale = FALSE, heatkeywidth = .2, 
			    heatkeyposition = "left", heatkeyname = NULL,
			    heatkeyname2 = NULL, 
			    color1 = "red", color2 = "green",
			    alpha = FALSE, heatkeytype = "left",
			    alphatrans = FALSE, fg = "black", ...)
{
  type <- match.arg(type)

  switch(type,
         mapping = plot.kohmapping(x = x, classif = classif,
           main = main, labels = labels, pchs = pchs,
           bgcol = bgcol, keepMargins = keepMargins, fg = fg, ...),
         property = plot.kohprop(x = x, property, main = main,
           palette.name = palette.name, ncolors = ncolors,
           zlim = zlim, heatkey = heatkey,
           contin = contin, keepMargins = keepMargins, 
           heatkeywidth = heatkeywidth,
	   heatkeyposition = heatkeyposition, fg = fg,
	   heatkeyname = heatkeyname, heatkeyname2 = heatkeyname2, ...),
         codes = plot.kohcodes(x = x, main = main,
           palette.name = palette.name, bgcol = bgcol,
           whatmap = whatmap, codeRendering = codeRendering,
           keepMargins = keepMargins, fg = fg, ...),
         quality = plot.kohquality(x = x, classif = classif, main = main,
           palette.name = palette.name, ncolors = ncolors,
           zlim = zlim, heatkey = heatkey, keepMargins = keepMargins, 
           heatkeywidth = heatkeywidth, fg = fg, ...),
         counts = plot.kohcounts(x = x, classif = classif, main = main,
           palette.name = palette.name, ncolors = ncolors,
           zlim = zlim, heatkey = heatkey, keepMargins = keepMargins,
           heatkeywidth = heatkeywidth, fg = fg, ...),
         changes = plot.kohchanges(x = x, main = main,
           keepMargins = keepMargins, ...),
         dist.neighbours = plot.kohUmatrix(x = x, main = main,
           palette.name = palette.name, ncolors = ncolors,
           zlim = zlim, heatkey = heatkey, keepMargins = keepMargins,
           heatkeywidth = heatkeywidth, fg = fg, ...),
         classes = plot.kohclasses(x = x, main = main,
           palette.name = palette.name, bgcol = bgcol,
           codeRendering = codeRendering, property,
           scale = scale, keepMargins = keepMargins, fg = fg, ...),
         property2 = plot.kohprop2(x = x, property1 = property,
           property2 = property2, main = main,
           palette.name1 = palette.name, palette.name2 = palette.name2,
           ncolors = ncolors, zlim = zlim, heatkey = heatkey,
           heatkey2 = heatkey2,
           contin1 = contin, contin2 = contin2,
           keepMargins = keepMargins, heatkeywidth = heatkeywidth,
           heatkeyname1 = heatkeyname, heatkeyname2 = heatkeyname2,
           fg = fg, ...),
	 propblend = plot.kohpropblend(x = x, property1 = property,
	   property2 = property2, main = main, color1 = color1,
	   color2 = color2, ncolors = ncolors, alpha = alpha,
	   zlim = zlim, heatkeytype = heatkeytype,
	   contin = contin, contin2 = contin2, keepMargins = keepMargins,
	   heatkeywidth = heatkeywidth, heatkeyname1 = heatkeyname,
	   heatkeyname2 = heatkeyname2, alphatrans = alphatrans, fg = fg,
	   ...),
	 somclasses = plot.somclasses(x = x, property = property,
	   main = main, palette.name = palette.name, ncolors = ncolors,
           zlim = zlim, heatkey = heatkey, contin = contin,
           keepMargins = keepMargins, heatkeywidth = heatkeywidth,
	   fg = fg, ...))
}


### Overwrite the original plot.somgrid in the class library since
### that leaves open an ugly space at the top of the plot in case of
### hexagonal grids

### Unchanged in version 2.0

plot.somgrid <- function(x, xlim, ylim, ...)
{
  ## Following two lines leave equal amounts of space on both
  ## sides of the plot if no xlim or ylim are given
  if (missing(xlim)) xlim <- c(0, max(x$pts[,1]) + min(x$pts[,1]))
  if (missing(ylim)) ylim <-  c(max(x$pts[,2]) + min(x$pts[,2]), 0)
  MASS::eqscplot(xlim, ylim, axes = FALSE,
                 type = "n", xlab = "", ylab = "", ...)
}


### Adapted for version 2.0: April 11.

plot.kohmapping <- function(x, classif, main, labels, pchs, bgcol,
                            keepMargins, fg = "black", ...)
{
  if (is.null(main)) main <- "Mapping plot"
  
  margins <- rep(0.6, 4)
  if (main != "") margins[3] <- margins[3] + 2
  if (!keepMargins) {
    opar <- par("mar")
    on.exit(par(mar = opar))
  }
  par(mar=margins)
    
  if (is.null(classif) & !is.null(x$unit.classif)) {
    classif <- x$unit.classif
  } else {
    if (is.list(classif) && !is.null(classif$unit.classif))
      classif <- classif$unit.classif
  }
  if (is.null(classif))
    stop("No mapping available")
  
  plot(x$grid, ...)
  title.y <- max(x$grid$pts[,2]) + 1.2
  if (title.y > par("usr")[4] - .2){
    title(main)
  } else {
    text(mean(range(x$grid$pts[,1])),
         title.y,
         main, adj = .5, cex = par("cex.main"),
         font = par("font.main"))
  }

  if (is.null(bgcol)) bgcol <- "transparent"
  
  if (x$grid$topo == "rectangular") {
    symbols(x$grid$pts[, 1], x$grid$pts[, 2],
          circles = rep(0.5, nrow(x$grid$pts)),
          inches = FALSE, add = TRUE, bg = bgcol)
  } else {  
    for(i in 1:nrow(x$grid$pts)) {
      hexagon(x$grid$pts[i, 1], x$grid$pts[i, 2],bg=bgcolors[i],fg="black")
    }
  }
  
  if (is.null(labels) & !is.null(pchs))
    points(x$grid$pts[classif, 1] + rnorm(length(classif), 0, 0.12),
           x$grid$pts[classif, 2] + rnorm(length(classif), 0, 0.12),
           pch = pchs, ...)
  if (!is.null(labels))
    text(x$grid$pts[classif, 1] + rnorm(length(classif), 0, 0.12),
         x$grid$pts[classif, 2] + rnorm(length(classif), 0, 0.12),
         labels, ...)


  invisible()
}


### Adapted for version 2.0: April 11.
### Readapted by Adam Diehl 04/27/2015 (assorted refinements and added
### heatkeyposition param).
### Checked: nope

plot.kohprop <- function(x, property, main, palette.name = NULL, ncolors,
                         zlim = NULL, heatkey, contin, keepMargins,
                         heatkeywidth, fg = "black", heatkeyposition = "left",
			 heatkeyname = NULL, heatkeyname2 = NULL, ...)
{
  if (is.null(main)) main <- "Property plot"
  if (is.null(palette.name)) palette.name <- heat.colors

  if (!(heatkeyposition %in% c("left", "right"))) {
     warning("plot.kohprop: Invalid argument to position. Using default (left).")
     position = "left"
  }
  
  margins <- rep(0.6, 4)
  if (heatkey) {
      if (heatkeyposition == "left") {
          margins[2] <- margins[2] + 4
      } else {
          margins[4] <- margins[4] + 4
      }
  }
  if (main != "") margins[3] <- margins[3] + 2
  if (!keepMargins) {
    opar <- par("mar")
    on.exit(par(mar = opar))
  }
  par(mar = margins)
  
  plot(x$grid, ...)
  title.y <- max(x$grid$pts[,2]) + 1.2
  if (title.y > par("usr")[4] - .2){
    title(main)
  } else {
    text(mean(range(x$grid$pts[,1])),
         title.y,
         main, adj = .5, cex = par("cex.main"),
         font = par("font.main"))
  }
  
  if (is.null(zlim))
    zlim <- range(property, finite = TRUE)

  if (missing(ncolors)) 
    ncolors <- min(length(unique(property[!is.na(property)])), 20)
  bgcol <- palette.name(ncolors)

  bgcolors <- rep("#e6e6e6", nrow(x$grid$pts))

  if (zlim[1] != zlim[2]) {
    # This chokes if all values in property are equal (e.g., all are 0)!
    showcolors <- as.integer(cut(property,
                               seq(zlim[1], zlim[2],
                               length = ncolors + 1),
                               include.lowest = TRUE))
  } else {
    showcolors <- property
    showcolors[!is.na(property)] <- 1
  }
  bgcolors[!is.na(showcolors)] <- bgcol[showcolors[!is.na(showcolors)]]

  if (x$grid$topo == "rectangular") {
    symbols(x$grid$pts[, 1], x$grid$pts[, 2],
          circles = rep(0.5, nrow(x$grid$pts)), inches = FALSE,
          add = TRUE, fg = fg, bg = bgcolors)
  } else { 
    for(i in 1:nrow(x$grid$pts)) {
        if (is.na(showcolors[i])) {
	    hexagon(x$grid$pts[i, 1], x$grid$pts[i, 2],bg="#e6e6e6", fg="transparent")
        } else {
            hexagon(x$grid$pts[i, 1], x$grid$pts[i, 2],bg=bgcolors[i],fg=fg)
        }
    }
  }

  ## if contin, a pretty labelling of z colors will be used; if not,
  ## all colours will have their own label. The latter only if the
  ## number of categories is smaller than 10, unless explicitly
  ## given.
  if (missing(contin))
    contin <- !(length(unique(property)) < 10)
  
  if (heatkey) {
    if (length(unique(property)) < 10 & !contin) {
      plot.heatkey(x, zlim, bgcol, labels = levels(as.factor(property)),
                   contin = contin, heatkeywidth = heatkeywidth,
		   position = heatkeyposition, heatkeyname = heatkeyname,
		   heatkeynametop = heatkeyname2, ...)
    } else {
      plot.heatkey(x, zlim, bgcol, labels = NULL, contin = contin,
                   heatkeywidth = heatkeywidth,
		   position = heatkeyposition, heatkeyname = heatkeyname,
		   heatkeynametop = heatkeyname2, ...)
    }
  }

  invisible()
}



### Adapted for version 2.0, April 11.
### Whatmap argument left out because of trouble with indexing:
### changes is a matrix with a number of columns that is equal to the
### layers in whatmap; so there is no safe way to distinguish between,
### say, a whatmap of c(1,2,4) and 2:4 in a six-layer map where only
### the first four layers are used. Anyway.
### Checked: April 13.

plot.kohchanges <- function(x, main, keepMargins, ...)
{
  if (is.null(main)) main <- "Training progress"
  
  nmaps <- ncol(x$changes)
  
  ## check whether a legend is necessary and what names should be used
  if (nmaps > 1) {
    if (!is.null(colnames(x$changes))) {
      varnames <- colnames(x$changes)
    } else {
      varnames <- paste("Matrix", 1:ncol(x$changes))
    }
  }

  ## prepare a second y-axis in case of two maps
  if (nmaps == 2) {
    if (!keepMargins) {
      opar <- par("mar")
      on.exit(par(mar = opar))
    }
    par(mar=c(5.1, 4.1, 4.1, 4.1)) # axis scale to the right as well

    ## scale so that both have the same max value; assume only
    ## positive values.
    huhn <- x$changes
    huhn[,2] <- max(x$changes[,1]) * huhn[,2] / max(x$changes[,2])
    ticks <- pretty(x$changes[,2], length(axTicks(2)))
  } else {
    huhn <- x$changes
  }

  ## plot the plot!
  matplot(huhn, type = "l", lty = 1, main = main, 
          ylab = "Mean distance to closest unit", xlab = "Iteration", ...)
  abline(h=0, col="gray")

  ## plot the second axis
  if (nmaps == 2)
    axis(4, col.axis=2, at=ticks * max(x$changes[,1]) / max(x$changes[,2]),
         labels=ticks)

  ## plot the legend
  if (nmaps > 1)
    legend("topright", legend = varnames, lty=1, col = 1:nmaps, bty="n") 

  invisible()
}


### Adapted for version 2.0: April 11.
### Checked: April 13.

plot.kohcounts <- function(x, classif, main, palette.name, ncolors,
                           zlim, heatkey, keepMargins, heatkeywidth,
			   fg = "black", ...)
{
  if (is.null(main)) main <- "Counts plot"
  if (is.null(palette.name)) palette.name <- heat.colors
  
  if (is.null(classif) & !is.null(x$unit.classif)) {
    classif <- x$unit.classif
  } else {
    if (is.list(classif) && !is.null(classif$unit.classif))
      classif <- classif$unit.classif
  }
  if (is.null(classif))
    stop("No mapping available")

  counts <- rep(NA, nrow(x$grid$pts))
  huhn <- table(classif)

  counts[as.integer(names(huhn))] <- huhn

  counts<-log10(counts)
  contin <- FALSE
  if (max(counts, na.rm = TRUE) > 10) contin <- TRUE
  
  plot.kohprop(x, property = counts, main = main,
               palette.name = palette.name, ncolors = ncolors,
               zlim = zlim, heatkey = heatkey, contin = contin,
               keepMargins = keepMargins, heatkeywidth = heatkeywidth,
	       fg = fg, ...)

  invisible(counts)
}

### Introduced for version 2.0.5: Jan 16, 2009

plot.kohUmatrix <- function(x, classif, main, palette.name, ncolors,
                            zlim, heatkey, keepMargins, heatkeywidth,
			    fg = "black", ...)
{
  if (x$method != "som" & x$method != "supersom")
    stop("Neighbour distance plot only implemented for (super)som")
  
  if (is.null(main)) main <- "Neighbour distance plot"
  if (is.null(palette.name)) palette.name <- heat.colors

  nhbrdist <- unit.distances(x$grid, x$toroidal)
  nhbrdist[nhbrdist > 1.05] <- NA
  if (x$method == "som") {
    for (i in 2:nrow(nhbrdist)) {
      for (j in 1:(i - 1)) {
        if (!is.na(nhbrdist[i,j]))
          nhbrdist[i,j] <- nhbrdist[j,i] <- dist(x$codes[c(i,j),])
      }
    }
  } else {
    if (x$method == "supersom") { # superfluous check, really
      nhbrdist[!is.na(nhbrdist)] <- 0
      for (k in 1:length(x$data)) {
        for (i in 2:nrow(nhbrdist)) {
          for (j in 1:(i - 1)) {
            if (!is.na(nhbrdist[i,j]))
              nhbrdist[i,j] <- nhbrdist[i,j] +
                x$weights[k] * dist(x$codes[[k]][c(i,j),])
          }
        }
        
        nhbrdist[j,i] <- nhbrdist[i,j]
      }
    }
  }

  neigh.dists <- colSums(nhbrdist, na.rm = TRUE)
  plot.kohprop(x, property = neigh.dists, main = main,
               palette.name = palette.name, ncolors = ncolors,
               zlim = zlim, heatkey = heatkey, contin = TRUE,
               keepMargins = keepMargins, heatkeywidth = heatkeywidth,
	       fg = fg, ...)

  invisible(neigh.dists)
}

### Newly written for version 2.0, August 30 2007.
### Revised as a property plot: August 31 2007.

plot.kohquality <- function(x, classif, main, palette.name, ncolors,
                            zlim, heatkey, keepMargins, fg = "black", ...)
{
  if (is.null(main)) main <- "Distance plot"
  if (is.null(palette.name)) palette.name <- heat.colors

  distances <- NULL
  if (is.null(classif) & !is.null(x$unit.classif)) {
    classif <- x$unit.classif
    distances <- x$distances
  } else {
    if (is.list(classif) &&
        !is.null(classif$unit.classif) &&
        !is.null(classif$distances)) {
      classif <- classif$unit.classif
      distances <- classif$distances
    }
  }
  if (is.null(distances))
    stop("No mapping or mapping distances available")

  similarities <- rep(NA, nrow(x$grid$pts))
  hits <- as.integer(names(table(classif)))
  similarities[hits] <- sapply(split(distances, classif), mean)
                      
  plot.kohprop(x, property = similarities, main = main,
               palette.name = palette.name, ncolors = ncolors,
               zlim = zlim, heatkey = heatkey, contin = TRUE,
               keepMargins = keepMargins, fg = fg, ...)

  invisible(similarities)
}


### Adapted for version 2.0: April 11.
### Checked: April 13.
### New elements: whatmap, codeRendering, keepMargins, legend
### Added palette.name for version 2.0.6. Aug 3, 2010.

plot.kohcodes <- function(x, main, palette.name, bgcol, whatmap,
                          codeRendering, keepMargins, fg = "black", ...)
{
  if (!keepMargins) {
    opar <- par(c("mar", "ask"))
    on.exit(par(opar))
  }

  if (is.null(palette.name)) palette.name <- terrain.colors
  
  whatmap <- check.whatmap(x, whatmap)
  nmaps <- length(whatmap)
  
  ## check if x$codes is a list; if so, call this function for every
  ## list element separately.
  if (is.list(x$codes)) {
    if (prod(par("mfrow")) < nmaps) par(ask = TRUE)

    for (i in 1:nmaps) {
      ## make a new object that only has one set of codebook vectors
      huhn <- x
      huhn$codes <- huhn$codes[[whatmap[i]]]

      ## allow a different title for every plot
      if (length(main) == length(x$codes)) {
        main.title <- main[whatmap[i]]
      } else {
        if (length(main) == nmaps) {
          main.title <- main[i]
        } else {
          if (length(main) == 1) {
            main.title <- main
          } else {
            if (is.null(main)) {
              if (!is.null(names(x$codes))) {
                main.title <- names(x$codes)[whatmap[i]]
              } else {
                main.title <- "Codes plot"
              }
            }
          }
        }
      }

      ## allow a different codeRendering for every plot
      if (length(codeRendering) == length(x$codes)) {
        cR <- codeRendering[whatmap[i]]
      } else {
        if (length(codeRendering) == nmaps) {
          cR <- codeRendering[i]
        } else {
          cR <- codeRendering
        }
      }

      plot.kohcodes(huhn, main = main.title, palette.name = palette.name,
                    bgcol=bgcol, whatmap = NULL,
                    codeRendering = cR, keepMargins = TRUE, fg = fg, ...)
    }
  } else {
    codes <- x$codes
    nvars <- ncol(codes)
    
    if (is.null(codeRendering)) { ## defaults
      if (nvars < 15) {
        codeRendering <- "segments"
        maxlegendcols <- 3
      } else {
        codeRendering <- "lines"
      }
    }
    
    margins <- rep(0.6, 4)  # no text annotation anywhere
    if (!is.null(main))
      margins[3] <- margins[3] + 2
    par(mar = margins)
    
    if (codeRendering == "segments" & # we need space for the legend here...
        nvars < 15 &
        !is.null(colnames(codes))) {
      plot(x$grid, 
           ylim = c(max(x$grid$pts[,2]) + min(x$grid$pts[,2]), -2))
      current.plot <- par("mfg")
      plot.width <- diff(par("usr")[1:2])

      cex <- 1 # First see if the legend fits
      leg.result <- legend(x = mean(x$grid$pts[,1]), xjust = 0.5,
                           y = 0, yjust = 1,
                           legend = colnames(codes),
                           cex=cex, plot=FALSE,
                           ncol = min(maxlegendcols, nvars),
                           fill = palette.name(nvars))
      while (leg.result$rect$w > plot.width) {
        cex <- cex*0.9 # if too large, decrease text size
        leg.result <- legend(x = mean(x$grid$pts[,1]), xjust = 0.5,
                             y = 0, yjust = 1,
                             legend = colnames(codes),
                             cex=cex, plot=FALSE,
                             ncol = min(maxlegendcols, nvars),
                             fill = palette.name(nvars))
      } # until it fits!

      leg.result <- legend(x = mean(x$grid$pts[,1]), xjust = 0.5,
                           y = 0, yjust = 1, cex=cex,
                           legend = colnames(codes), plot=FALSE,
                           ncol = min(maxlegendcols, nvars),
                           fill = palette.name(nvars), ...)

      par(mfg = current.plot)
      plot(x$grid, 
           ylim = c(max(x$grid$pts[,2]) + min(x$grid$pts[,2]),
             -leg.result$rect$h))

      legend(x = mean(x$grid$pts[,1]), xjust = 0.5,
             y = 0, yjust = 1, cex=cex, plot = TRUE,
             legend = colnames(codes),
             ncol = min(maxlegendcols, nvars),
             fill = palette.name(nvars), ...)
    } else {
      plot(x$grid, ...)
    }
    
    title.y <- max(x$grid$pts[,2]) + 1.2
    if (title.y > par("usr")[4] - .2){
      title(main)
    } else {
      text(mean(range(x$grid$pts[,1])),
           title.y,
           main, adj = .5, cex = par("cex.main"),
           font = par("font.main"))
    }
    
    if (is.null(bgcol)) bgcol <- "transparent"
    
    if (x$grid$topo == "rectangular") {
      symbols(x$grid$pts[, 1], x$grid$pts[, 2],
            circles = rep(0.5, nrow(x$grid$pts)), inches = FALSE,
            add = TRUE, bg = bgcol)
    } else {
      for(i in 1:nrow(x$grid$pts)) {
        hexagon(x$grid$pts[i, 1], x$grid$pts[i, 2],bg=bgcol[i],fg=fg)
      }
    }
    
    if (codeRendering == "lines") {
      yrange <- range(codes)
      codes <- codes - mean(yrange)
    } else {
      codemins <- apply(codes, 2, min)
      codes <- sweep(codes, 2, codemins)
    }

    switch(codeRendering,
           segments = {
             stars(codes, locations = x$grid$pts,
                   labels = NULL, len = 0.4,
                   add=TRUE, col.segments=palette.name(nvars),
                   draw.segments=TRUE)
           },             
           lines = {
             for (i in 1:nrow(x$grid$pts)) { # draw baseline
               if (yrange[1]<0 & yrange[2] > 0) {
                 lines(seq(x$grid$pts[i, 1] - 0.4,
                           x$grid$pts[i, 1] + 0.4,
                           length = 2),
                       rep(x$grid$pts[i, 2], 2),
                       col = "gray")
               }
               lines(seq(x$grid$pts[i, 1] - 0.4,
                         x$grid$pts[i, 1] + 0.4,
                         length = ncol(codes)),
                     x$grid$pts[i, 2] + codes[i, ] * 0.8/diff(yrange),
                     col = "red")
             }
           },
           stars = stars(codes, locations = x$grid$pts,
             labels = NULL, len = 0.4, add=TRUE)
           )
    
  }

  invisible()
}


### This isn't going to with with non-som types
plot.kohclasses <- function(x, main, palette.name = NULL, bgcol,
                             codeRendering, property, scale, keepMargins,
			     fg = "black", fgPie = "black", bg="#e6e6e6", ...)
{
  if (!keepMargins) {
    opar <- par(c("mar", "ask"))
    on.exit(par(opar))
  }
  
  if (is.null(palette.name)) palette.name <- rainbow
  
  codes <- property
  nvars <- ncol(codes)
  maxlegendcols <- 3

  margins <- rep(0.6, 4)  # no text annotation anywhere
  if (!is.null(main))
    margins[3] <- margins[3] + 2
  par(mar = margins)
  
  if(!scale) {
    bgcol <- rep("#e6e6e6", nrow(x$grid$pts))
  }
  
  if (nvars < 15 &
    !is.null(colnames(codes))) {
      plot(x$grid, 
           ylim = c(max(x$grid$pts[,2]) + min(x$grid$pts[,2]), -2))
      current.plot <- par("mfg")
      plot.width <- diff(par("usr")[1:2])
      
      cex <- 1 # First see if the legend fits
      leg.result <- legend(x = mean(x$grid$pts[,1]), xjust = 0.5,
                           y = 0, yjust = 1,
                           legend = colnames(codes),
                           cex=cex, plot=FALSE,
                           ncol = min(maxlegendcols, nvars),
                           fill = palette.name(nvars))
      while (leg.result$rect$w > plot.width) {
        cex <- cex*0.9 # if too large, decrease text size
        leg.result <- legend(x = mean(x$grid$pts[,1]), xjust = 0.5,
                             y = 0, yjust = 1,
                             legend = colnames(codes),
                             cex=cex, plot=FALSE,
                             ncol = min(maxlegendcols, nvars),
                             fill = palette.name(nvars))
      } # until it fits!
      
      leg.result <- legend(x = mean(x$grid$pts[,1]), xjust = 0.5,
                           y = 0, yjust = 1, cex=cex,
                           legend = colnames(codes), plot=FALSE,
                           ncol = min(maxlegendcols, nvars),
                           fill = palette.name(nvars), ...)
      
      par(mfg = current.plot)
      plot(x$grid, 
           ylim = c(max(x$grid$pts[,2]) + min(x$grid$pts[,2]),
                    -leg.result$rect$h))
      
      legend(x = mean(x$grid$pts[,1]), xjust = 0.5,
             y = 0, yjust = 1, cex=cex, plot = TRUE,
             legend = colnames(codes),
             ncol = min(maxlegendcols, nvars),
             fill = palette.name(nvars), ...)
    } else {
      plot(x$grid, ...)
    }
    
    title.y <- max(x$grid$pts[,2]) + 1.2
    if (title.y > par("usr")[4] - .2){
      title(main)
    } else {
      text(mean(range(x$grid$pts[,1])),
           title.y,
           main, adj = .5, cex = par("cex.main"),
           font = par("font.main"))
    }
    
    if (is.null(bgcol)) bgcol <- "transparent"
    
    if (x$grid$topo == "rectangular") {
      symbols(x$grid$pts[, 1], x$grid$pts[, 2],
              circles = rep(0.5, nrow(x$grid$pts)), inches = FALSE,
              add = TRUE, bg = bgcol, border = fg)
    } else {
      for(i in 1:nrow(x$grid$pts)) {

          if ( is.na(codes[i,1]) || sum(codes[i,]) == 0 ) {
	      hexagon(x$grid$pts[i, 1], x$grid$pts[i, 2],bg="#e6e6e6", fg="transparent")
	  } else {
	      hexagon(x$grid$pts[i, 1], x$grid$pts[i, 2],bg=bg,fg=fg)
	  }

#        if(sum(codes[i,] > 1)) {
#            #bgcol[i] = NA
#	    hexagon(x$grid$pts[i, 1], x$grid$pts[i, 2],bg="black",fg=fg)
#        } else {
#	    hexagon(x$grid$pts[i, 1], x$grid$pts[i, 2],bg="#e6e6e6", fg="transparent")
#          }
        }

    }
    
    codemins <- apply(codes, 2, min.na)
    codes <- sweep(codes, 2, codemins)

    maxScale<-max(rowSums(codes))
    for(i in 1:nrow(x$grid$pts)) {
        if ( is.na(codes[i,1]) || sum(codes[i,]) == 0 ) {
	    next
	} else {
    	    counts<-codes[i,]
    	    tp<-pi/2 - 2*pi*c(0,cumsum(counts)/sum(counts))
    	    scale_factor = .45
    	    if(scale) {
      	        scale_factor = scale_factor * log10(sum(counts)) / log10(maxScale)
            }
    	    mcolors<-palette.name(nvars)

	    if (sum(counts) == 0) next
    
            for(j in 1:nvars){
      	        #if (tp[j+1] == tp[j]) next
      		pp<-seq(tp[j], tp[j+1], length=floor((tp[j]-tp[j+1])*10)+2)
      
	        xi <- x$grid$pts[i, 1] + c(0, scale_factor*cos(pp))
      		yi <- x$grid$pts[i, 2] + c(0, scale_factor*sin(pp))
      		polygon(xi,yi, col=mcolors[j], border = fgPie)
    	    }

  	}
    }

    
  invisible()
}

# Shortcut
min.na = function(x) { min(x, na.rm = TRUE) }

hexagon <- function(x,y,fg="black",bg="white") {
  X_vec <- rep(x, 6)
  Y_vec <- rep(y, 6)
  scale <- .5/cos(pi/6)
  for(i in 1:6) {
    X_vec[i] <- X_vec[i] + (scale * sin(2 * pi * (i/6)))
    Y_vec[i] <- Y_vec[i] + (scale * cos(2 * pi * (i/6)))
  }
  
  polygon(X_vec, Y_vec, border=fg, col=bg)
}



### Added heatkeywidth parameter in version 2.0.5 (contribution by
### Henning Rust)
### Added position, heatkeyname and heatkeynametop params (for labels
### at bottom and top of range) (by Adam Diehl)
plot.heatkey <- function (x, zlim, bgcol, labels = NULL, contin = FALSE, heatkeywidth = 0.2, heatkeyname = NULL, heatkeynametop = NULL, position = "left", ...)  
{
  ncolors <- length(bgcol)
  if (!(position %in% c("left", "right"))) {
     warning("plot.heatkey: Invalid argument to position. Using default (left).")
     position = "left"
  }

  yrange <- range(x$grid$pts[, 2])
  smallestx <- min(x$grid$pts[,1])
  largestx <- max(x$grid$pts[,1])

  ## A width of .2 looks OK on my screen
  if (position == "left") {
      xleft <- c(smallestx - heatkeywidth, smallestx) - 1
      yleft <- seq(yrange[1] - 0.5,
                   yrange[2] + 0.5,
                   length = ncolors + 1)
  } else {
      xleft <- c(largestx, largestx + heatkeywidth) + 1
      yleft <- seq(yrange[1] - 0.5,
                   yrange[2] + 0.5,
                   length = ncolors + 1)
  }
  rect(xleft[1], yleft[1:ncolors],
       xleft[2], yleft[2:(ncolors + 1)],
       border = "black", col = bgcol,
       xpd = TRUE)

  cex <- list(...)$cex

  if (contin) {
    zvals <- pretty(zlim)
    zvals <- zvals[zvals <= max(zlim) & zvals >= min(zlim)]
    yvals <- yrange[1] - .5 + (diff(yrange) + 1)*(zvals - zlim[1])/diff(zlim)


    if (position == "left") {
        text(xleft[2] - 1.3*diff(xleft),
             yvals,
             formatC(zvals),
             xpd=TRUE, adj=1, cex=cex)
    } else {
        text(xleft[2] + 0.5 * diff(xleft),
             yvals,
             formatC(zvals),
             xpd=TRUE, adj=0, cex=cex)
    }
  } else {
    if (is.null(labels))
      labels <- 1:ncolors
    if (position == "left") {
        text(xleft[2] - 1.3 * diff(xleft),
             yleft[-1] - 0.5*diff(yleft[1:2]),
             sort(labels),
             xpd = TRUE, adj=1, cex=cex)
    } else {
        text(xleft[2] + 0.5 * diff(xleft),
             yleft[-1] - 0.5 * diff(yleft[1:2]),
             sort(labels),
             xpd = TRUE, adj=0, cex=cex)
    }
  }
  if (!is.null(heatkeyname)) {
      if (position == "left") {
          text(smallestx - 1, yrange[1] - 2, heatkeyname, xpd = TRUE, adj = 1, cex = cex)
      } else {
          text(largestx + 1, yrange[1] - 2, heatkeyname, xpd = TRUE, adj = 0, cex = cex)
      }
  }
  if (!is.null(heatkeynametop)) {
      if (position == "left") {
          text(smallestx - 1, yrange[2] + 2, heatkeynametop, xpd = TRUE, adj = 1, cex = cex)
      } else { 
          text(largestx + 1, yrange[2] + 2, heatkeynametop, xpd = TRUE, adj = 0, cex = cex)
      }
  }
}

### Show cluster boundaries additional to one of the map plots
### Additional arguments may be col. Based on code from Leo Lopes.

add.cluster.boundaries <- function(x, cluster, lwd = 5, ...) {
  nhbrdist <- unit.distances(x$grid, x$toroidal)
  neighbours <- alply(nhbrdist, 1, function(y) which(y > .95 & y < 1.05))
  ## which neighbours are in a different cluster?
  neighbours.diffclass <-
    lapply(1:length(neighbours),
           function(i, y, class) y[[i]][(class[y[[i]]] != class[i])],
           neighbours, cluster)
  ## avoid counting differences twice
  neighbours.diffclass2 <-
    lapply(1:length(neighbours),
           function(i, y) y[[i]][y[[i]] > i],
           neighbours.diffclass)

  ## Function to actually plot the boundaries. u1 always larger than
  ## u2, so we only need to check E, NE and NW for hexagonal maps, and
  ## E, NE, N and NW for rectangular maps
  plot.boundary <- function(u2, u1, grid, lwd, ...) {
    dloc <- grid$pts[u1,] - grid$pts[u2,]
    
		## Alan Boyle mod
    ## E = 1-2
    ## SE = 2-3
    ## SW = 3-4
    ## W = 4-5
    ## NW = 5-6
    ## NE = 6-1
    if (grid$topo == "hexagonal") {
      if(abs(dloc[1]) <= 1 && abs(dloc[2]) <= 1) {  ## Normal cells
        if (dloc[1] < 0) {
          if (dloc[2] == 0) { ## E
            angle <- 1
          } else if (dloc[2] < 0) { ## NE
            angle <- 6
          } else { ## SE
            angle <- 2
          }
        } else {
          if (dloc[2] == 0) { ## W
            angle <- 4
          } else if (dloc[2] < 0) { ## NW
            angle <- 5
          } else { ## SW
            angle <- 3
          }   
        }
      } else if(abs(dloc[1]) > 1 && abs(dloc[2]) <= 1) { ##X-axis edges
        if (dloc[1] > 0) {
          if (dloc[2] == 0) { ## E
            angle <- 1
          } else if (dloc[2] < 0) { ## NE
            angle <- 6
          } else { ## SE
            angle <- 2
          }
        } else {
          if (dloc[2] == 0) { ## W
            angle <- 4
          } else if (dloc[2] < 0) { ## NW
            angle <- 5
          } else { ## SW
            angle <- 3
          }   
        }
      } else if(abs(dloc[1]) <= 1 && abs(dloc[2]) > 1) { ##Y-axis edges - only N/S
        if (dloc[1] > 0) {
          if (dloc[2] < 0) { ## SW
            angle <- 3
          } else { ## NW
            angle <- 5
          }
        } else {
          if (dloc[2] < 0) { ## SE
            angle <- 2
          } else { ## NE
            angle <- 6
          }   
        }
      } else { #Corner edges
        if (dloc[1] > 0) { ## SE
            angle <- 2
        } else { ## NW
            angle <- 5
        }
      }
      
      radius <- .5/cos(pi/6)             ## horizontal unit distance always 1
      angle2 <- (angle%%6) + 1
      segments(grid$pts[u1,1]+radius*sin(2 * pi * (angle/6)),
               grid$pts[u1,2]+radius*cos(2 * pi * (angle/6)),
               grid$pts[u1,1]+radius*sin(2 * pi * (angle2/6)),
               grid$pts[u1,2]+radius*cos(2 * pi * (angle2/6)),
               lwd = lwd, ...)

    } else { ## slightly different use of angle here
      angle <- 2                         ## NE
      if (abs(dloc[1]) < .1) {
        angle <- 3                       ## N
      } else {
        if (abs(dloc[2]) < .1) {
          angle <- 1                     ## E
        } else {
          if (dloc[1] < 0) {
            angle <- 4                   ## NW
          }
        }
      }

      boundary <- switch(angle,
                         "1" = { ## E
                           x0 <- x1 <- grid$pts[u2,1] + .5
                           y0 <- grid$pts[u2,2] - .3
                           y1 <- y0 + .6
                           list(x0, y0, x1, y1)
                         },
                         "2" = { ## NE
                           x0 <- c(grid$pts[u2,1] + .5, grid$pts[u2,1] + .3)
                           x1 <- x0 + .2
                           y0 <- c(grid$pts[u2,2] + .7, grid$pts[u2,2] + .5)
                           y1 <- y0 - .2
                           list(x0, y0, x1, y1)
                         } ,
                         "3" = { ## N
                           y0 <- y1 <- grid$pts[u2,2] + .5
                           x0 <- grid$pts[u2,1] - .3
                           x1 <- x0 + .6
                           list(x0, y0, x1, y1)
                           },
                         "4" = { ## NW
                           x0 <- c(grid$pts[u2,1] - .7, grid$pts[u2,1] - .5)
                           x1 <- x0 + .2
                           y0 <- c(grid$pts[u2,2] + .5, grid$pts[u2,2] + .3)
                           y1 <- y0 + .2
                           list(x0, y0, x1, y1)
                         })
      ## Go!
      segments(x0 = boundary[[1]], y0 = boundary[[2]],
               x1 = boundary[[3]], y1 = boundary[[4]],
               lwd = lwd, ...)
    }
  }
  
  for (i in 1:length(neighbours)) {
    if (length(neighbours.diffclass2[[i]]) > 0)
      sapply(neighbours.diffclass2[[i]],
             function(ii, j) plot.boundary(ii, j, x$grid, lwd = lwd, ...),
             i)
  }
}


### Plot an SOM with each cell divided into two segments with separate palettes for properties 1 and 2.
### Added by Adam Diehl
plot.kohprop2 = function (x, property1, property2, main = NULL, palette.name1,
    palette.name2, ncolors, zlim = NULL, heatkey = TRUE, 
    heatkey2 = TRUE, contin1, contin2, keepMargins, heatkeywidth, heatkeyname1,
    heatkeyname2, fg = "black", ...) 
{

    if (is.null(main)) 
        main <- "Property plot"
    if (is.null(palette.name1)) 
        palette.name1 <- heat.colors
    if (is.null(palette.name2))
        palette.name2 <- heat.colors
    margins <- rep(0.6, 4)
    if (heatkey) 
        margins[2] <- margins[2] + 4
    if (heatkey2)
	margins[4] <- margins[4] + 4
    if (main != "") 
        margins[3] <- margins[3] + 2
    if (!keepMargins) {
        opar <- par("mar")
        on.exit(par(mar = opar))
    }
    par(mar = margins)
    plot(x$grid, ...)
    title.y <- max(x$grid$pts[, 2]) + 1.2
    if (title.y > par("usr")[4] - 0.2) {
        title(main)
    }
    else {
        text(mean(range(x$grid$pts[, 1])), title.y, main, adj = 0.5, 
            cex = par("cex.main"), font = par("font.main"))
    }

    if (is.null(zlim))
        zlim <- range(property1, property2, finite = TRUE)

    if (missing(ncolors) || is.null(ncolors)) 
        ncolors <- min(length(unique(property1[!is.na(property1)])), 
            length(unique(property2[!is.na(property2)])), 20)


    bgcol1 <- palette.name1(ncolors)
    bgcol2 <- palette.name2(ncolors)

    bgcolors1 <- rep("#e6e6e6", nrow(x$grid$pts))
    bgcolors2 <- rep("#e6e6e6", nrow(x$grid$pts))

    showcolors1 <- as.integer(cut(property1, seq(zlim[1], zlim[2], 
        length = ncolors + 1), include.lowest = TRUE))
    showcolors2 <- as.integer(cut(property2, seq(zlim[1], zlim[2],
        length = ncolors + 1), include.lowest = TRUE))

    bgcolors1[!is.na(showcolors1)] <- bgcol1[showcolors1[!is.na(showcolors1)]]
    bgcolors2[!is.na(showcolors2)] <- bgcol2[showcolors2[!is.na(showcolors2)]]

    if (x$grid$topo == "rectangular") {

        symbols(x$grid$pts[, 1], x$grid$pts[, 2], circles = rep(0.5, 
            nrow(x$grid$pts)), inches = FALSE, add = TRUE, fg = fg, 
            bg = "transparent")

    } else {

        for (i in 1:nrow(x$grid$pts)) {
	    if (is.na(showcolors1[i]) && is.na(showcolors2[i])) {
	        hexagon(x$grid$pts[i, 1], x$grid$pts[i, 2], bg = "#e6e6e6", fg = "transparent")
	    } else {
                half_hexagon_upper(x$grid$pts[i, 1], x$grid$pts[i, 2], bg = bgcolors1[i], 
                    fg = "transparent")
 	        half_hexagon_lower(x$grid$pts[i, 1], x$grid$pts[i, 2], bg = bgcolors2[i],
                    fg = "transparent")
		hexagon(x$grid$pts[i, 1], x$grid$pts[i, 2], bg = "transparent", fg = fg)
            }
        }
    }

    # Heat key for set 1
    if (missing(contin1))
        contin1 <- !(length(unique(property1)) < 10)
    if (heatkey) {
        if (length(unique(property1)) < 10 & !contin1) {
            plot.heatkey(x, zlim, bgcol1, labels = levels(as.factor(property1)), 
                contin = contin1, heatkeywidth = heatkeywidth, heatkeyname = heatkeyname1,
                position = "left", ...)
        } else {
            plot.heatkey(x, zlim, bgcol1, labels = NULL, contin = contin1, 
                heatkeywidth = heatkeywidth, heatkeyname = heatkeyname1,
		position = "left", ...)
        }
    }

    # Heat key for set 2
    if (missing(contin2))
        contin2 <- !(length(unique(property2)) < 10)
    if (heatkey2) {
        if (length(unique(property2)) < 10 & !contin2) {
            plot.heatkey(x, zlim, bgcol2, labels = levels(as.factor(property2)),
                contin = contin2, heatkeywidth = heatkeywidth, heatkeyname = heatkeyname2,
                position = "right", ...)
        }
        else {
            plot.heatkey(x, zlim, bgcol2, labels = NULL, contin = contin2,
                heatkeywidth = heatkeywidth, heatkeyname = heatkeyname2,
		position = "right", ...)
        }
    }
    invisible()
}

### Draw the upper half of a hexagon for a prop2 plot
### Added by Adam Diehl
half_hexagon_upper <- function(x,y,fg="black",bg="white") {
  X_vec <- rep(x, 6)
  Y_vec <- rep(y, 6)
  scale <- .5/cos(pi/6)
  for(i in c(1,6,5,4)) {
    X_vec[i] <- X_vec[i] + (scale * sin(2 * pi * (i/6)))
    Y_vec[i] <- Y_vec[i] + (scale * cos(2 * pi * (i/6)))
  }

  polygon(X_vec, Y_vec, border=fg, col=bg)
}

### Draw the lower half of a hexagon fpr a prop2 plot
### Added by Adam Diehl
half_hexagon_lower <- function(x,y,fg="black",bg="white") {
  X_vec <- rep(x, 6)
  Y_vec <- rep(y, 6)
  scale <- .5/cos(pi/6)
  for(i in 1:4) {
    X_vec[i] <- X_vec[i] + (scale * sin(2 * pi * (i/6)))
    Y_vec[i] <- Y_vec[i] + (scale * cos(2 * pi * (i/6)))
  }

  polygon(X_vec, Y_vec, border=fg, col=bg)
}

### Plot a 2-D heatkey to the left of a plot with optional names.
### Y-Axis uses colors in bgcol, X-axis adjusts alpha evenly over
### kdim columns. klim gives max and min levels for X-axis labels.
### Added by Adam Diehl
plot.heatkey.2d <- function (x, zlim, klim, kdim, bgcol, labels, continx = FALSE, continy = FALSE, heatkeywidth, heatkeyname, heatkeyname2, heatkeynamex, ...)
{
  ncolors <- length(bgcol)

  yrange <- range(x$grid$pts[, 2])
  smallestx <- min(x$grid$pts[,1])
  xrange = c(smallestx - (heatkeywidth * kdim), smallestx - 1)
  ## A width of .2 looks OK on my screen
  xleft <- seq( (smallestx - (heatkeywidth * kdim)), (smallestx - 1),
  	        length = kdim)
  yleft <- seq(yrange[1] - 0.5,
               yrange[2] + 0.5,
                      length = ncolors + 1)
 
  counts = seq(klim[1], klim[2], length = kdim)
  alphavals = (counts / max(counts))

  bgcolors = as.data.frame(matrix(nrow = ncolors, ncol = kdim))
  for (i in 1:kdim) {
      for (j in 1:ncolors) {
      	  col = col2rgb(bgcol[j])/255
      	  bgcolors[j,i] = rgb(col[1], col[2], col[3], alphavals[i])
      }
  }

  for (i in 1:kdim) {
      rect(xleft[i], yleft[1:ncolors],
           (xleft[i] + heatkeywidth), yleft[2:(ncolors + 1)],
           border = "transparent", col = as.vector(bgcolors[,i]),
           xpd = TRUE)
   }
   rect(xleft[1], yleft[1], xleft[kdim] + heatkeywidth, yleft[(ncolors + 1)], border = "black", col = "transparent", xpd = TRUE)

  cex <- list(...)$cex

  # Y-Axis Series
    if (continy) {
       zvals <- pretty(zlim)
       zvals <- zvals[zvals <= max(zlim) & zvals >= min(zlim)]
       yvals <- yrange[1] - .5 + (diff(yrange) + 1)*(zvals - zlim[1])/diff(zlim)

       text(xleft[2] - 1.3*diff(xleft),
         yvals,
         formatC(zvals),
         xpd=TRUE, adj=1, cex=0.8)
    } else {
       if (missing(labels) || is.null(labels))
       labels <- 1:ncolors

       text(xleft[2] - 1.3 * diff(xleft),
          yleft[-1] - 0.5*diff(yleft[1:2]),
          sort(labels),
          xpd = TRUE, adj=1, cex=0.8)
   }

  # X-Axis Series
    if (continx) {
       # Because of small scale, use only about three values spanning the range
       kvals <- pretty(klim, n = 3)
       kvals <- kvals[kvals <= max(klim) & kvals >= min(klim)]
       xvals = c(smallestx - (heatkeywidth * kdim) + (2 * heatkeywidth), smallestx - ((heatkeywidth * kdim)/2)+heatkeywidth, smallestx - heatkeywidth)

       text(xvals, 
            yrange[1] - 2,
            formatC(kvals),
            xpd=TRUE, adj=1, cex=0.8)
   } else {
       if (missing(labels) || is.null(labels))
           labels <- 1:kdim

       text(xleft[2] - 1.3 * diff(xleft),
           xleft[-1] - 0.1*diff(xleft[1:2]),
           sort(labels),
           xpd = TRUE, adj=1, cex=0.8)
  }

  # Y-Axis Names
  # Heat key name at bottom of range
  if (!missing(heatkeyname) && !is.null(heatkeyname)) {
    text(smallestx - (heatkeywidth * kdim) - 3, yrange[1] - 0.5, heatkeyname, xpd = TRUE, adj = 1, cex = cex)
  }
  # Heat key name at top of range
  if (!missing(heatkeyname2) && !is.null(heatkeyname2)) {
    text(smallestx - (heatkeywidth * kdim) - 3, yrange[2] + 0.5, heatkeyname2, xpd = TRUE, adj = 1, cex = cex)
  }

  # X-Axis Name
  if (!missing(heatkeynamex) && !is.null(heatkeynamex)) {
    text(smallestx - ((heatkeywidth * kdim) / 2), yrange[1] - 3.5, heatkeynamex, xpd = TRUE, adj = 0.5, cex = 0.8)
  }

}

### Plot an SOM with a blended RGB color scheme mimicking a two-color fluorescence image where
### hue relates relative counts in class1 vs class2 and alpha relates total number
### of counts between classes. Valid values for color1 and color2 are "red" "green" or "blue".
### Don't use the same color for both -- that would be silly! (red/red actually forced to red/green,
### green/green to green/red and blue/blue to blue/red in practice)
### Added by Adam Diehl
plot.kohpropblend = function (x, property1, property2, main, color1, color2, ncolors, alpha = FALSE, zlim,
    heatkeytype = "2d", contin, contin2, keepMargins, heatkeywidth, heatkeyname1, heatkeyname2, alphatrans = FALSE,
    fg = "black", ...)
{
    if (missing(main) || is.null(main))
        main <- "Property plot"

    if (!(heatkeytype %in% c("left", "right", "both", "2d"))) {
        warning("plot.kohpropblend: Warning: invalid argument to heatkeytype. Using default value: left")
	heatkeytype = "left"
    }

    if (alpha && heatkeytype != "2d" && heatkeytype != "both") {
        warning("plot.kohblend: Warning: heatkeytype \"2d\" or \"both\" are strongly recommended when alpha = TRUE")
    }

    margins <- rep(0.6, 4)
    if (heatkeytype == "left" || heatkeytype == "both")
        margins[2] <- margins[2] + 4
    if (heatkeytype == "right" || heatkeytype == "both")
        margins[4] <- margins[4] + 4
    if (heatkeytype == "2d")
	margins[2] = margins[2] + 10
    if (main != "")
        margins[3] <- margins[3] + 2
    if (!keepMargins) {
        opar <- par("mar")
        on.exit(par(mar = opar))
    }
    par(mar = margins)
    plot(x$grid)
    title.y <- max(x$grid$pts[, 2]) + 2
    if (title.y > par("usr")[4] - 0.2) {
        title(main)
    }
    else {
        text(mean(range(x$grid$pts[, 1])), title.y, main, adj = 0.5,
            cex = par("cex.main"), font = par("font.main"))
    }

    # Prepare alpha vector, normalized to 0-1
    label = "Count"
    counts = property1 + property2
    if (alphatrans == TRUE) {  # Log-tranform counts before computing alpha
        counts = log2(counts)
        label = "Log2 Count"
    }
    alphavals = counts
    alphavals = (alphavals / max(alphavals, na.rm = TRUE))

    # Normalize the paired vector values to ranges between 0 and 1
    for (i in 1:nrow(x$grid$pts)) {
        if ( !is.na(property1[i]) ) {
	    sum = property1[i] + property2[i]
	    property1[i] = property1[i] / sum
	    property2[i] = property2[i] / sum
	}
    }

    # Might not be necessary
    if (missing(zlim) || is.null(zlim))
        zlim <- range(property1, property2, finite = TRUE)

    # Also might not be necessary
    if (missing(ncolors))
        ncolors <- min(length(unique(property1[!is.na(property1)])),
            length(unique(property2[!is.na(property2)])), 20)

    # Colors for heat key
    bgcol <- colorRampPalette(c(color1, color2))(ncolors)

    # Colors for alpha key
    alphacol <- colorRampPalette(c("white", "black"))(ncolors)

    bgcolors <- rep("#e6e6e6", nrow(x$grid$pts))

    showcolors <- as.integer(cut(property1, seq(zlim[1], zlim[2],
        length = ncolors + 1), include.lowest = TRUE))

    
    # Set RGB and Alpha for all grid cells
    for (i in 1:nrow(x$grid$pts)) {
    	if ( !is.na(showcolors[i]) ) {
	     if (alpha == TRUE) {

	         if (color1 == "red") {
    		     if (color2 == "blue") {
                         bgcolors[i] = rgb(property1[i], 0, property2[i], alphavals[i])
	             } else {
		         bgcolors[i] = rgb(property1[i], property2[i], 0, alphavals[i])
		     }
		 } else if (color1 == "green") {
		     if	 (color2 == "blue") {
                         bgcolors[i] = rgb(0, property1[i], property2[i], alphavals[i])
      	             } else {
 			 bgcolors[i] = rgb(property2[i], property1[i], 0, alphavals[i])
                     }
		 } else {
		     if  (color2 == "green") {
                         bgcolors[i] = rgb(0, property2[i], property1[i], alphavals[i])
                     } else {
                         bgcolors[i] = rgb(property2[i], 0, property1[i], alphavals[i])
                     }
		 }
             } else {
	          if (color1 == "red") {
                     if	     (color2   == "blue") {
                         bgcolors[i] = rgb(property1[i], 0, property2[i])
	             } else {
                         bgcolors[i] = rgb(property1[i], property2[i], 0)
                     }
                 } else if (color1 == "green") {
                     if  (color2 == "blue") {
                         bgcolors[i] = rgb(0, property1[i], property2[i])
                     } else {
                         bgcolors[i] = rgb(property2[i], property1[i], 0)
                     }
                 } else	{
                     if  (color2 == "green") {
                         bgcolors[i] = rgb(0, property2[i], property1[i])
                     } else {
                         bgcolors[i] = rgb(property2[i], 0, property1[i])
                     }
                 }   
 	     }
	}     
    }

    if (x$grid$topo == "rectangular") {

        symbols(x$grid$pts[, 1], x$grid$pts[, 2], circles = rep(0.5,
            nrow(x$grid$pts)), inches = FALSE, add = TRUE, fg = fg,
            bg = "transparent")

    } else {

        for (i in 1:nrow(x$grid$pts)) {
            if ( is.na(showcolors[i]) ) {
                hexagon(x$grid$pts[i, 1], x$grid$pts[i, 2], bg = "#e6e6e6", fg = "transparent")
            } else {
                hexagon(x$grid$pts[i, 1], x$grid$pts[i, 2], bg = bgcolors[i], fg = fg)
            }
        }
    }

       # Standard left heat key
    if (missing(contin))
        contin <- !(length(unique(property1)) < 10)
    if (heatkeytype == "left" || heatkeytype == "both") {
        if (length(unique(property1)) < 10 & !contin) {
            plot.heatkey(x, zlim, bgcol, labels = levels(as.factor(property1)),
                         contin = contin, heatkeywidth = heatkeywidth, heatkeyname = heatkeyname1, heatkeyname2 = heatkeyname2,
                         position = "left", ...)
        } else {
            plot.heatkey(x, zlim, bgcol, labels = NULL, contin = contin,
           		 heatkeywidth = heatkeywidth, heatkeyname = heatkeyname1, heatkeynametop = heatkeyname2,
			 position = "left", ...)
        }
	if (heatkeytype == "both") {
	   # Add right heatmap
	    if (length(unique(property1)) < 10 & !contin) {
                plot.heatkey(x, zlim = c(min(counts, na.rm = true), max(counts, na.rm = TRUE)), alphacol, labels = levels(as.factor(property1)),
                  	     contin = contin2, heatkeywidth = heatkeywidth, heatkeyname = label, position = "right", ...)
            } else {
                plot.heatkey(x, zlim = c(min(counts, na.rm = true), max(counts, na.rm = TRUE)), alphacol, labels = NULL, contin = contin2,
                             heatkeywidth = heatkeywidth, heatkeyname = label, position = "right", ...)
            }
	}
    } else if (heatkeytype == "right") {
         if (length(unique(property1)) < 10 & !contin) {
            plot.heatkey(x, zlim, bgcol, labels = levels(as.factor(property1)),
                         contin = contin, heatkeywidth = heatkeywidth, heatkeyname = heatkeyname1, heatkeyname2 = heatkeyname2,
                         position = "right", ...)
        } else {
            plot.heatkey(x, zlim, bgcol, labels = NULL, contin = contin,
           		 heatkeywidth = heatkeywidth, heatkeyname = heatkeyname1, heatkeynametop = heatkeyname2,
			 position = "right", ...)
        }
    }

    # Two-dimensional for alpha == TRUE
    if (heatkeytype == "2d") {

        if (missing(contin2)) {
	   contin2 <- !(length(unique(counts)) < 10)
        }
       if (length(unique(property1)) < 10 & !contin) {
           plot.heatkey.2d(x, zlim = zlim, klim = c(min(counts, na.rm = true), max(counts, na.rm = TRUE)), kdim = min(ncolors, max(counts, na.rm = TRUE)), bgcol,
       	   labels = levels(as.factor(property1)), continy = contin, continx = contin2, heatkeywidth = heatkeywidth, heatkeyname = heatkeyname1, heatkeyname2 = heatkeyname2,
   	   heatkeynamex = label, ...)
       } else {
           plot.heatkey.2d(x, zlim = zlim, klim = c(min(counts, na.rm = true), max(counts, na.rm = TRUE)), kdim = min(ncolors, max(counts, na.rm = TRUE)), bgcol,
           labels = NULL, continy = contin, continx = contin2, heatkeywidth = heatkeywidth, heatkeyname = heatkeyname1, heatkeyname2 = heatkeyname2,
           heatkeynamex = label, ...)
       }
    }

    invisible()
}

### Plot classifications of SOM neurons according to an external annotation of text labels.
### Added by Adam Diehl
plot.somclasses <- function(x, property, main, palette.name = NULL, ncolors,
                            zlim = NULL, heatkey = TRUE, contin = FALSE,
			    keepMargins = TRUE, heatkeywidth=0.5, fg = "black", ...)
{
  if (is.null(main)) main <- "SOM Classes Plot"
  if (is.null(palette.name)) palette.name <- heat.colors

  margins <- rep(0.6, 4)
  if (heatkey) margins[2] <- margins[2] + 4
  if (main != "") margins[3] <- margins[3] + 2
  if (!keepMargins) {
    opar <- par("mar")
    on.exit(par(mar = opar))
  }
  par(mar = margins)

  plot(x$grid, ...)
  title.y <- max(x$grid$pts[,2]) + 1.2
  if (title.y > par("usr")[4] - .2) {
    title(main)
  } else {
    text(mean(range(x$grid$pts[,1])),
         title.y,
         main, adj = .5, cex = par("cex.main"),
         font = par("font.main"))
  }

  if (is.null(zlim))
    zlim <- c(1, length(unique(property[!is.na(property)])))

  if (missing(ncolors))
    ncolors <- length(unique(property[!is.na(property)]))

  bgcol <- palette.name(ncolors)
  bgcolors <- rep("#e6e6e6", nrow(x$grid$pts))

  if (zlim[1] != zlim[2]) {
    # This chokes if all values in property are equal (e.g., all are 0)!
    showcolors <- as.integer(factor(property, levels = unique(property[!is.na(property)])))
  } else {
    showcolors <- property
    showcolors[!is.na(property)] <- 1
  }
  bgcolors[!is.na(showcolors)] <- bgcol[showcolors[!is.na(showcolors)]]

  if (x$grid$topo == "rectangular") {
    symbols(x$grid$pts[, 1], x$grid$pts[, 2],
          circles = rep(0.5, nrow(x$grid$pts)), inches = FALSE,
          add = TRUE, fg = fg, bg = bgcolors)
  } else {
    for(i in 1:nrow(x$grid$pts)) {
        if (is.na(showcolors[i])) {
            hexagon(x$grid$pts[i, 1], x$grid$pts[i, 2],bg="#e6e6e6", fg="transparent")
        } else {
            hexagon(x$grid$pts[i, 1], x$grid$pts[i, 2],bg=bgcolors[i],fg=fg)
        }
    }
  }

  if (heatkey) {
      plot.heatkey.somclasses(x, zlim, bgcol, labels = unique(property[!is.na(property)]),
                              heatkeywidth = heatkeywidth, ...)
  }

  invisible()
}

### Plot a key of discrete color labels for the plot.somclasses function.
### Added by Adam Diehl
plot.heatkey.somclasses = function(x, zlim, bgcol, labels, heatkeywidth = 0.5, ...) {
    ncolors = length(bgcol)

    yrange <- range(x$grid$pts[, 2])
    smallestx <- min(x$grid$pts[,1])
    xleft <- c(smallestx - heatkeywidth, smallestx) - 1
    yleft <- seq(yrange[1] - 0.5,
                 yrange[2] + 0.5,
                 length = ncolors + 1)
    rect(xleft[1], yleft[1:ncolors],
         xleft[2], yleft[2:(ncolors + 1)],
         border = "black", col = bgcol,
         xpd = TRUE)

    cex <- list(...)$cex

    text(xleft[2] - 1.3 * diff(xleft),
         yleft[-1] - 0.5*diff(yleft[1:2]),
         labels,
         xpd = TRUE, adj=1, cex=cex)
}

### Get a set of z limits centered around zero
### Added by Adam Diehl
get_zero_centered_zlim = function(x) {
    lim = max(c(abs(max(x, na.rm = TRUE)), abs(min(x, na.rm = TRUE))))
    return(c(-lim, lim))
}

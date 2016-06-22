\name{plot.kohonen}
\alias{plot.kohonen}
\alias{plot.somgrid}
\alias{plot.kohchanges}
\alias{plot.kohcounts}
\alias{plot.kohcodes}
\alias{plot.kohprop}
\alias{plot.kohquality}
\alias{plot.heatkey}
\alias{plot.kohmapping}
\alias{plot.kohUmatrix}
\alias{add.cluster.boundaries}
\title{Plot kohonen object}
\description{
  Plot self-organising map, obtained from function kohonen. Several types
  of plots are supported.
}
\usage{
\method{plot}{kohonen}(x, type = c("codes", "changes", "counts",
                 "dist.neighbours", "mapping", "property", "quality",
		 "classes", "property2", "propblend", "somclasses"),
     classif = NULL, labels = NULL, pchs = NULL, main = NULL,
     palette.name = NULL, palette.name2 = NULL, ncolors, bgcol = NULL,
     zlim = NULL, heatkey = TRUE, heatkey2 = TRUE, property, property2,
     contin, contin2, whatmap = NULL, codeRendering = NULL,
     keepMargins = FALSE, scale = FALSE, heatkeywidth = .2,
     heatkeyposition = c("left", "right"), heatkeyname = NULL,
     heatkeyname2 = NULL, heatkeytype = c("left", "right", "both",
     "2d"), color1 = c("red", "green", "blue"), color2 = c("red",
     "green", "blue"), alphatrans = FALSE, alpha = FALSE,
     fg = "black", \dots)
add.cluster.boundaries(x, cluster, lwd = 5, ...)
}
\arguments{
  \item{x}{kohonen object.}
  \item{type}{type of plot. (Wow!)}
  \item{classif}{classification object, as returned by
    \code{predict.kohonen}, or vector of unit numbers.
    Only needed if \code{type} equals "mapping" and
    "counts".}
  \item{labels}{labels to plot when \code{type} equals
    "mapping".}
  \item{pchs}{symbols to plot when \code{type} equals
    "mapping".}
  \item{main}{title of the plot.}
  \item{palette.name}{colors to use as unit background for "codes",
    "counts", "prediction", "property", "quality", "property2",
    and "somclasses" plotting types.}
  \item{ncolors}{number of colors to use for the unit
    backgrounds. Default is 20 for continuous data, and the number of
    distinct values (if less than 20) for categorical data.}
  \item{bgcol}{optional argument to colour the unit backgrounds for the
    "mapping" and "codes" plotting type. Defaults to "gray" and
    "transparent" in both types, respectively.}
  \item{zlim}{optional range for color coding of unit backgrounds.}
  \item{heatkey}{whether or not to generate a heatkey at the left side
    of the plot in the "property", "property2", "propblend",
    "somclasses", and "counts" plotting types.}
  \item{heatkey2}{whether or not to generate a heatkey at the right side
    of the plot for the second property in the "property2" plotting type.}
  \item{heatkeyposition}{Location of heatkey relative to the plot for 
    "property", "property2", "propblend", "somclasses", and "counts"
    plotting types..}
  \item{heatkeyname}{Label for heat key corresponding to property 1,
    plotted on the left in the "property2" plotting type, and at the bottom
    of the range in "propblend" plots.}
  \item{heatkeyname2}{Label for heat key corresponding to property 2,
    plotted on the right in the "property2" plotting type, and at the top
    of the range in "propblend" plots.}
  \item{heatkeytype}{Type of heatkey(s) to generate for the "propblend"
    plotting type.}
  \item{heatkeywidth}{width of the colour key; the default of 0.2 should
    work in most cases but in some cases, e.g. when plotting multiple
    figures, it may need to be adjusted.}
  \item{property}{values to use with the "property", "property2", and
    "propblend" plotting types.}
  \item{property2}{values to use for the second property vector for
    the "property2", and "propblend" plotting types.}
  \item{contin}{whether or not the data should be seen as discrete
    (i.e. classes) or continuous in nature. Only relevant for the colour
    keys of plots of supervised networks.}
  \item{contin2}{whether or not the data for property 2 should be seen as
    discrete (i.e. classes) or continuous in nature. Only relevant for
    the colour keys of plots of supervised networks.}
  \item{whatmap}{For \code{supersom} maps and a "codes" plot: what maps
    to show.}
  \item{codeRendering}{How to show the codes. Possible choices:
    "segments", "stars" and "lines".}
  \item{keepMargins}{if \code{FALSE} (the default), restore the original
    graphical parameters after plotting the kohonen map. If \code{TRUE},
    one retains the map coordinate system so that one can add symbols to
    the plot, or map unit numbers using the \code{identify} function.}
  \item{color1}{(RGB) Color to represent property 1 in "propblend" plots.}
  \item{color2}{(RGB) Color to represent property 2 in "propblend" plots.}
  \item{alpha}{For the "propblend" plot type, set shading density in each
    cell to represent the number of observations assigned to it.}
  \item{alphatrans}{For the "propblend" plot type with alpha = TRUE,
    calculate alpha based on log-transformed counts}
  \item{cluster}{cluster labels of the map units.}
  \item{fg}{Color to use for cell borders within the plot}
  \item{lwd, \dots}{other graphical parameters.}}

\details{Several different types of plots are supported:
  \describe{
    \item{"changes"}{shows the mean distance to the closest codebook vector
      during training.} 
    \item{"codes"}{shows the codebook vectors.}
    \item{"counts"}{shows the number of objects mapped to the
      individual units. Empty units are depicted in gray.}
    \item{"dist.neighbours"}{shows the sum of the distances to all
      immediate neighbours. This kind of visualisation is also known as
      a U-matrix plot. Units near a class boundary can be expected to
      have higher average distances to their neighbours. Only available
      for the "som" and "supersom" maps, for the moment.}
    \item{"mapping"}{shows where objects are mapped. It
      needs the "classif" argument, and a "labels" or "pchs"
      argument.}
    \item{"property"}{properties of each unit can be calculated and
      shown in colour code. It can be used to visualise the similarity
      of one particular object to all units in the map, to show the mean
      similarity of all units and the objects mapped to them,
      etcetera. The parameter \code{property} contains the numerical
      values. See examples below.}
    \item{"quality"}{shows the mean distance of objects mapped to a
      unit to the codebook vector of that unit. The smaller the
      distances, the better the objects are represented by the codebook
      vectors.}
    \item{"clases"}{Plots individual pie charts showing the contribution
      of each class in the SOM to each neuron.}
    \item{"property2"}{Similar to "property", but two properties can be
      showns simultaneously within each plot cell in divided hexagons.}
    \item{"propblend"}{Represents the relative contribution of two
      properties to each neuron as the blend of two RGB hues, with
      optional transparency according to the number of observations
      assigned to each neuron.}
    \item{"somclasses"}{Plot color codes for a vector of labels
      corresponding to each neuron and an accompanying heatkey with
      text labels for each class}
  }
  Function \code{add.cluster.boundaries} will add to an existing plot of
  a map thick lines, visualizing which units would be clustered together.
}

\author{Ron Wehrens, Alan Boyle, Hennig Rust, Adam Diehl}
\seealso{\code{\link{som}}, \code{\link{bdk}}, \code{\link{xyf}}}

\examples{
data(wines)
set.seed(7)

kohmap <- xyf(scale(wines), classvec2classmat(wine.classes),
              grid = somgrid(5, 5, "hexagonal"), rlen=100)
plot(kohmap, type="changes")
plot(kohmap, type="codes", main = c("Codes X", "Codes Y"))
plot(kohmap, type="counts")

## palette suggested by Leo Lopes
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}
plot(kohmap, type="quality", palette.name = coolBlueHotRed)
plot(kohmap, type="mapping", 
     labels = wine.classes, col = wine.classes+1,
     main = "mapping plot")

## add background colors to units according to their predicted class labels
xyfpredictions <- classmat2classvec(predict(kohmap)$unit.predictions)
bgcols <- c("gray", "pink", "lightgreen")
plot(kohmap, type="mapping", col = wine.classes+1,
     pchs = wine.classes, bgcol = bgcols[as.integer(xyfpredictions)], 
     main = "another mapping plot")

## Another way to show clustering information
set.seed(7)
sommap <- som(scale(wines),grid = somgrid(6, 4, "hexagonal"))
plot(sommap, type="dist.neighbours", main = "SOM neighbour distances")
## use hierarchical clustering to cluster the codebook vectors
som.hc <- cutree(hclust(dist(sommap$codes)), 5)
add.cluster.boundaries(sommap, som.hc)

## and the same for rectangular maps
set.seed(7)
sommap <- som(scale(wines),grid = somgrid(6, 4, "rectangular"))
plot(sommap, type="dist.neighbours", main = "SOM neighbour distances")
## use hierarchical clustering to cluster the codebook vectors
som.hc <- cutree(hclust(dist(sommap$codes)), 5)
add.cluster.boundaries(sommap, som.hc)
}
\keyword{classif}

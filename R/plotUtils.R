p.abline <- function(intercept=0, slope=1, ...) {
    panel.levelplot(...)
    panel.abline(intercept, slope)
}

kinla_plot <- function(x, fe=F, lc=F, re=F, hp=F, pre=F, q=F, cpo=F, pri=F, ...) {
    plot(x, plot.fixed.effects = fe,
           plot.lincomb = lc, 
           plot.random.effects = re, 
           plot.hyperparameters = hp, 
           plot.predictor = pre, 
           plot.q = q, 
           plot.cpo = cpo, 
           plot.prior = pri, ...)
}

ktheme <- list(axis.line = list(col = "transparent"),
               clip=list(panel="off"),
               layout.heights=list(main.key.padding=-2),
               layout.widths=list(axis.key.padding=0))

par_dark <- list(bg='#2E3440', fg="#A2BF8A", cex=.7, bty='L', pch=16, lwd=1.2, col.main="#A2BF8A", 
  col.axis="#D8DEE9", col.lab="#D8DEE9", mar=c(5,4,1,1))

#' Plot text
#'
#' @export
plotm <- function(order=1, ...) {
  if (order==1) 
    plotl(..., col=order, lty=order) 
  else 
    lines(..., col=order, lty=order)
}

#' Plot text
#'
#' @export
bg <- function(col='Orange',...)
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = col,...)

#' Plot text
#'
#' @export
plott = function(...) txtplot::txtplot(...)

#' Plot density in terminal
#'
#' @export
histt = function(...) txtplot::textdensity(...)

#' Plot bar chart in terminal
#'
#' @export
barplott = function(...) txtplot::txtbarchart(...)

#' Plot segments
#'
#' plot segment based on output from boxplot
#' @export
plot_segment = function(x, y, add=T, ...) {
  bl = boxplot(y ~ x, plot=F)
  if (add) 
    points(as.numeric(bl$names), bl$stats[3, ],...)
  else
    plotp(as.numeric(bl$names), bl$stats[3, ],...)
  # segments(as.numeric(bl$names), bl$stats[1, ],,bl$stats[5, ],...)
  segments(as.numeric(bl$names), bl$stats[2, ],,bl$stats[4, ],...)
}

#' panel.smooth
#'
#' Panel for plotting pair()
#' @export
panel.smooth <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...) {
    Kgrid()
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) 
        lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
            col = col.smooth, ...)
}
#' panel.hist
#'
#' Panel for plotting pair()
#' @export
panel.hist <- function(x, ...) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, ...)
}
#' panel.cor
#'
#' Panel for plotting pair()
#' @export
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
    usr <- par("usr"); on.exit(par(usr))
    Kgrid()
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}
#' panel.cor.robust
#'
#' Panel for plotting pair()
#' @export
panel.cor.robust <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
    usr <- par("usr"); on.exit(par(usr))
    Kgrid()
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y, method="spearman"))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}
#' panel.text
#'
#' Panel for plotting pair()
#' @export
panel.text <- function(x, y, ...) {
    usr <- par("usr"); on.exit(par(usr))
    Kgrid()
    par(usr = c(0, 1, 0, 1))
    txt <- toupper(names(x))
    text(0.5, 0.5, txt)
}
#' s2n - String to numeric
#'
#' Converting string to ACSII number, to use with plotting parameter `pch`
#' @export
s2n <- function(x = "string") {
  if (!is.character(x)) stop("Enclosing the input in double quote!")
  num <- c(33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126)
  char <- c("!", "\"", "#", "$", "%", "&", "'", "(", ")", "*", "+", ",", "-", ".", "/", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ":", ";", "<", "=", ">", "?", "@", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "[", "\\", "]", "^", "_", "`", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "{", "|", "}", "~")
  return(num[match(x, char)])
}

## ------------------------------------------------------------------------
#' Brackets
#'
#' Add brackets for object for labels. 
#' @export
bracket <- function(x, type = c("curve", "square", "curly")) {
  type. = match.arg(type)
  switch(type., 
         curve = paste("(", x, ")", sep = ""),
         square = paste("[", x, "]", sep = ""),
         curve = paste("{", x, "}", sep = ""))
}
## ------------------------------------------------------------------------
#' labelOutlier
#'
#' Add labels (id) for data points with farthest \code{mahalanobis} distance. 
#' @export
labelOutlier <- function(x, y = NULL, alpha = 0.01, at = 3, n = 5, ...) {
  if (!is.null(y)) xy <- cbind(x, y) else xy <- x
  if (dim(xy)[2] != 2) stop("Only possible on two dimension")
  xy.dist <- round(mahalanobis(xy, colMeans(xy), cov=var(xy)))
  topn <- min(tail(sort(xy.dist), n))
  topn <- which(xy.dist >= topn)
  text(xy[topn, 1], xy[topn, 2], label = topn, pos = at, xpd = TRUE, ...)
}

kPars <- function(nrow=1, ncol=1,...) {
  par(mfrow = c(nrow, ncol),
      mar   = c(2,2,1,1)+0.1,
      mgp   = c(1.1,.2,0),
      tcl   = -.1,
      font.main = 1,
      cex.main  = .9,
      cex.axis=.7, 
      cex.lab=.8, 
      xaxs='r', 
      yaxs='r',
      ...)  
}

# --------------------------------------------------------------------------
#' put
#'
#' @export
put <- function(n.row, n.col, mar = NULL, ...) {
  if (is.null(mar)) 
    kPars(n.row, n.col)
  else 
    par(mfrow = c(n.row, n.col), ...)
  palette(zurich)
} 

## -------------------------------------------------------------------------
#' Kgrid function
#'
#' Add background and grid lines similar to ggplot.
#' @param bg (string) Background color Defaults to "gray90".
#' @param cols (string) Gridlines color  Defaults to "white".
#' @keywords grid
#' @export
#' @examples
#' Kgrid()
# Kgrid(bg = "white", cols = AddAlpha("skyblue"))
# Kgrid <- function(bg = AddAlpha('steelblue', .05), cols = "white") {
Kgrid <- function(bg = NA, cols = "gray93" ) {
    bg(col = bg, border = NA)
    xaxp <- par("xaxp"); yaxp <- par("yaxp")
    Vvec <- seq(xaxp[1], xaxp[2], (xaxp[2]-xaxp[1])/xaxp[3])
    Hvec <- seq(yaxp[1], yaxp[2], (yaxp[2]-yaxp[1])/yaxp[3])
    vvec <- seq(xaxp[1] + diff(Vvec)[1]/2, xaxp[2], by = abs(diff(Vvec)[1]))
    hvec <- seq(yaxp[1] + diff(Hvec)[1]/2, yaxp[2], by = abs(diff(Hvec)[1]))
    abline(v=Vvec, h = Hvec, lty=1, col = cols, lwd = 1)
    abline(v=vvec, h = hvec, lty=1, col = cols, lwd = .5)
}
# --------------------------------------------------------------------------
#' savePNG function
#'
#' Add background and grid lines similar to ggplot.
#' @param bg (string) Background color Defaults to "gray90".
#' @param cols (string) Gridlines color  Defaults to "white".
#' @keywords grid
#' @export
#' @examples
#' savePNG()
savePNG <- function(name="name", w = dev.size()[1], h = dev.size()[2]) {
  dev.copy(png, paste(name, "png", sep="."), width=w, height=h, res=300, units="in")
  dev.off()
}
# --------------------------------------------------------------------------
#' savePDF function
#'
#' Add background and grid lines similar to ggplot.
#' @param bg (string) Background color Defaults to "gray90".
#' @param cols (string) Gridlines color  Defaults to "white".
#' @keywords grid
#' @export
#' @examples
#' savePDF()
savePDF <- function(name="name", w = dev.size()[1], h = dev.size()[2]) {
  dev.copy(pdf, paste(name, "pdf", sep="."), width=w, height=h)
  dev.off()
}
# --------------------------------------------------------------------------
#' savePs function
#'
#' Add background and grid lines similar to ggplot.
#' @param bg (string) Background color Defaults to "gray90".
#' @param cols (string) Gridlines color  Defaults to "white".
#' @keywords grid
#' @export
#' @examples
#' savePs()
savePs <- function(name="name", w = dev.size()[1], h = dev.size()[2]) {
  dev.copy(png, paste(name, "png", sep="."), width=w, height=h, res=300, units="in")
  dev.off()
  dev.copy(pdf, paste(name, "pdf", sep="."), width=w, height=h)
  dev.off()
  # dev.copy(postscript, paste(name, "eps", sep="."), width=w, height=h, paper = "special", horizontal = FALSE, onefile = FALSE)
  dev.copy(cairo_ps, paste(name, "eps", sep="."), width=w, height=h, fallback_resolution = 300, onefile = FALSE)
  dev.off()
}
# --------------------------------------------------------------------------
#' PlotData function
#'
#' Add background and grid lines similar to ggplot.
#' @param bg (string) Background color Defaults to "gray90".
#' @param cols (string) Gridlines color  Defaults to "white".
#' @keywords grid
#' @export
#' @examples
#' PlotData()
PlotData <- function(data, origin = TRUE, ...) {
    plot(times, log10(Data0[, "V"]), type = "n", ylim = c(0, 7),
        ylab = expression(paste(log[10], "(Viral load)") ),
        xlab = "Day post infection (dpi)", las = 1, axes=FALSE, ...)
    Kgrid(); Kaxis(); Kaxis(2)
    if (origin) lines(times, log10(Data0[, "V"]), lwd = 1, lty = 2, col = 1)
    points(data[,"time"], data[,"V"], col = AddAlpha(1, 0.7), pch = 20)
    abline(h = log10(50), lty = 3, col = "dimgray", lwd = 1)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], log10(50), col = AddAlpha("dimgray", 0.2), border = NA)
    text(4, 0.2, "Undetectable level")
}
# --------------------------------------------------------------------------
#' AddAlpha 
#'
#' @export
AddAlpha <- function (plotclr, alpha = 0.5, verbose = 0) {
    tmp <- col2rgb(plotclr, alpha = alpha)
    tmp[4, ] = round(alpha * 255)
    for (i in 1:ncol(tmp)) {
        plotclr[i] = rgb(tmp[1, i], tmp[2, i], tmp[3, i], tmp[4, 
            i], maxColorValue = 255)
    }
    return(plotclr)
}
# --------------------------------------------------------------------------
#' Kolygon 
#'
#' @export
Kolygon <- function(x, y, ylow = NULL, drawline=FALSE,
                    col = 'gray93', alpha = 0.2, ...) {
  # if (is.null(border)) border <- col
  if (missing(y)) {
    y <- x
    x <- seq(length(y))
  }
  xx <- c(x, rev(x))
  if (is.null(ylow)) yy <- c(y, rep(0, length(y)))
    else yy <- c(y, rev(ylow))
  polygon(xx, yy, col = AddAlpha(col, alpha),...)
  if (drawline) {
    lines(x, y, col=col, lty=2)
    lines(x, ylow, col=col, lty=2)
  }
}
# --------------------------------------------------------------------------
#' Kaxis 
#'
#' @export
Kaxis <- function(side = 1, col='gray93', colticks='dimgray', ...) {
    axis(side, col=col, col.ticks=colticks, ...)
}
# --------------------------------------------------------------------------  ineplotl 
#'
#' @export
plotl <- function(..., grid=FALSE, autoax = TRUE) {
  plot(..., type = "n", axes = FALSE)
  if (grid) Kgrid()
  if (autoax) {
    x_ <- list(...)[[1]]
    x_names <- names(x_)
    if (!is.null(x_names))
      Kaxis(1, at=x_, labels=x_names)
    else
      Kaxis(1)
    Kaxis(2)
  }
  lines(...)
}
# --------------------------------------------------------------------------
#' hplot 
#'
#' @export
ploth <- function(..., grid=FALSE, autoax = TRUE) {
  plot(..., type = "n", axes = FALSE)
  if (grid) Kgrid()
  if (autoax) {
    Kaxis(1); Kaxis(2)
  }
  lines(..., type = "h")
  points(...)
}
# --------------------------------------------------------------------------
#' blankplot 
#'
#' @export
plotb <- function(..., autoax = TRUE) {
  plot(..., type = "n", axes = FALSE)
  Kgrid()
  if (autoax) {
    Kaxis(1); Kaxis(2) 
  }
}
# --------------------------------------------------------------------------
#' pointplot 
#'
#' @export
plotp <- function(..., grid=FALSE, autoax = TRUE) {
  plot(..., type = "n", axes = FALSE)
  if (grid) Kgrid()
  if (autoax) {
    Kaxis(1); Kaxis(2)
  }
  user <- any(names(list(...)) %in% c("col", "cex", "pch"))
  if (user)  points(...) 
  else points(..., col = "gray30", cex = 0.5, pch = 20)
}
# --------------------------------------------------------------------------
#' xyline 
#'
#' @export
xyline <- function(x, y, ...) {
  abline(v = x, h = y, ...)
}
# --------------------------------------------------------------------------
#' bothplot 
#'
#' @export
plotlnp <- function(..., autoax = TRUE) {
  plot(..., type = "n", axes = FALSE)
  Kgrid()
  if (autoax) {
    Kaxis(1); Kaxis(2)
  }
  lines(..., type = "b")
}
# --------------------------------------------------------------------------
#' vline 
#'
#' @export
vline <- function(v = 0, ...) abline(v = v, lty = 3, ...)
# --------------------------------------------------------------------------
#' hline 
#'
#' @export
hline <- function(h = 0, ...) abline(h = h, lty = 3, ...)
# --------------------------------------------------------------------------
#' logplot 
#'
#' @export
logplot <- function(x, y, log = c("x", "y", "xy"), base10 = FALSE, ...) {
  logwhat <- match.arg(log)
  if (!base10) {
    switch(logwhat, 
         x = plot(log(x), y, ...), 
         y = plot(x, log(y), ...), 
         xy = plot(log(x), log(y), ...))
  } else {
    switch(logwhat, 
         x = plot(log10(x), y, ...), 
         y = plot(x, log10(y), ...), 
         xy = plot(log10(x), log10(y), ...))
  }
} 
# --------------------------------------------------------------------------
#' hlegend 
#'
#' @export
hlegend <- function(...) legend(..., horiz = TRUE, bty = 'n')
# --------------------------------------------------------------------------

#' genCols 
#'
#' @export
gen_brewer <- function(n, c_pallete = "Set1") {
  pals <- try(RColorBrewer::brewer.pal(8, c_pallete))
  if (inherits(pals, 'try-error')) pals <- c_pallete
  getCols <- colorRampPalette(pals)
  return(getCols(n))
}

#' genCols 
#'
#' @export
gen_colors <- function(c_pallete = kg, n) {
  colorRampPalette(c_pallete)(n)
}

#' lo 
#'
#' @export
plot_smooth = function(fml, dt, add=TRUE, ...) {
  l = loess(as.formula(fml), dt, ...)
  x = seq(min(l$x), max(l$x), length.out=1000)
  p = predict(l, newdata=x, se=TRUE)
  u = p$fit + p$se.fit
  l = p$fit - p$se.fit
  if (add)
    lines(x, p$fit, ...)
  else 
    plotl(x, p$fit, ...)
  Kolygon(x, u, l, ...)
}

#' @export 
area_plot <- function(..., add=FALSE, autoax = TRUE) {
  if (!add) {
    plot(..., type = "n", axes = FALSE)
    Kgrid()
  }
  Kolygon(...)
  if (autoax) {
    Kaxis(1); Kaxis(2, las = 2) 
  }
}

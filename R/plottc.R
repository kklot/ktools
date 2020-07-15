#' plot in terminal with color
#' 
#' 
#' @export
plottc = function (x, y = NULL, pch = "*", col='green', width = round(options()$width *
    0.8), height = round(0.25 * width), xlab = NULL, ylab = NULL,
    xlim = NULL, ylim = NULL)
{
    require(crayon)
    require(txtplot)
    if (!is.null(y)) {
        if (length(x) != length(y))
            stop("x and y need to have same length")
    }
    else {
        y <- x
        x <- 1:length(y)
    }
    if (!is.numeric(x) | !is.numeric(y))
        stop("x and y need to be of type numeric")
    indx <- txtplot:::checkValid(x)
    indy <- txtplot:::checkValid(y)
    x <- x[indy & indx]
    y <- y[indy & indx]
    if (is.null(xlim)) {
        rngx <- txtplot:::getRng(x)
    }
    else {
        if (length(xlim) != 2)
            stop("need vector of length 2 for xlim")
        if (!is.numeric(xlim))
            stop("xlim needs to be a numeric")
        ind <- x >= xlim[1] & x <= xlim[2]
        x <- x[ind]
        y <- y[ind]
        rngx <- xlim
    }
    if (is.null(ylim)) {
        rngy <- txtplot:::getRng(y)
    }
    else {
        if (length(ylim) != 2)
            stop("need vector of length 2 for ylim")
        if (!is.numeric(ylim))
            stop("ylim needs to be a numeric")
        ind <- y >= ylim[1] & y <= ylim[2]
        x <- x[ind]
        y <- y[ind]
        rngy <- ylim
    }
    xticks <- txtplot:::getTicks(rngx)
    yticks <- txtplot:::getTicks(rngy)
    xtkch <- formatC(xticks, digits = 5, width = -1)
    ytkch <- formatC(yticks, digits = 5, width = -1)
    lmar <- max(nchar(ytkch)) + 2 + 2 * as.numeric(!is.null(ylab))
    nch <- nchar(xtkch[length(xtkch)])
    if (is.element(nch, c(3, 4))) {
        rmar <- 1
    }
    else {
        if (nch == 5)
            rmar <- 2
        else rmar <- 0
    }
    pwid <- width - lmar - rmar
    if (!is.null(xlab)) {
        if (!is.character(xlab))
            stop("xlab needs to be a character vector")
        xlab <- substr(xlab, 1, pwid)
        bmar <- 3
    }
    else {
        bmar <- 2
    }
    phgt <- height - bmar
    if (!is.null(ylab)) {
        if (!is.character(ylab))
            stop("ylab needs to be a character vector")
        ylab <- substr(ylab, 1, phgt)
    }
    if (pwid < 6)
        stop(paste("effective plotting width <6 characters",
            sep = ""))
    if (phgt < 6)
        stop(paste("effective plotting heigth <6 characters",
            sep = ""))
    xdelta <- diff(rngx)
    ydelta <- diff(rngy)
    ch <- character((width + 1) * height)
    ch[1:((width + 1) * height)] <- " "
    ch[(1:height) * (width + 1)] <- "\n"
    ind <- c(lmar:width, (phgt * (width + 1) + lmar):(phgt *
        (width + 1) + width))
    ch[ind] <- "-"
    ind <- c(lmar + (0:phgt) * (width + 1), width + (0:phgt) *
        (width + 1))
    ch[ind] <- "|"
    ind <- c(lmar, width, phgt * (width + 1) + lmar, phgt * (width +
        1) + width)
    ch[ind] <- "+"
    xtck <- round((xticks - rngx[1])/xdelta * pwid)
    ind <- c(lmar + xtck, phgt * (width + 1) + lmar + xtck)
    ch[ind] <- "+"
    ytck <- round((yticks - rngy[1])/ydelta * phgt)
    ind <- c(lmar + (phgt - ytck) * (width + 1), width + (phgt -
        ytck) * (width + 1))
    ch[ind] <- "+"
    indx <- (phgt + 1) * (width + 1) + lmar + xtck
    xl <- strsplit(xtkch, NULL)
    for (i in 1:length(xl)) {
        ln <- length(xl[[i]])
        fln <- floor(ln/2)
        z <- 1
        shift <- 0
        if (i == 1 & (indx[i] - fln <= (phgt + 1) * (width +
            1))) {
            shift <- indx[i] - fln - ((phgt + 1) * (width + 1) +
                1)
        }
        if (i == length(xl) & (indx[i] + fln > (phgt + 1) * (width +
            1) + width + 1)) {
            shift <- indx[i] + fln - ((phgt + 1) * (width + 1) +
                width)
        }
        for (j in (-fln:fln) - shift) {
            if (!(ln%%2) & j == fln - shift)
                break
            ch[indx[i] + j] <- xl[[i]][z]
            z <- z + 1
        }
    }
    indy <- lmar + (phgt - ytck) * (width + 1)
    yl <- strsplit(ytkch, NULL)
    for (i in 1:length(yl)) {
        ln <- length(yl[[i]])
        z <- 1
        for (j in -((ln + 1):2)) {
            ch[indy[i] + j] <- yl[[i]][z]
            z <- z + 1
        }
    }
    if (!is.null(xlab)) {
        xlabpos <- round(0.5 * (rngx[2] - rngx[1])/xdelta * pwid)
        indxlab <- (phgt + 2) * (width + 1) + lmar + xlabpos
        xl <- strsplit(xlab, NULL)
        ln <- length(xl[[1]])
        fln <- floor(ln/2)
        z <- 1
        for (j in -fln:fln) {
            if (!(ln%%2) & j == fln)
                break
            ch[indxlab + j] <- xl[[1]][z]
            z <- z + 1
        }
    }
    if (!is.null(ylab)) {
        ylabpos <- round(0.5 * (rngy[2] - rngy[1])/ydelta * phgt)
        indylab <- 1 + (phgt - ylabpos) * (width + 1)
        yl <- strsplit(ylab, NULL)
        ln <- length(yl[[1]])
        fln <- floor(ln/2)
        z <- 1
        for (j in -fln:fln) {
            if (!(ln%%2) & j == fln)
                break
            ch[indylab + j * (width + 1)] <- yl[[1]][z]
            z <- z + 1
        }
    }
    xplt <- round((x - rngx[1])/xdelta * pwid)
    yplt <- round((y - rngy[1])/ydelta * phgt)
    ind <- (phgt - yplt) * (width + 1) + lmar + xplt
    ch[ind] <- pch
    FUN <- match.fun(col)
    cat(FUN(ch), sep = "")
}
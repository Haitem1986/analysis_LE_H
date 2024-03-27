windRose2 <- function (mydata, ws = "ws", wd = "wd", ws2 = NA, wd2 = NA, ws.int = 2, 
                       angle = 30, type = "default", bias.corr = TRUE, cols = "default", 
                       grid.line = NULL, width = 1, seg = NULL, auto.text = TRUE, 
                       breaks = 4, offset = 10, normalise = FALSE, max.freq = NULL, 
                       paddle = TRUE, key.header = NULL, key.footer = "(m/s)", key.position = "bottom", 
                       key = TRUE, dig.lab = 5, include.lowest = FALSE, statistic = "prop.count", 
                       pollutant = NULL, annotate = TRUE, angle.scale = 315, border = NA, 
                       alpha = 1, plot = TRUE, ...) 
{
  if (is.null(seg)) 
    seg <- 0.9
  if (length(cols) == 1 && cols == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))
    calm.col <- "black"
  }
  else {
    calm.col <- "black"
  }
  current.strip <- trellis.par.get("strip.background")
  current.font <- trellis.par.get("fontsize")
  on.exit(trellis.par.set(fontsize = current.font))
  mydata <- openair:::checkNum(mydata, vars = c(ws, wd))
  if (360/angle != round(360/angle)) {
    warning("In windRose(...):\n  angle will produce some spoke overlap", 
            "\n  suggest one of: 5, 6, 8, 9, 10, 12, 15, 30, 45, etc.", 
            call. = FALSE)
  }
  if (angle < 3) {
    warning("In windRose(...):\n  angle too small", "\n  enforcing 'angle = 3'", 
            call. = FALSE)
    angle <- 3
  }
  extra <- list(...)
  extra$xlab <- if ("xlab" %in% names(extra)) {
    quickText(extra$xlab, auto.text)
  }
  else {
    quickText("", auto.text)
  }
  extra$ylab <- if ("ylab" %in% names(extra)) {
    quickText(extra$ylab, auto.text)
  }
  else {
    quickText("", auto.text)
  }
  extra$main <- if ("main" %in% names(extra)) {
    quickText(extra$main, auto.text)
  }
  else {
    quickText("", auto.text)
  }
  if ("fontsize" %in% names(extra)) {
    trellis.par.set(fontsize = list(text = extra$fontsize))
  }
  if (is.character(statistic)) {
    ok.stat <- c("prop.count", "prop.mean", "abs.count", 
                 "frequency")
    if (!is.character(statistic) || !statistic[1] %in% ok.stat) {
      warning("In windRose(...):\n  statistic unrecognised", 
              "\n  enforcing statistic = 'prop.count'", call. = FALSE)
      statistic <- "prop.count"
    }
    if (statistic == "prop.count") {
      stat.fun <- length
      stat.unit <- "%"
      stat.scale <- "all"
      stat.lab <- "Frequency of counts by wind direction (%)"
      stat.fun2 <- function(x) format(mean(x, na.rm = TRUE), 
                                      digits = dig.lab)
      stat.lab2 <- "mean"
      stat.labcalm <- function(x) round(x, 1)
    }
    if (statistic == "prop.mean") {
      stat.fun <- function(x) sum(x, na.rm = TRUE)
      stat.unit <- "%"
      stat.scale <- "panel"
      stat.lab <- "Proportion contribution to the mean (%)"
      stat.fun2 <- function(x) format(mean(x, na.rm = TRUE), 
                                      digits = 5)
      stat.lab2 <- "mean"
      stat.labcalm <- function(x) round(x, 1)
    }
    if (statistic == "abs.count" | statistic == "frequency") {
      stat.fun <- length
      stat.unit <- ""
      stat.scale <- "none"
      stat.lab <- "Count by wind direction"
      stat.fun2 <- function(x) round(length(x), 0)
      stat.lab2 <- "count"
      stat.labcalm <- function(x) round(x, 0)
    }
  }
  if (is.list(statistic)) {
    stat.fun <- statistic$fun
    stat.unit <- statistic$unit
    stat.scale <- statistic$scale
    stat.lab <- statistic$lab
    stat.fun2 <- statistic$fun2
    stat.lab2 <- statistic$lab2
    stat.labcalm <- statistic$labcalm
  }
  vars <- c(wd, ws)
  diff <- FALSE
  rm.neg <- TRUE
  if (!is.na(ws2) & !is.na(wd2)) {
    vars <- c(vars, ws2, wd2)
    diff <- TRUE
    rm.neg <- FALSE
    mydata$ws <- mydata[[ws2]] - mydata[[ws]]
    mydata$wd <- mydata[[wd2]] - mydata[[wd]]
    id <- which(mydata$wd < 0)
    if (length(id) > 0) 
      mydata$wd[id] <- mydata$wd[id] + 360
    pollutant <- "ws"
    key.footer <- "ws"
    wd <- "wd"
    ws <- "ws"
    vars <- c("ws", "wd")
    if (missing(angle)) 
      angle <- 10
    if (missing(offset)) 
      offset <- 20
    if (is.na(breaks[1])) {
      max.br <- max(ceiling(abs(c(min(mydata$ws, na.rm = TRUE), 
                                  max(mydata$ws, na.rm = TRUE)))))
      breaks <- c(-1 * max.br, 0, max.br)
    }
    if (missing(cols)) 
      cols <- c("lightskyblue", "tomato")
    seg <- 1
  }
  if (any(type %in% dateTypes)) 
    vars <- c(vars, "date")
  if (!is.null(pollutant)) 
    vars <- c(vars, pollutant)
  mydata <- cutData(mydata, type, ...)
  mydata <- checkPrep(mydata, vars, type, remove.calm = FALSE, 
                      remove.neg = rm.neg)
  mydata_orig <- mydata
  id <- which(is.na(mydata[[ws]]))
  if (length(id) > 0) {
    mydata <- mydata[-id, ]
  }
  if (is.null(pollutant)) 
    pollutant <- ws
  mydata$x <- mydata[[pollutant]]
  mydata[[wd]] <- angle * ceiling(mydata[[wd]]/angle - 0.5)
  mydata[[wd]][mydata[[wd]] == 0] <- 360
  mydata[[wd]][mydata[, ws] == 0] <- -999
  if (length(breaks) == 1) 
    breaks <- 0:(breaks - 1) * ws.int
  if (max(breaks) < max(mydata$x, na.rm = TRUE)) {
    breaks <- c(breaks, max(mydata$x, na.rm = TRUE))
  }
  if (min(breaks) > min(mydata$x, na.rm = TRUE)) {
    warning("Some values are below minimum break.")
  }
  breaks <- unique(breaks)
  mydata$x <- cut(mydata$x, breaks = breaks, include.lowest = include.lowest, 
                  dig.lab = dig.lab)
  labs <- gsub("[(]|[)]|[[]|[]]", "", levels(mydata$x))
  labs <- gsub("[,]", " to ", labs)
  prepare.grid <- function(mydata) {
    if (all(is.na(mydata$x))) {
      weights <- tibble(Interval1 = NA, wd = NA, calm = 100, 
                        panel.fun = NA, mean.wd = NA, freqs = NA)
    }
    else {
      levels(mydata$x) <- c(paste("Interval", 1:length(labs), 
                                  sep = ""))
      all <- stat.fun(mydata[[wd]])
      calm <- mydata[mydata[[wd]] == -999, ][[pollutant]]
      calm <- stat.fun(calm)
      weights <- tapply(mydata[[pollutant]], list(mydata[[wd]], 
                                                  mydata$x), stat.fun)
      freqs <- tapply(mydata[[pollutant]], mydata[[wd]], 
                      length)
      if (stat.scale == "all") {
        calm <- calm/all
        weights <- weights/all
      }
      if (stat.scale == "panel") {
        temp <- stat.fun(stat.fun(weights)) + calm
        calm <- calm/temp
        weights <- weights/temp
      }
      weights[is.na(weights)] <- 0
      weights <- t(apply(weights, 1, cumsum))
      if (stat.scale == "all" | stat.scale == "panel") {
        weights <- weights * 100
        calm <- calm * 100
      }
      panel.fun <- stat.fun2(mydata[[pollutant]])
      u <- mean(sin(2 * pi * mydata[[wd]]/360), na.rm = TRUE)
      v <- mean(cos(2 * pi * mydata[[wd]]/360), na.rm = TRUE)
      mean.wd <- atan2(u, v) * 360/2/pi
      if (all(is.na(mean.wd))) {
        mean.wd <- NA
      }
      else {
        if (mean.wd < 0) 
          mean.wd <- mean.wd + 360
        if (mean.wd > 180) 
          mean.wd <- mean.wd - 360
      }
      weights <- bind_cols(as_tibble(weights), tibble(wd = as.numeric(row.names(weights)), 
                                                      calm = calm, panel.fun = panel.fun, mean.wd = mean.wd, 
                                                      freqs = freqs))
    }
    weights
  }
  if (paddle) {
    poly <- function(wd, len1, len2, width, colour, x.off = 0, 
                     y.off = 0) {
      theta <- wd * pi/180
      len1 <- len1 + off.set
      len2 <- len2 + off.set
      x1 <- len1 * sin(theta) - width * cos(theta) + x.off
      x2 <- len1 * sin(theta) + width * cos(theta) + x.off
      x3 <- len2 * sin(theta) - width * cos(theta) + x.off
      x4 <- len2 * sin(theta) + width * cos(theta) + x.off
      y1 <- len1 * cos(theta) + width * sin(theta) + y.off
      y2 <- len1 * cos(theta) - width * sin(theta) + y.off
      y3 <- len2 * cos(theta) + width * sin(theta) + y.off
      y4 <- len2 * cos(theta) - width * sin(theta) + y.off
      lpolygon(c(x1, x2, x4, x3), c(y1, y2, y4, y3), col = colour, 
               border = border)
    }
  }
  else {
    poly <- function(wd, len1, len2, width, colour, x.off = 0, 
                     y.off = 0) {
      len1 <- len1 + off.set
      len2 <- len2 + off.set
      theta <- seq((wd - seg * angle/2), (wd + seg * angle/2), 
                   length.out = (angle - 2) * 10)
      theta <- ifelse(theta < 1, 360 - theta, theta)
      theta <- theta * pi/180
      x1 <- len1 * sin(theta) + x.off
      x2 <- rev(len2 * sin(theta) + x.off)
      y1 <- len1 * cos(theta) + x.off
      y2 <- rev(len2 * cos(theta) + x.off)
      lpolygon(c(x1, x2), c(y1, y2), col = colour, border = border)
    }
  }
  results <- mydata %>% group_by(across(type)) %>% do(prepare.grid(.))
  results$calm <- stat.labcalm(results$calm)
  results$mean.wd <- stat.labcalm(results$mean.wd)
  corr_bias <- function(results) {
    wd_select <- inner_join(mydata_orig, results[1, type], 
                            by = type)
    if (!all(round(wd_select[[wd]])%%10 == 0, na.rm = TRUE)) 
      return(results)
    wds <- seq(10, 360, 10)
    tmp <- angle * ceiling(wds/angle - 0.5)
    id <- which(tmp == 0)
    if (length(id > 0)) 
      tmp[id] <- 360
    tmp <- table(tmp)
    vars <- grep("Interval[1-9]", names(results))
    n_data <- nrow(filter(results, wd != -999))
    if (n_data > 0) {
      results[results[["wd"]] != -999, vars] <- results[results[["wd"]] != 
                                                          -999, vars] * mean(tmp)/tmp
    }
    return(results)
  }
  if (bias.corr) {
    results <- results %>% group_by(across(type)) %>% do(corr_bias(.))
  }
  strip.dat <- strip.fun(results, type, auto.text)
  strip <- strip.dat[[1]]
  strip.left <- strip.dat[[2]]
  pol.name <- strip.dat[[3]]
  if (length(labs) < length(cols)) {
    col <- cols[1:length(labs)]
  }
  else {
    col <- openColours(cols, length(labs))
  }
  legend_col <- col
  col <- grDevices::adjustcolor(col, alpha.f = alpha)
  if (normalise) {
    vars <- grep("Interval[1-9]", names(results))
    results$freq <- results[[max(vars)]]
    results$freq <- ave(results$freq, results[type], FUN = function(x) x/sum(x))
    results$norm <- results$freq/max(results$freq)
    results[, vars] <- results[, vars]/results[[max(vars)]]
    stat.lab <- "Normalised by wind sector"
    stat.unit <- ""
  }
  if (is.null(max.freq)) {
    max.freq <- max(results[results$wd != -999, grep("Interval", 
                                                     names(results))], na.rm = TRUE)
  }
  else {
    max.freq <- max.freq
  }
  off.set <- max.freq * (offset/100)
  box.widths <- seq(0.002^0.25, 0.016^0.25, length.out = length(labs))^4
  box.widths <- box.widths * max.freq * angle/5
  legend <- list(col = legend_col, space = key.position, auto.text = auto.text, 
                 labels = labs, footer = key.footer, header = key.header, 
                 height = 0.6, width = 1.5, fit = "scale", plot.style = if (paddle) "paddle" else "other")
  legend <- makeOpenKeyLegend(key, legend, "windRose")
  temp <- paste(type, collapse = "+")
  myform <- formula(paste("Interval1 ~ wd | ", temp, sep = ""))
  mymax <- max(pretty(c(0, max.freq), 4))
  grid.value <- NULL
  if (is.list(grid.line)) {
    if (is.null(grid.line[["value"]])) {
      grid.value <- NULL
    }
    else {
      grid.value <- grid.line[["value"]]
    }
    if (is.null(grid.line[["lty"]])) {
      grid.lty <- 1
    }
    else {
      grid.lty <- grid.line[["lty"]]
    }
    if (is.null(grid.line[["col"]])) {
      grid.col <- "grey85"
    }
    else {
      grid.col <- grid.line[["col"]]
    }
  }
  else {
    grid.value <- grid.line
    grid.lty <- 1
    grid.col <- "grey85"
  }
  myby <- if (is.null(grid.value)) 
    pretty(c(0, mymax), 4)[2]
  else grid.value
  if (myby/mymax > 0.9) 
    myby <- mymax * 0.9
  is_annotated <- !(annotate %in% c(FALSE, NA, NaN)) && !is.null(annotate)
  if (is_annotated) 
    sub <- stat.lab
  else sub <- NULL
  xy.args <- list(x = myform, xlim = 1.03 * c(-max.freq - off.set, 
                                              max.freq + off.set), ylim = 1.03 * c(-max.freq - off.set, 
                                                                                   max.freq + off.set), data = results, type = "n", sub = sub, 
                  strip = strip, strip.left = strip.left, as.table = TRUE, 
                  aspect = 1, par.strip.text = list(cex = 0.8), scales = list(draw = FALSE), 
                  panel = function(x, y, subscripts, ...) {
                    panel.xyplot(x, y, ...)
                    angles <- seq(0, 2 * pi, length = 360)
                    sapply(seq(off.set, mymax + off.set, by = myby), 
                           function(x) llines(x * sin(angles), x * cos(angles), 
                                              col = grid.col, lwd = 1, lty = grid.lty))
                    dat <- results[subscripts, ]
                    dat <- filter(dat, wd <= 360, wd >= 0)
                    upper <- max.freq + off.set
                    if (nrow(dat) > 0) {
                      dat$Interval0 <- 0
                      for (i in 1:nrow(dat)) {
                        for (j in seq_along(labs)) {
                          tmp <- paste("poly(dat$wd[i], dat$Interval", 
                                       j - 1, "[i], dat$Interval", j, "[i], width * box.widths[", 
                                       j, "], col[", j, "])", sep = "")
                          eval(parse(text = tmp))
                        }
                      }
                    }
                    if (normalise) {
                      panel.wdprob(dat, seg, angle, off.set)
                    }
                    ltext(seq((myby + off.set), (mymax + off.set), myby) * 
                            sin(pi * angle.scale/180), seq((myby + off.set), 
                                                           (mymax + off.set), myby) * cos(pi * angle.scale/180), 
                          paste(seq(myby, mymax, by = myby), stat.unit, 
                                sep = ""), cex = 0.7)
                    if (annotate[1] == TRUE || length(annotate) == 2L || 
                        annotate[1] == " ") {
                      if (annotate[1] == TRUE) {
                        annotations_to_place <- paste0(stat.lab2, " = ", 
                                                       dat$panel.fun[1], "\n", "calm = ", dat$calm[1], 
                                                       stat.unit)
                      }
                      if (annotate[1] == " ") {
                        annotations_to_place <- paste0(stat.lab2, " = ", 
                                                       dat$panel.fun[1])
                      }
                      if (length(annotate) == 2L) {
                        annotations_to_place <- paste0(annotate[1], 
                                                       " = ", dat$panel.fun[1], "\n", annotate[2], 
                                                       " = ", dat$calm[1], stat.unit)
                      }
                      if (diff) {
                        annotate <- c("mean_ws", "mean_wd")
                        annotations_to_place <- paste0(mean_ws = paste("mean ws = ", 
                                                                       round(as.numeric(dat$panel.fun[1]), 1)), 
                                                       "\n", mean_wd = paste("mean wd = ", round(dat$mean.wd[1], 
                                                                                                 1)))
                      }
                      ltext(max.freq + off.set, -max.freq - off.set, 
                            label = annotations_to_place, adj = c(1, 0), 
                            cex = 0.7, col = calm.col)
                      lsegments(-upper, 0, upper, 0)
                      lsegments(0, -upper, 0, upper)
                      if (!is.na(ws2) & !is.na(wd2)) {
                        axislabs <- c("0", "+90", paste0("+/-", 180), 
                                      "-90")
                        s_adj <- 0.1
                      } else {
                        axislabs <- c("N", "E", "S", "W")
                        s_adj <- 0.07
                      }
                      ltext(upper * -1 * 0.95, 0.07 * upper, axislabs[4], 
                            cex = 0.7)
                      ltext(s_adj * upper, upper * -1 * 0.95, axislabs[3], 
                            cex = 0.7)
                      ltext(0.07 * upper, upper * 0.95, axislabs[1], 
                            cex = 0.7)
                      ltext(upper * 0.95, 0.07 * upper, axislabs[2], 
                            cex = 0.7)
                    }
                  }, legend = legend)
  xy.args <- listUpdate(xy.args, extra)
  plt <- do.call(xyplot, xy.args)
  if (plot) {
    if (length(type) == 1) {
      plot(plt)
    }
    else {
      plt <- useOuterStrips(plt, strip = strip, strip.left = strip.left)
      plot(plt)
    }
  }
  newdata <- as_tibble(results)
  attr(newdata, "intervals") <- labs
  output <- list(plot = plt, data = newdata, call = match.call())
  class(output) <- "openair"
  invisible(output)
}
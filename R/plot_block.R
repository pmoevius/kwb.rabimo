# plot_block -------------------------------------------------------------------
plot_block <- function(block, cex = 1, delta = 0.1)
{
  fraction <- create_fraction_accessor(block)

  r <- list(
    main = rectangle(
      height = block$areaFractionMain,
      name = "areaFractionMain"
    ),
    road = rectangle(
      height = block$areaFractionRoad,
      name = "areaFractionRoad"
    ),
    mbs = rectangle(
      height = fraction("main/builtSealed"),
      value = block$mainFractionBuiltSealed,
      name = "mainFractionBuiltSealed"
    ),
    mus = rectangle(
      height = fraction("main/unbuiltSealed"),
      value = block$mainFractionUnbuiltSealed,
      name = "mainFractionUnbuiltSealed"
    ),
    rrs = rectangle(
      height = fraction("road/roadSealed"),
      value = block$roadFractionRoadSealed,
      name = "roadFractionRoadSealed"
    ),
    bsc = rectangle(
      width = block$builtSealedFractionConnected,
      value = block$builtSealedFractionConnected,
      height = fraction("main/builtSealed"),
      name = "builtSealedFractionConnected"
    ),
    usc = rectangle(
      width = block$unbuiltSealedFractionConnected,
      value = block$unbuiltSealedFractionConnected,
      height = fraction("main/unbuiltSealed"),
      name = "unbuiltSealedFractionConnected"
    ),
    rsc = rectangle(
      width = block$roadSealedFractionConnected,
      value = block$roadSealedFractionConnected,
      height = fraction("road/roadSealed"),
      name = "roadSealedFractionConnected"
    )
  )

  x <- filter_elements(block, "roadSealedFractionSurface")

  r <- c(r, lapply(names(x), function(name) {
    rectangle(
      width = x[[name]],
      height = fraction("road/roadSealed"),
      label = right(name, 1L)
    )
  }))

  x <- filter_elements(block, "unbuiltSealedFractionSurface")

  r <- c(r, lapply(names(x), function(name) {
    rectangle(
      width = x[[name]],
      height = fraction("main/unbuiltSealed"),
      label = right(name, 1L)
    )
  }))

  {
    graphics::par(mar = c(2, 2, 1, 1))

    limits <- get_bounding_box(r, delta = delta)

    init_plot(xlim = c(0, 4), ylim = c(-1, 2))

    # 1st column
    plot.rectangles(r[2:1], cex = cex)

    # 2nd column
    p1 <- plot.rectangles(
      r[2:1],
      x = 1.2,
      delta = delta,
      label = FALSE,
      lty = 2 # dashed
    )

    p2 <- plot.rectangles(
      r[3:4],
      x = 1.2,
      y = p1$ytop[2],
      reverse = TRUE,
      cex = cex
    )

    p3 <- plot.rectangles(
      r[5],
      x = 1.2,
      y = p1$ybottom[1],
      cex = cex
    )

    # 3rd column
    p4a <- plot.rectangles(
      r[4],
      x = 2.4,
      y = p2$ybottom[2],
      delta = 1,
      cex = cex,
      label = FALSE,
      lty = 2 # dashed
    )

    p4b <- plot.rectangles(
      r[3],
      x = 1.2,
      y = p2$ytop[1L] + delta,
      delta = 1,
      cex = cex,
      label = FALSE,
      lty = 2
    )

    p5 <- plot.rectangles(
      r[5],
      x = 2.4,
      y = p1$ybottom[1],
      label = FALSE,
      lty = 2
    )

    plot.rectangles(
      r[6],
      x = 1.2,
      y = p4b$ybottom[1],
      cex = cex,
      label_left = TRUE,
      col = "lightgrey"
    )

    p7 <- plot.rectangles(
      r[7],
      x = 2.4,
      y = p4a$ybottom[1],
      cex = cex,
      label_left = TRUE,
      col = "lightgrey"
    )

    plot.rectangles(
      r[8],
      x = 2.4,
      y = p5$ybottom,
      label_left = TRUE,
      cex = cex,
      col = "lightgrey"
    )

    plot.rectangles(
      r[9:12],
      x = 2.4,
      y = - r[[9]]$height - delta/2,
      horizontal = TRUE,
      cex = cex
    )

    plot.rectangles(
      r[13:16],
      x = 2.4,
      y = p7$ytop + delta/2,
      horizontal = TRUE,
      cex = cex
    )

  }

  r
}

# plot.rectangles --------------------------------------------------------------
plot.rectangles <- function(
    r,
    x = 0,
    y = 0,
    delta = 0,
    label = TRUE,
    label_left = FALSE,
    cex = 1,
    horizontal = FALSE,
    reverse = FALSE,
    ...,
    add = TRUE
)
{
  if (!add) {
    init_plot(r, delta)
  }

  h <- get_heights(r)
  w <- get_widths(r)

  vertical <- !horizontal

  positions <- stacked_positions(
    x = if (horizontal) w else h,
    delta = delta,
    reverse = reverse
  )

  lowers <- positions[, "lower"]
  uppers <- positions[, "upper"]

  xleft   <- x + if (vertical) 0 else lowers
  ybottom <- y + if (vertical) lowers else 0
  xright  <- x + if (vertical) w else uppers
  ytop    <- y + if (vertical) uppers else h

  graphics::rect(
    xleft = xleft,
    ybottom = ybottom,
    xright = xright,
    ytop = ytop,
    ...
  )

  if (label) {
    midx <- (xleft + xright)/2
    midy <- (ybottom + ytop)/2
    graphics::text(
      if (label_left) xleft else midx,
      midy,
      labels = get_labels(r),
      cex = cex,
      adj = if (label_left) 0 else NULL
    )
  }

  invisible(list(
    xleft = xleft,
    ybottom = ybottom,
    xright = xright,
    ytop = ytop
  ))
}

# init_plot --------------------------------------------------------------------
init_plot <- function(
    width = 1,
    height = 1,
    xlim = c(0, width),
    ylim = c(0, height),
    axes = FALSE
)
{
  plot(
    NA,
    xlim = xlim,
    ylim = ylim,
    xlab = "",
    ylab = "",
    asp = 1,
    axes = axes
  )
}

# get_heights ------------------------------------------------------------------
get_heights <- function(r)
{
  sapply(r, select_elements, "height")
}

# get_widths -------------------------------------------------------------------
get_widths <- function(r)
{
  sapply(r, select_elements, "width")
}

# get_names --------------------------------------------------------------------
get_names <- function(r)
{
  sapply(r, select_elements, "name")
}

# get_labels -------------------------------------------------------------------
get_labels <- function(r)
{
  sapply(r, select_elements, "label")
}

# get_bounding_box -------------------------------------------------------------
get_bounding_box <- function(r, delta = 0)
{
  extra <- delta * (length(r) - 1)

  list(
    xlim = c(0, sum(get_widths(r)) + extra),
    ylim = c(0, sum(get_heights(r)) + extra)
  )
}

# stacked_positions ------------------------------------------------------------
stacked_positions <- function(x, delta = 0, reverse = FALSE)
{
  upper <- cumsum(x) + (seq_along(x) - 1L) * delta
  lower <- c(0, upper[-length(upper)] + delta)

  cbind(
    lower = if (reverse) -upper else lower,
    upper = if (reverse) -lower else upper
  )
}

# rectangle --------------------------------------------------------------------
rectangle <- function(
    width = 1, height = 1, name = "Name?", value = height, label = NULL
)
{
  name <- gsub("Fraction", "X", name)

  list(
    width = width,
    height = height,
    name = name,
    value = value,
    label = default_if_null(label, sprintf("%s: %0.1f", name, value * 100))
  )
}

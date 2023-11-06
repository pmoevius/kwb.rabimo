# Global variables -------------------------------------------------------------
UPPER_LIMIT_EYN <- 0.7

AA <- c(
  0.9946811499, #  0
  1.213648255,  #  1
  -1.350801214, #  2
  11.80883489,  #  3
  -21.53832235, #  4
  19.3775197,   #  5
  0.862954876,  #  6
  9.184851852,  #  7
  -147.2049991, #  8
  1291.164889,  #  9
  -6357.554955, # 10
  19022.42165,  # 11
  -35235.40521, # 12
  39509.02815,  # 13
  -24573.23867, # 14
  6515.556685   # 15
)

# yratio_cpp -------------------------------------------------------------------
yratio_cpp <- function(bag, x)
{
  stopifnot(length(bag) == 1L)

  ALMOST_ONE <- 0.99999
  ALMOST_ZERO <- 1e-7

  TWO_THIRDS <- 2/3
  ONE_THIRD <- 1/3

  {
    bag_plus_one <- bag + 1
    reciprocal_bag_plus_one <- 1 / bag_plus_one

    h13 <- exp(-bag_plus_one * 1.09861)
    h23 <- exp(-bag_plus_one * 0.405465)

    a2 <- -13.5 * reciprocal_bag_plus_one * (1.0 + 3.0 * (h13 - h23))
    a1 <- 9.0 * reciprocal_bag_plus_one * (h13 + h13 - h23) - TWO_THIRDS * a2

    a0 <- 1.0 / (1.0 - reciprocal_bag_plus_one - 0.5 * a1 - ONE_THIRD * a2)

    a1 <- a1 * a0
    a2 <- a2 * a0

    b <- ifelse(
      bag >= 0.49999,
      - sqrt(0.25 * a1 * a1 - a2) + 0.5 * a1,
      - sqrt(0.5 * a1 * a1 - a2)
    )

    c <- a1 - b
    a <- a0 / (b - c)

    # First usage of x
    epa <- exp(x / a)

    y0 <- pmin((epa - 1) / (b - c * epa), ALMOST_ONE)

    # If bag is between a certain range return y0
    if (bag >= 0.7 && bag <= 3.8) {
      return(y0)
    }
  }

  # NUMERISCHE INTEGRATION FUER BAG > 3.8 (3. Naeherungsloesung)
  if (bag > 3.8) {

    h <- rep(1.0, length(x))
    todo <- rep(TRUE, length(x))

    i <- 0L

    while(any(todo) && i < 15L) {
      y0[todo] <- pmin(y0[todo], 0.999)
      epa[todo] <- exp(bag * log(y0[todo]))
      h[todo] <- pmin(pmax(1.0 - epa[todo], ALMOST_ZERO), ALMOST_ONE)
      h[todo] <- h[todo] *
        (y0[todo] + epa[todo] * y0[todo] /
           (h[todo] - bag * epa[todo] / log(h[todo])) - x[todo])
      y0[todo] <- y0[todo] - h[todo]
      i <- i + 1L
      todo[todo] <- abs(h[todo]) > 0.001
    }

    # Return y0 (1.0 at maximum)
    return(pmin(y0, 1.0))
  }

  # NUMERISCHE INTEGRATION FUER BAG < 0.7 (2.Naeherungsloesung)
  y0 <- sapply(seq_along(x), function(i) {
    second_approximation(bag, xi = x[i], y0i = y0[i])
  })

  # Return y0 (1.0 at maximum)
  pmin(y0, 1.0)
}

# second_approximation ---------------------------------------------------------
second_approximation <- function(bag, xi, y0i, dbg = FALSE)
{
  stopifnot(length(bag) == 1L)
  stopifnot(length(y0i) == 1L)

  # NUMERISCHE INTEGRATION FUER BAG<0.7 (2.Naeherungsloesung)
  # j = 1

  while (TRUE) # j <= 30
  {
    eyn <- exp(bag * log(y0i))

    # If eyn, bag are in a certain range, return y0i (1.0 at maximum)
    if ((eyn > 0.9) || (eyn >= UPPER_LIMIT_EYN && bag > 4.0)) {
      return(min(y0i, 1.0))
    }

    # Set start and end index (?), depending on the value of eyn
    if (eyn > UPPER_LIMIT_EYN) {
      ia <- 8
      ie <- 16
    } else {
      ia <- 2
      ie <- 6
    }

    sum_1 <- 0.0
    sum_2 <- 0.0
    h <- 1.0

    # Let i loop between start index ia and end index ie
    for (i in ia:ie) {
      h <- h * eyn
      w <- AA[i] * h
      j <- i - ia + 1L
      sum_2 <- sum_2 + w / (j * bag + 1.0)
      sum_1 <- sum_1 + w
    }

    h <- AA[ia - 1L]
    h <- (xi - y0i * sum_2 - y0i * h) / (h + sum_1)

    y0i <- y0i + h

    # Break out of this loop if a condition is met
    kwb.utils::printIf(dbg, h)
    kwb.utils::printIf(dbg, y0i)

    if (abs(h) / y0i < 0.007) {
      break;
    }

    #j++
  }

  if (y0i > 0.9) {

    bagrov(bag, xi)

  } else {

    y0i
  }
}

# second_approximation_vectorised (not working!) -------------------------------
second_approximation_vectorised <- function(bag, x, y0)
{
  geth <- function(iak, iek, eynk, xk, y0k) {
    iseq <- iak:iek
    w <- AA[iseq]*(eynk^(seq_along(iseq)))
    sum_1 <- sum(w / (iseq * bag + 1.0))
    sum_2 <- sum(w)
    h <- AA[iak - 1L]
    (xk - y0k * sum_2 - y0k * h) / (h + sum_1)
  }

  (n <- length(x))
  (result <- numeric(n))
  (todo <- rep(TRUE, n))
  (eyn <- numeric(n))

  times <- 0L
  #while (j <= 30L)
  while (any(todo) && times < 30L) {

    times <- times + 1L
    print(times)

    eyn[todo] <- exp(bag * log(y0[todo]))
    eyn

    # If eyn, bag are in a certain range, return y0 (1.0 at maximum)
    (bigeyn <- eyn[todo] > 0.9) # | (eyn >= UPPER_LIMIT_EYN & bag > 4.0)

    result[todo][bigeyn] <- pmin(y0[todo][bigeyn], 1)
    todo[todo][bigeyn] <- FALSE

    result
    todo

    # Set start and end index (?), depending on the value of eyn
    (big <- (eyn[todo] > UPPER_LIMIT_EYN))

    # Let i loop between start index ia and end index ie
    (hvalues <- mapply(
      FUN = geth,
      iak = ifelse(big, 8L, 2L),
      iek = ifelse(big, 16L, 6L),
      eynk = eyn[todo],
      xk = x[todo],
      y0k = y0[todo]
    ))

    y0[todo] <- y0[todo] + hvalues
    y0

    # Break out of this loop if a condition is met
    (breakout <- abs(hvalues) / y0[todo] < 0.007)
    todo[todo][breakout] <- FALSE
    todo

    # j++
  }
}

# bagrov -----------------------------------------------------------------------
bagrov <- function(bagf, x0)
{
  #qDebug() << "In bagrov()...";

  i <- 0L

  if (x0 == 0.0) {
    return(0.0)
  }

  y0 <- 0.99

  doloop <- FALSE

  # NUMERISCHE INTEGRATION DER BAGROVBEZIEHUNG

  while (TRUE) {

    j <- 1L
    du <- 2.0 * y0
    h <- 1.0 + 1.0 / (1.0 -  exp(bagf * log(y0)))
    si <- h * du / 4.0
    sg <- 0.0
    su <- 0.0

    again <- TRUE

    while (again) {

      s <- si
      j <- j * 2L
      du <- du / 2.0
      sg <- sg + su

      su <- 0.0
      u <- du / 2.0

      ii <- 1L

      while (ii <= j) {
        su <- su + 1.0 / (1.0 -  exp(bagf * log(u)))
        u <- u + du
        ii <- ii + 2L
      }

      si <- (2.0 * sg + 4.0 * su + h) * du / 6.0

      again <- abs(s - si) > 0.001 * s
    }

    x <- si

    # ENDE DER NUMERISCHEN INTEGRATION
    skip <- FALSE

    if (doloop) {

      delta <- (x0 - x) * (1.0 -  exp(bagf *  log(y0)))
      y0 <- y0 + delta

      if (y0 >= 1.0) {
        y0 <- 0.99
        skip <- TRUE
      }

      if (!skip && y0 <= 0.0) {
        y0 <- 0.01
        skip <- TRUE
      }

      if (!skip && abs(delta) < 0.01) {
        return(y0)
      }

      if (i < 10L) {
        i <- i + 1L
        skip <- TRUE
      }

    } # end of if (doloop)

    if (!skip) {

      if (x0 > x) {
        return(1.0)
      }

      y0 <- 0.5
      i <- 1
    }

    # SCHLEIFE I = 1(1)10 ZUR BERECHNUNG VON DELTA
    doloop <- TRUE

  } # end of while (TRUE)

} # end of function

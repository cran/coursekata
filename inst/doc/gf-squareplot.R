## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4,
  dpi = 96
)

## ----setup, message = FALSE---------------------------------------------------
library(coursekata)

## ----basic--------------------------------------------------------------------
gf_squareplot(~Thumb, data = Fingers)

## ----bars-outline-------------------------------------------------------------
gf_squareplot(~Thumb, data = Fingers, bars = "outline")

## ----custom-------------------------------------------------------------------
gf_squareplot(~Thumb, data = Fingers,
              fill = "coral",
              binwidth = 5,
              xrange = c(30, 90))

## ----integer------------------------------------------------------------------
int_data <- data.frame(rolls = sample(1:6, 30, replace = TRUE))
gf_squareplot(~rolls, data = int_data)

## ----large-sample-------------------------------------------------------------
large_data <- data.frame(x = rnorm(500, mean = 50, sd = 10))
gf_squareplot(~x, data = large_data)

## ----mean-line----------------------------------------------------------------
gf_squareplot(~Thumb, data = Fingers, show_mean = TRUE)

## ----dgp, fig.height = 5------------------------------------------------------
set.seed(42)
samp_dist <- do(100) * b1(Thumb ~ Height, data = sample(Fingers, 30))
gf_squareplot(~b1, data = samp_dist,
              show_dgp = TRUE,
              show_mean = TRUE,
              xrange = c(-0.5, 1.5),
              xbreaks = seq(-0.5, 1.5, by = 0.25))

## ----factor-------------------------------------------------------------------
ratings <- factor(sample(1:5, 20, replace = TRUE, prob = c(1, 2, 4, 2, 1)),
                  levels = 1:5)
df <- data.frame(rating = ratings)
gf_squareplot(~rating, data = df)


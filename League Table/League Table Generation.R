##############################
# League Table & Funnel Plot #
##############################

# Adapted from:
# https://www.metafor-project.org/doku.php/plots:caterpillar_plot
# https://www.metafor-project.org/doku.php/plots:funnel_plot_variations

# install.packages("metafor")

library("metafor")

### simulate some data
set.seed(2020)
k <- 10
vi <- rchisq(k, df=10) * .1
vi
yi <- rnorm(k, rnorm(k, 0.5, 0.4), sqrt(vi))
yi


# Add list of names
names <- c(LETTERS[1:10])

### fit RE model
res <- rma(yi, vi)
res

### create plot
forest_plot <- forest(yi, vi,
                      xlim=c(0,3.5),        ### adjust horizontal plot region limits
                      order="obs",             ### order by size of yi
                      slab=names, annotate=FALSE, ### remove study labels and annotations
                      cex.lab=1, cex.axis=1,   ### increase size of x-axis title/labels
                      lty=c("solid","blank"))  ### remove horizontal line at top of plot





funnel_plot <- funnel(res, 
                      lty = "solid",
                      main="Standard Error")

citation(package='metafor') 

#####################
# Generic SPC Chart #
#####################
# install.packages("qicharts2")

# Adapted from: https://cran.r-project.org/web/packages/qicharts2/vignettes/qicharts2.html

library(qicharts2)

set.seed(2020)



# Generate 24 random numbers from a normal distribution.
y <- rnorm(24)

i_chart <- qic(y, chart = 'i')

i_chart
# I'm not a fan of the default aesthetic from qicharts, so we can make our own
library(ggplot2)

i_chart_redux <- ggplot(i_chart$data, aes(x = x, y = y)) +
                    geom_point() +
                    geom_line(aes(y = y)) +
                    geom_line(aes(y = cl)) +
                    geom_line(aes(y = lcl)) +
                    geom_line(aes(y = ucl)) +
                    labs(title = 'Example of I-Chart') +
                    theme_classic()
i_chart_redux

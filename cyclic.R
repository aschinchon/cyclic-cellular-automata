library(tidyverse)
library(Rcpp)
library(colourlovers)
library(reshape2)
library(cowplot)

# Import C++ code
sourceCpp('cyclic_funs.cpp')

#################################################################
# Functions
#################################################################

# This function creates a w x h matrix of random states
initial_grid <- function(s, w, h){
  matrix(sample(x = seq_len(s)-1,
               size = w *h,
               replace = TRUE),
         nrow = h,
         ncol = w)}

# This function implements neighborhoods
# You can add your own
convolution_indexes <- function(r, n){
  crossing(x = -r:r, y = -r:r) %>% 
    mutate(M = ((x != 0) | (y != 0)) * 1 ,
           N = (abs(x) + abs(y) <= r) * M,
           Mr = ((abs(x) == r) | (abs(y) == r)) * M,
           Nr = (abs(x) + abs(y) == r) * M,
           Cr = ((x == 0) | (y == 0)) * M,
           S1 = (((x > 0) & (y > 0))|((x < 0) & (y < 0))) * 1,
           Bl = (abs(x) == abs(y)) * M,
           D1 = (abs(x) > abs(y)) * M,
           D2 = ((abs(x) == abs(y)) | abs(x) == r) * M,
           C2 = M - N,
           Z = ((abs(y) == r) | (x == y)) * M,
           t = ((y == r) | (x == 0)) * M,
           U = ((abs(x) == r) | (y == -r)) * M,
           H = (abs(x) == r | y == 0) * M,
           TM = ((abs(x) == abs(y)) | abs(x) == r | abs(y) == r) * M,
           S2 = ((y==0) | ((x == r) & (y > 0)) |((x == -r) & (y < 0))) * M,
           M2 = ((abs(x) == r) | (abs(x) == abs(y) & y > 0)) * M) %>% 
    select(x, y, matches(n)) %>% 
    filter_at(3, all_vars(. > 0)) %>% 
    select(x,y)
}

#################################################################
# Initialization
#################################################################
range <- 5
thresold <- 29
states <- 5
neighborhood <- "M"
iter <- 600

width  <- 1500
height <- 1500
  
X <- initial_grid(s = states,
                  w = width,
                  h = height)

L <- convolution_indexes(r = range, n = neighborhood)
  
for (i in 1:iter){
    X <- iterate_cyclic(X, L, states, thresold)  
}
  
# Transform resulting environment matrix into data frame
df <- melt(X)
colnames(df) <- c("x","y","v") # to name columns
  
# Pick a top palette from colourlovers
palette <- sample(clpalettes('top'), 1)[[1]] 
colors <- palette %>% swatch %>% .[[1]]

# Do the plot
ggplot(data = df, aes(x = x, y = y, fill = v)) + 
  geom_raster(interpolate = TRUE) +
  coord_equal() +
  scale_fill_gradientn(colours = colors) +
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_continuous(expand = c(0,0)) +
  theme_nothing() 


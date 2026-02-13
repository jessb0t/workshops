# Description: color palettes and custom palette creation function
# Function inputs:
#  - name: palette name from my_colors
#  - type: continuous or discrete
#    - use 'continuous' with scale_*_gradientn(colors=my_colors(name, type))
#    - use 'discrete' with scale_*_manual(values=my_colors(name, type, n))
#  - n: for discrete palettes, the unique number of hex values to return
# Author: Jessica M. Alexander, based upon code published at {https://www.jumpingrivers.com/blog/custom-colour-palettes-for-ggplot2/}
# Last Updated: 2025-06-17

my_colors = list(
  contrast_three = c("#004488", "#BB5566", "#DDAA33"), #from ggpubfigs [https://github.com/JLSteenwyk/ggpubfigs]
  veronese = c("#67322e", "#99610a", "#c38f16", "#6e948c",
               "#2c6b67", "#175449", "#122c43"), #from MetBrewer [https://github.com/BlakeRMills/MetBrewer]
  ut_pop = c("#f8971f", "#005f86", "#00a9b7")  #from ut brand book [https://brand.utexas.edu/application/brand-book-toolkit/]
)


my_palettes = function(name, type = c("discrete", "continuous"), n, all_palettes = my_colors){
  palette = all_palettes[[name]]
  if (missing(n)) {
    n = length(palette)
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  structure(out, name = name, class = "palette")
}

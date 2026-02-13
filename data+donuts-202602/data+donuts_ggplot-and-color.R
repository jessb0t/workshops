# Data + Donuts: Working with Color in Data Visualizations Using R and ggplot2
# Author: Jessica M. Alexander (https://jessb0t.github.io/)
# Workshop Date: 2025-02-13
# Summary: This script uses the 'palmerpenguins' dataset to showcase the extensive control available
#          to customize color options in ggplot2.
# Learning Goals:
# 1) be able to manipulate colors of individual ggplot layers
# 2) be able to manipulate colors based on a fill/color specification in aes()
# 3) be aware of the myriad packages available to support selection of a color palette
# 4) be aware of the difference between discrete and continuous color palettes
# 5) be aware of how different color palettes affect the experience of individuals with color vision deficiency
# 6) be able to make your own color palette and use with a data plot


## EXAMPLE OF DATA STORYTELLING WITH COLOR
#https://github.com/jkaminags/TidyTuesday/blob/main/2025/attribution.jpeg


## SETTING UP
# install.packages("tidyverse") #alternately, you can install just ggplot2 and dplyr for this workshop
# install.packages("palmerpenguins") #our dataset for the workshop [https://allisonhorst.github.io/palmerpenguins/]
# install.packages("RColorBrewer") #a color palette extension for ggplot2 [https://r-graph-gallery.com/38-rcolorbrewers-palettes.html]
# install.packages("viridis")  #another color palette extension for ggplot2 [https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html]
# install.packages("cowplot") #original package from Claus O. Wilke, lots of subsequent packages depend on this [https://cran.r-project.org/web/packages/cowplot/vignettes/introduction.html]
# install.packages("colorspace") #another option that I am less familiar with (but it's a dependency of colorblindr!) [https://cran.r-project.org/web/packages/colorspace/vignettes/colorspace.html]
# install.packages("remotes") #allows you to install colorblindr via their suggested methodology
# remotes::install_github("clauswilke/colorblindr") #allows you to simulate various color-vision deficiencies for your plots [https://github.com/clauswilke/colorblindr]
# install.packages("MetBrewer") #fun selection of color palettes based on artwork at the NY MET [https://github.com/BlakeRMills/MetBrewer]

library(ggplot2)
library(RColorBrewer)
library(viridis)
library(cowplot)
library(colorspace)
library(colorblindr)
library(MetBrewer)

#our dataset for the workshop
library(palmerpenguins)
penguins <- penguins


## LET'S START WITH THE BASICS
p0 <- ggplot(data=penguins,
             aes(x=bill_length_mm, y=bill_depth_mm)) +
  geom_point() +
  #geom_point(color="hotpink1") +
  #geom_smooth(method="lm") +
  #geom_smooth(method="lm", color="#e06666", fill="#f2b3b3") +
  theme_minimal() +
  labs(x="Bill Length (mm)", y="Bill Depth (mm)")
p0

# built-in colors {colors()}: https://sape.inf.usi.ch/quick-reference/ggplot2/colour
# using hex codes: https://www.color-hex.com/

#importance of the third dimension: simpson's paradox (https://en.wikipedia.org/wiki/Simpson%27s_paradox)
#Jordan Ellenberg: "[the lesson] isn't really to tell us which viewpoint to take
#but to insist that we keep both the parts and the whole in mind at once."


# THERE ARE SO MANY WAYS TO CHOOSE COLOR PALETTES
# 1: default ggplot2 colors just by specifying 'color' in the aes() call
p1 <- ggplot(data=penguins,
            aes(x=bill_length_mm, y=bill_depth_mm, color=species)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_minimal() +
  labs(x="Bill Length (mm)", y="Bill Depth (mm)", color="Species:")
p1
colorblindr::cvd_grid(p1)

# 2: with RColorBrewer (https://en.wikipedia.org/wiki/Cynthia_Brewer) {using scale_*_brewer()}
p2 <- p1 + scale_color_brewer(palette = "Dark2")
p2
colorblindr::cvd_grid(p2)

# 3: with the viridis package {using scale_*_viridis()}
p3 <- p1 + scale_color_viridis(discrete=TRUE)
p3
colorblindr::cvd_grid(p3)

# 4: pick your own palette! {using scale_*_manual()}
#selected from https://colorbrewer2.org/
col <- c('#1b9e77','#d95f02','#7570b3')  #this is the "javascript" export option, put into R syntax
p1 + scale_color_manual(values = col)

#selected from MetBrewer (note that he has a package for MoMA, too!)
veronese <- met.brewer("Veronese", 3)
p1 + scale_color_manual(values = veronese)

#hand-selected (here, borrowed from https://github.com/JLSteenwyk/ggpubfigs)
contrast_three <- c("#004488", "#BB5566", "#DDAA33")
p1 + scale_color_manual(values=contrast_three)

# 5: why limit yourself to color? add shape! (Claus O. Wilke calls this "redundant coding")
p4 <- ggplot(data=penguins,
             aes(x=bill_length_mm, y=bill_depth_mm, color=species, fill=species, shape=species)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_minimal() +
  scale_color_manual(values = contrast_three, name="Species") +
  scale_fill_manual(values = contrast_three, name="Species") +
  scale_shape_manual(values=c(22, 21, 24), name="Species") +                 #this line, in combination with the two above it, force a single legend by specifying an identical name argument
  labs(x="Bill Length (mm)", y="Bill Depth (mm)")
p4


## QUICK TOUR OF SOME OTHER PLOT TYPES
#boxplots
p5 <- ggplot(data=penguins,
       aes(x=island, y=flipper_length_mm, color=species)) +
  #geom_point(position=position_jitterdodge(jitter.width=0.3, dodge.width=0.7), alpha=0.5) +
  geom_boxplot() +
  #geom_boxplot(alpha=0.3, outliers=FALSE) +
  theme_minimal() +
  labs(x="Island", y="Flipper Length (mm)", color="Species")
p5

p5 + scale_color_viridis(discrete = TRUE)
p5 + scale_color_manual(values = c("#f8971f", "#005f86", "#00a9b7"))

#barplots
library(dplyr)
pen_means <- penguins %>%
  filter(!is.na(flipper_length_mm)) %>%
  group_by(island, species) %>%
  summarise(count = n(),
            fl_mean = mean(flipper_length_mm),
            fl_sd = sd(flipper_length_mm))

p6 <- ggplot(data=pen_means,
       aes(x=island, y=fl_mean, fill=species, color=species)) +
  geom_bar(stat="identity", width=0.5, position=position_dodge(width=0.7)) + 
  geom_errorbar(aes(ymin=fl_mean-fl_sd, ymax=fl_mean+fl_sd), width=0.2, position=position_dodge(width=0.7)) +
  theme_minimal() +
  labs(x="Island", y="Mean Flipper Length (mm)", color="Species", fill="Species")
p6

p6 + scale_fill_viridis(discrete=TRUE, option="plasma") + scale_color_viridis(discrete=TRUE, option="plasma")
p6 + scale_fill_manual(values = contrast_three) + scale_color_manual(values = contrast_three)


## CONNECTING GROUPS (FACTORS) TO COLORS
levels(pen_means$species)

#option 1: change the order of the colors
contrast_three_v2 <- c("#004488", "#DDAA33", "#BB5566")
p6 + scale_fill_manual(values = contrast_three_v2) + scale_color_manual(values = contrast_three_v2)

#option 2: re-order of the underlying factor
pen_means$species <- factor(pen_means$species, levels = c("Gentoo", "Chinstrap", "Adelie"))
#re-run p6 above to re-generate the plot with the re-ordered levels
p6 + scale_fill_manual(values = contrast_three_v2) + scale_color_manual(values = contrast_three_v2)


## HIGHLIGHTING ONE GROUP
contrast_three_blue_highlight <- c("#004488", desaturate(lighten("#DDAA33", 0.4), 0.5), desaturate(lighten("#BB5566"), 0.4), 0.5)
p6 + scale_fill_manual(values = contrast_three_blue_highlight) + scale_color_manual(values = contrast_three_blue_highlight)

grey_with_blue_highlight <- c("#004488", "#babcbf", "#d7dade")
p6 + scale_fill_manual(values = grey_with_blue_highlight) + scale_color_manual(values = grey_with_blue_highlight)


## THUS FAR, WE HAVE BEEN USING QUALITIATIVE (I.E., DISCRETE PALETTES), WHAT ABOUT CONTINUOUS?
# 1: sequential/gradient palettes
p10 <- ggplot(data=penguins,
              aes(x=species, y=flipper_length_mm, color=body_mass_g)) +
  geom_jitter(width=0.2) +
  facet_wrap(~island, strip.position = "bottom") +
  theme_minimal() +
  theme(strip.placement = "outside",
        strip.text = element_text(size=10, face="bold"),
        panel.spacing = unit(0, "cm")) +
  labs(x="Species × Island", y="Flipper Length (mm)", color="Body Mass (g)")
p10

p10 + scale_color_distiller(palette="BuGn") #continuous legend

p10 + scale_color_fermenter(palette="BuGn") #binned legend
p10 + scale_color_fermenter(palette="BuGn", show.limits = TRUE)

# 2: create your own gradient palette
p10 + scale_color_gradient(low="#5e1a99", high="#de8423") #continuous legend

p10 + scale_color_steps(low="#5e1a99", high="#de8423") #binned legend

p10 + scale_color_gradient2(low="#5e1a99", mid="white", high="#de8423", midpoint=mean(penguins$body_mass_g, na.rm=TRUE)) +
  theme_dark() #effectively creates a diverging color palette

#useful overview of scale_*_* functions: [https://stackoverflow.com/questions/70942728/understanding-color-scales-in-ggplot2]


# MAKE YOUR OWN COLOR PALETTES FOR RE-USE
location = "/Users/jessicaalexander/github/workshops/color"      #change this to the appropriate path on your computer
source(file.path(location, 'customcolors.R'))

p1 + scale_color_manual(values = my_palettes(name="ut_pop", type="discrete", n=3))


# WHAT ABOUT OTHER PARTS OF THE PLOT? [https://ggplot2.tidyverse.org/reference/theme.html]
p30 <- p1 + scale_color_grey()
p30

p31 <- p30 + theme(panel.background = element_rect(fill = '#f8971f', color = '#f8971f'))
p31

p32 <- p31 + theme(panel.grid.major = element_line(color = "#f7eb3e"),
                   panel.grid.minor = element_line(color = "#f7eb3e"))
p32

p33 <- p32 + theme(axis.text = element_text(color="red"))
p33


## USEFUL RESOURCE
# Claus O. Wilke's book, Fundamentals of Data Visualization: [https://github.com/clauswilke/dataviz]


## YOUR TURN!
# create the visually overwhelming challenge plot :D
# feel free to use your own color palette: I used MetBrewer's Egypt: https://github.com/BlakeRMills/MetBrewer?tab=readme-ov-file#egypt

egypt = met.brewer("Egypt", 4)

ggplot(data=penguins,
       aes(x=flipper_length_mm, y=body_mass_g, color=island)) +
  geom_point(alpha=0.7) +
  scale_color_manual(values = egypt[1:3]) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = egypt[4]),
        panel.grid.minor = element_line(color = egypt[4]),
        axis.text = element_text(color = egypt[2]),
        axis.title = element_text(color = egypt[1]),
        legend.title = element_text(color = egypt[1])) +
  labs(x="Flipper Length (mm)", y="Body Mass (g)", color="Island")
# an excellent explanation of all the functions in ggplot that allow you to specify a color palette or color scale
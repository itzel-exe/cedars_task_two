
# Installs libraries
library(ggplot2)
library(dplyr)
library(tidyr)
#===========================================================================================================
# Data
#===========================================================================================================

# Declare given data
# Years pertaining to data
years <- c(1990, 1995, 2000, 2005, 2010)

df <- rbind(
  data.frame(risk = "Hypertension",         yr = 1:5, value = c(0.654, 0.633, 0.602, 0.561, 0.509)),
  data.frame(risk = "Diabetes",             yr = 1:5, value = c(0.359, 0.316, 0.260, 0.187, 0.092)),
  data.frame(risk = "Smoking",              yr = 1:5, value = c(0.171, 0.156, 0.142, 0.128, 0.116)),
  data.frame(risk = "Hypercholesterolemia", yr = 1:5, value = c(0.161, 0.104, 0.045, 0.001, 0.001)),
  data.frame(risk = "Obesity",              yr = 1:5, value = c(0.001, 0.013, 0.043, 0.077, 0.115))
)

#===========================================================================================================
# Chart Characterisitis
#===========================================================================================================
# Configuring chart visuals by setting constant characteristics
DRAW_ORDER <- c("Hypertension", "Diabetes", "Smoking", "Obesity", "Hypercholesterolemia")

#colors of the bands
band_colors <- c(Hypertension         = "#8BBB8F", 
          Diabetes             = "#DA654F",   
          Smoking              = "#7FABC5", 
          Hypercholesterolemia = "#D99155",   
          Obesity              = "#4D6C96")   


WHITE_SPACE      <- 0.038        # vertical white space between stacked bands (y units)
MIN_H    <- 0.015                # extra height added to every band ("raise the platform"),

# Buffer space values to account for small values (none visible strips)
HALF_WIDTH       <- 0.34         # half-width of each year's plateau (x units; 1 = spacing between years)
GAP       <- 0.006               # white gap between a plateau and its sloped connector (each side)
CORNER_X <- 0.015                # corner rounding of plateaus, horizontal (x units); set both to 0 for square
CORNER_Y <- 0.010                # corner rounding of plateaus, vertical (y units)
RIBBON_OPACITY    <- 1           # ribbon opacity (1 = opaque)

# Font and letter sizes
FONT        <- "sans"
SIZE_TITLE  <- 18
SIZE_YEAR   <- 14
SIZE_CAT    <- 14
SIZE_VALUE  <- 12


#===========================================================================================================
#Stack Bands In Order
#===========================================================================================================
stack <- df %>%
  # Creates the visual height (h) of each band and adds extra buffer height
  mutate(h = value + MIN_H) %>% 
  # Treat each year separately for the calculations that follow.
  group_by(yr) %>%  
  # Sort the bands within each year by smallest at the bottom to largest at the top, if 
  #values are equal use risk to break the tie
  arrange(value, risk, .by_group = TRUE) %>%       
  mutate(rk   = row_number(),                      # Ranks each row/band in order within its year
         ymin = cumsum(h) - h + GAP * (rk - 1),    # Calculates where the bottom of the current band should be, adds height of bands below it and gap between those bands
         ymax = ymin + h,                          # Calculates the top of the current band (y-axis)
         ymid = (ymin + ymax) / 2) %>%             # Caluates the vertical midpoint/center of the band
  ungroup()                                        # Ungroups -removes year grouping so the stack goes back to normal

#===========================================================================================================
# Retangles
#===========================================================================================================
# To create rectangle you need:
# xmin = left edge
# xmax = right edge
# ymin = bottom edge
# ymax = top edge
# rx = horizontal corner radius
# ry = vertical corner radium
# n = number of points used to draw each corner


# cx = center x-coordinate of the corner
# cy = center y-coordinate of the corner
# a0 = starting angle
# a1 = ending angle

#-----------------------------------------------------------------------------------------------------------
# Rectangle function - Creates segement/rectangles for bands#
rrect <- function(xmin, xmax, ymin, ymax, rx, ry, n = 8) {
  # Prevents rounded corners from overlapping in very thin bands
  # by limiting the vertical corner radius to half the band's height.
  ry <- min(ry, (ymax - ymin) / 2)
#-----------------------------------------------------------------------------------------------------------        
  # Arcircles Function - Creates curver corner/s of the rectangle
  arc <- function(cx, cy, a0, a1) { 
    # Creates sequence of n number of points used to draw each corner between the start and ending angles
    th <- seq(a0, a1, length.out = n)
    # Converts angles into coordinates
    data.frame(x = cx + rx * cos(th), y = cy + ry * sin(th))
  }
#-----------------------------------------------------------------------------------------------------------  
  # Joins corner together by joining the four sets of points together.
  rbind(arc(xmax - rx, ymax - ry, 0,       pi / 2),        # top-right
        arc(xmin + rx, ymax - ry, pi / 2,  pi),            # top-left
        arc(xmin + rx, ymin + ry, pi,      3 * pi / 2),    # bottom-left
        arc(xmax - rx, ymin + ry, 3 * pi / 2, 2 * pi))     # bottom-right
}
#--------------------------------------------------------------------------------------------------------------
# Gives sequence of the number of rows(bands) and for every row does the following..
# nrows(stack) counts the number of rows/bands in stack
# seq_len() creates the number of rows
# lapply() goes through those row numbers one at a time
# function(i) are instructions to execute for every row
# bind_rows() combines the results from all the rows into one dataframe
plateaus <- bind_rows(lapply(seq_len(nrow(stack)), function(i) {
  # Gets the current row(band)
  r <- stack[i, ]
  # Creates a segment (rounded retangle) take the left and right edge and center it around the year
  # Gets the top and bottom, corner sizes
  points <- rrect(r$yr - HALF_WIDTH, r$yr + HALF_WIDTH, r$ymin, r$ymax, CORNER_X, CORNER_Y)
  # Identifies each retangle based on its row number
  points$id   <- paste0("p", i)
  # Adds Risk information about every point making up the current rectangle
  points$risk <- r$risk
  # Return the points created
  points
}))

#Summary: 
#For every band in stack, take its year, bottom, top, and corner settings. Use those to create
#a rounded rectangle. Give that rectangle an ID and its risk category. Then combine all the rectangles 
#into one data frame called plateaus.
#-----------------------------------------------------------------------------------------------------------
#===========================================================================================================
# SLOPES - Creates Slope Shapes
#===========================================================================================================
# Determines which bands get connected
slopes_tbl <- stack %>%                                # Declares stack datafram
  group_by(risk) %>%                                   # Groups data by risk
  arrange(yr, .by_group = TRUE) %>%                    # Arranges each risk group by year from earlist to latest
  mutate(ymin2 = lead(ymin), ymax2 = lead(ymax)) %>%   # the the position in the next year
  filter(!is.na(ymin2)) %>%                            # Drops the final year and keep the rows that don't an an edge missing
  ungroup() %>%                                        # Removes risk grouping
  arrange(match(risk, DRAW_ORDER))                     # Determines which slopes get drawn first and which get drawn later
#===========================================================================================================
# nrows(slopes_tbl) counts the number of rows/bands in slope_tbl
# seq_len() creates the number of rows
# lapply() goes through those row numbers one at a time
# function(i) are instructions to execute for every row
# bind_rows() combines the results from all the rows into one dataframe

# r = current connector
# xL = left connector
# xR = right connector

# point 1 = xL current bottom
# point 2 = xL current top
# point 3 = xR next top
# point 4 = xR next bottom

slopes <- bind_rows(lapply(seq_len(nrow(slopes_tbl)), function(i) {
  # Gets the current row, which represents the current
  # risk band and its connection to the next year.
  r  <- slopes_tbl[i, ] 
  # Finds the LEFT side of the connector:
  # starts at the current year's position,
  # moves to the right edge of its plateau,
  # then moves slightly farther right by GAP.
  xL <- r$yr + HALF_WIDTH + WHITE_SPACE   
  # Finds the RIGHT side of the connector:
  # moves to the next year,
  # moves to the left edge of that year's plateau,
  # then moves slightly farther left by GAP.
  xR <- r$yr + 1 - HALF_WIDTH - WHITE_SPACE
            # Gives the slope an ID and carries the existing
            # risk category onto the slope.
             data.frame(id = paste0("s", i), risk = r$risk,   
                # Creates the four x-coordinates of the connector.        
                x = c(xL, xL, xR, xR), 
                # Creates the four y-coordinates:
                # current bottom, current top,
                # next top, next bottom
                y = c(r$ymin, r$ymax, r$ymax2, r$ymin2))         
}))

slopes$id <- factor(slopes$id, levels = unique(slopes$id))   #gives each slope an id
#===========================================================================================================
# 6. LABELS
#===========================================================================================================
#Creates a new stack with new column called txt that will be used for plotting as a label.
#Rounds the value to two decimal places using half-up rounding, then format it as text with exactly two decimal places.
vlab <- stack %>% mutate(txt = sprintf("%.2f", floor(value * 100 + 0.5 + 1e-9) / 100))
#category labels - keep only the bands belonging to the first year 
catlab <- stack %>% filter(yr == 1)
#===========================================================================================================
# 7. LAYOUT GEOMETRY (measured from Picture1.jpg)
#===========================================================================================================
#Defines canvas sizing
XLIM <- c(-0.277, 5.768)   # whole canvas in x units (left, right) edges
YLIM <- c(-0.082, 2.017)   # whole canvas in y units (bottom, top) edges

#===========================================================================================================
# 8. PLOT
#===========================================================================================================
#start with an empty plot called p
p <- ggplot() +
  # Draws sloped connectors
  geom_polygon(data = slopes,                       # 4 points for each connector
               aes(x, y, group = id, fill = risk),  # Columns containing th coordinates, id and fills each connector according to risk category
               alpha = RIBBON_OPACITY) +                     # Controls transpancy
  # Draws the plateaus
  geom_polygon(data = plateaus,                     # Contains the points to form the rectangles
               aes(x, y,
                   group = id,
                   fill = risk),
                   alpha = RIBBON_OPACITY) +
  # Draws the values inside the bands as white and bold
  
  geom_text(data = vlab,                            # Formatted values for labels
            aes(x = yr, y = ymid, label = txt),     # Year centered around, midpoint used, label needed
            colour = "white", fontface = "bold",    # Text is white and bold
            size = SIZE_VALUE / .pt, family = FONT) +
  # category names on the left (black, regular)
  geom_text(data = catlab,                           # First year's band
            aes(x = XLIM[1] + 0.03,y = ymid,label = risk), # Puts category labels inside the left edge and puts each label athe center of band
            hjust = 0, colour = "black",             # No horizontal justification, 
            size = SIZE_CAT / .pt, family = FONT) +
  # years across the top ofchart
  annotate("text",
           x = 1:5,                                  # Five positions get stored in year
           y = 1.634,                                # Puts labels the top
           label = years,                            # Five positions get stored in year              
           colour = "black", 
           size = SIZE_YEAR / .pt, family = FONT) +
  # title
  annotate("text", x = mean(XLIM), y = 1.936,                                               # Centers title 
           label = "Risk Factors for Stroke in Blacks",
           fontface = "bold", size = SIZE_TITLE / .pt, family = FONT) +                     # Makes bold
  scale_fill_manual(values = band_colors, guide = "none") +                                        # States what colors to use
  # Sets canvas
  coord_cartesian(xlim = XLIM, ylim = YLIM, expand = FALSE, clip = "off") +
  # Removes the given theme
  theme_void() +
  # Custom theme  
  theme(plot.margin = margin(0, 0, 0, 0),               # Removes margins
        plot.background = element_rect(fill = "white",  # Makes background white
        colour = "grey55",                              # Make borders grey
        linewidth = 1))                                 # Border thickness

print(p)                                                #Display plot


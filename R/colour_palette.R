library(DescTools)
library(ggplot2)


# Load Colour Palette from DMN Style Guidelines 
colour_names <- c("land", "water", "neighbour", "park", "country", "dmnpink",
                  "silver", "dmnred", "dmnblue", "dmnbeige", "ocre", "dmngreen",
                  "dmnorange")
C <- c(0, 15, 0, 10, 0, 0, 5, 0, 50, 0 , 0, 25, 0)
M <- c(0, 0, 0, 0, 0, 20, 0, 100, 0, 0, 20, 0, 50)
Y <- c(10, 0, 10, 10, 15, 20, 0, 100, 0, 30, 70, 40, 100)
K <- c(15, 0, 25, 20, 35, 20, 15, 25, 35, 20, 20, 55, 0)
DMN_cmyk_codes <- matrix(data=c(C, M, Y, K), ncol=4)

# Function to convert given CMYK codes to hex codes 
convert_colour <- function(colour_name) {
  c <- colour_name[1]
  m <- colour_name[2]
  y <- colour_name[3]
  k <- colour_name[4]
  
  hexcode <- CmykToRgb(c, m, y, k, 100)
  return(hexcode)
}

# convert CMYK codes to hex codes for all colours 
DMN_hexcodes <- apply(X=DMN_cmyk_codes, MARGIN=1, FUN=convert_colour)


# dataframe to store the DMN Colour Palette colour names and associated hex codes
DMN_colour_palette <- data.frame(color_name=colour_names, hexcode=DMN_hexcodes)


# assign hex codes to colour names
land <- DMN_hexcodes[1]
water <- DMN_hexcodes[2]
neighbour <- DMN_hexcodes[3]
park <- DMN_hexcodes[4]
country <- DMN_hexcodes[5]
dmnpink <- DMN_hexcodes[6]
silver <- DMN_hexcodes[7]
dmnred <- DMN_hexcodes[8]
dmnblue <- DMN_hexcodes[9]
dmnbeige <- DMN_hexcodes[10]
ocre <- DMN_hexcodes[11]
dmngreen <- DMN_hexcodes[12]
dmnorange <- DMN_hexcodes[13]

full = read.csv("data.csv", stringsAsFactors = F)

# Obtain the Diet subset
Diet_vars = c("yearY234567890",
              "vidY234567890",
              "hhidY234567890", 
#              "idssnY234567890", # sees diet-related questions are asked at household level
              "hcriceY234567890", 
              "hcmaizeY234567890", # measured in cobs rather than kg
              "hcmaniocY234567890",
              "hcplantainY234567890",
              "hcsardinesY234567890",
              "hcchickenY234567890",
              "hctotbirdkgY234567890",
              "hctotfishkgY234567890",
              "hcpigeonpeaY234567890")

Diet = unique(full[,Diet_vars])


# Set the food energe per measuring unit for each of the foods consisting the diet
  # Per 100g
rice_cal = 101
rice_fat = 0.3
corn_cal = 106
corn_fat = 4.6
manioc_cal = 159
manioc_fac = 0.3
plantain_cal = 122
plantain_fat = 0.4
sardine_cal = 208
sardine_fat = 11
chicken_cal = 239
chicken_fat = 14
bird_cal = 112
bird_fat = 2
fish_cal = 200 # Using the average of seafish for now
fish_fat = 12
pigeon_pea_cal = 343
pigeon_pea_fat = 1.5

Cal = c(rice_cal, corn_cal, manioc_cal, plantain_cal, sardine_cal, chicken_cal, bird_cal, fish_cal, pigeon_pea_cal)

cal_calculator <- function(units, measure){
  x = units*measure*10
  return(x)
}

# Calorie Calculation
d = data.frame(matrix(ncol = length(Cal),
                      nrow = nrow(Diet)))
dimnames(d)[[2]] = c("Rice_Cal", 
                   "Maize_Cal",
                   "Manioc_Cal",
                   "Plantain_Cal",
                   "Sardine_Cal",
                   "Chicken_Cal",
                   "Bird_Cal",
                   "Fish_Cal",
                   "Pea_Cal")

for (i in 1:length(Cal)) {
  d[,i] = cal_calculator(Diet[,3+i], Cal[i])
}

# Note Maize is not measured in KG but in cob, needs to divide the output value by 10
d$Maize_Cal = d$Maize_Cal/10
d$Total_Cal = rowSums(d)

# Calculate total calories from staple food
d_pct = prop.table(as.matrix(d), margin = 1)
dimnames(d_pct)[[2]] = paste("Pct", 
                             dimnames(d_pct)[[2]], 
                             sep = "_")

d_pct = as.data.frame(d_pct)

d_pct$Pct_Staple = d_pct$Pct_Rice_Cal + 
  d_pct$Pct_Maize_Cal + 
  d_pct$Pct_Manioc_Cal + 
  d_pct$Pct_Plantain_Cal +
  d_pct$Pct_Pea_Cal

d_pct$Pct_Meat = 1 - d_pct$Pct_Staple
d_pct$Total_Cal = d$Total_Cal
d_pct = cbind(d_pct, Diet$yearY234567890, Diet$vidY234567890, Diet$hhidY234567890)
colnames(d_pct)[13:15] = c("year", "vid", "hhid")

# Global diet by year (average out everyone across all villages)
library(plyr)
library(dplyr)

global = d_pct %>%
  group_by(year) %>%
  summarise(
    M_Staple = mean(Pct_Staple),
    M_Meat = mean(Pct_Meat),
    M_Rice = mean(Pct_Rice_Cal),
    M_Maize = mean(Pct_Maize_Cal),
    M_Manioc = mean(Pct_Manioc_Cal),
    M_Plantain = mean(Pct_Plantain_Cal),
    M_Pea = mean(Pct_Pea_Cal),
    
    M_Sardine = mean(Pct_Sardine_Cal),
    M_Chicken = mean(Pct_Chicken_Cal),
    M_Bird = mean(Pct_Bird_Cal),
    M_Fish = mean(Pct_Fish_Cal)
  )

# For each village group, diet (calorie consumption per household) by year
village = d_pct %>%
  group_by(year, vid) %>%
summarise(
  M_Staple = mean(Pct_Staple),
  M_Meat = mean(Pct_Meat),

  M_Rice = mean(Pct_Rice_Cal),
  M_Maize = mean(Pct_Maize_Cal),
  M_Manioc = mean(Pct_Manioc_Cal),
  M_Plantain = mean(Pct_Plantain_Cal),
  M_Pea = mean(Pct_Pea_Cal),
  
  M_Sardine = mean(Pct_Sardine_Cal),
  M_Chicken = mean(Pct_Chicken_Cal),
  M_Bird = mean(Pct_Bird_Cal),
  M_Fish = mean(Pct_Fish_Cal)
)
  
# convert column year to rownames for the upcoming transpose
t_global = as.data.frame(t(global))
colnames(t_global) = t_global[1,]
t_global[1,] <- NULL
t_global = t_global[-c(1:3),]
t_global$Type = c(rep("Staple", 5), rep("Meat", 4))

# For illustration purpose, let's do one pie chart for 2005
t_global_2005 = t_global[,c(4,10)]
t_global_2005$Slice = dimnames(t_global_2005)[[1]] 
colnames(t_global_2005) = c("Share", "Type", "Slice")
t_global_2005$Share = round(t_global_2005$Share*100, digits = 1)


# Donut plot
#' x      numeric vector for each slice
#' group  vector identifying the group for each slice
#' labels vector of labels for individual slices
#' col    colors for each group
#' radius radius for inner and outer pie (usually in [0,1])

donuts <- function(x, group = 1, labels = NA, col = NULL, radius = c(.7, 1)) {
  group <- rep_len(group, length(x))
  ug  <- unique(group)
  tbl <- table(group)[order(ug)]

  col <- if (is.null(col))
    seq_along(ug) else rep_len(col, length(ug))
  col.main <- Map(rep, col[seq_along(tbl)], tbl)
  col.sub  <- lapply(col.main, function(x) {
    al <- head(seq(0, 1, length.out = length(x) + 2L)[-1L], -1L)
    Vectorize(adjustcolor)(x, alpha.f = al)
  })

  plot.new()

  par(new = TRUE)
  pie(x, border = NA, radius = radius[2L],
      col = unlist(col.sub), labels = labels)

  par(new = TRUE)
  pie(x, border = NA, radius = radius[1L],
      col = unlist(col.main), labels = NA)
}

with(t_global_2005,
     donuts(Share, Type, sprintf('%s: %s%%', Slice, Share),
            col = c('orange', 'dodgerblue2'))
)



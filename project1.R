#project 1 Regression : problem R2
install.packages("rmarkdown")

#IMPORT DATA--------------------------------------------------------------------
# Read raw data as a text string
raw_text <- readLines("https://users.stat.ufl.edu/~winner/data/beerhall.dat")
# Replace multiple spaces with a single tab
cleaned_text <- gsub(" {2,}", "\t", raw_text)
# Read the cleaned data using tabs as the separator
beerhall_dat <- read.table(textConnection(cleaned_text), header = FALSE, sep = "\t", fill = TRUE)
# Define column names based on the .txt file
column_names <- c(
  "County",
  "Region",
  "Region_Code",
  "Criminals_per_100k",
  "Beerhouses_per_100k",
  "School_Attendance_per_10k",
  "Worship_Attendance_per_2000"
)
# Assign column names
colnames(beerhall_dat) <- column_names
#check data import 
head(beerhall_dat)
write.csv(beerhall_norm, "beerhall_data_raw.csv", row.names = FALSE)

#NORMALISATION

#transform to percentage
# Initialize an empty data frame with the same structure as beerhall_dat
beerhall_norm <- data.frame(criminals_per_100 = numeric(nrow(beerhall_dat)),
                            beerhouses_per_100 = numeric(nrow(beerhall_dat)),
                            school_per_100 = numeric(nrow(beerhall_dat)),
                            worship_per_100 = numeric(nrow(beerhall_dat)))
beerhall_norm$criminals_per_100 <- beerhall_dat$Criminals_per_100k / 1000  # Per 100 people
beerhall_norm$beerhouses_per_100 <- beerhall_dat$Beerhouses_per_100k / 1000  # Per 100 people
beerhall_norm$school_per_100 <- beerhall_dat$School_Attendance_per_10k / 100  # Per 100 people
beerhall_norm$worship_per_100 <- beerhall_dat$Worship_Attendance_per_2000 / 20  # Per 100 people

head(beerhall_norm)
write.csv(beerhall_norm, "beerhall_data_norm.csv", row.names = FALSE)

#ANALYSIS------------------------------------------------------------------------
data = beerhall_norm
summary(beerhall_norm)

#exploratory analysis
# scatterplots for continuous variables : percent beerhouse, school, worship
layout(matrix(1:4, nrow = 2))
plot(criminals ~ beerhouses, data = data, ylab = "percent criminals", xlab = " nb beerhall per 100")
plot(criminals ~ school, data = data, ylab = "percent criminals", xlab = "percent schooled")
plot(criminals ~ worship, data = data, ylab = "percent criminals", xlab = "percent worshiping")

# fit linear model y=b0+b1x+e
beerhall.formula <- criminals ~ beerhouses + school + worship
beerhall.lm <- lm(beerhall.formula, data = data)
summary(beerhall.lm)

#examine diagnostic plots: assess model assumptions and identify any influential points
par(mfrow = c(2, 2))  # Arrange 4 plots in a 2x2 grid
plot(beerhall.lm)  # Produces standard diagnostic plots


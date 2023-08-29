# Install and load the circlize package if you haven't already
install.packages("circlize")
library(circlize)

# Create a data frame from your input data
data <- read.table(text = "source sink frequency
urine control_oral 0.0006
vaginal control_oral 0.001066667
Unknown control_oral 0.9984
oral control_oral 0
urine control_urine 0
vaginal control_urine 0.594466667
Unknown control_urine 0.404866667
oral control_urine 6.67E-05
urine control_vaginal 0.919933333
vaginal control_vaginal 0
Unknown control_vaginal 0.0682
oral control_vaginal 0.0006
urine pre_oral 0.000733333
vaginal pre_oral 0
Unknown pre_oral 0.9992
oral pre_oral 0
urine pre_urine 0
vaginal pre_urine 0.4825
Unknown pre_urine 0.5171
oral pre_urine 0.001033333
urine pre_vaginal 0.974566667
vaginal pre_vaginal 0
Unknown pre_vaginal 0.025433333
oral pre_vaginal 0
urine post_oral 0.0007
vaginal post_oral 0.004266667
Unknown post_oral 0.995
oral post_oral 0
urine post_urine 0
vaginal post_urine 0.578033333
Unknown post_urine 0.419366667
oral post_urine 0.0002
urine post_vaginal 0.572166667
vaginal post_vaginal 0
Unknown post_vaginal 0.421133333
oral post_vaginal 0.0038", header = TRUE)

# Create a chord diagram
chordDiagram(data, transparency = 0.5)

# Define color specifications for sources
source_colors <- c(
  "urine" = "#ddcc77",
  "oral" = "#44aa99",
  "vaginal" = "#cc6677",
  "Unknown" = "#656565"
)

# Create a chord diagram with specified colors and no numbers
chordDiagram(
  data,
  transparency = 0.5,
  col = source_colors[data$source], # Assign colors based on source
)


# Create a chord diagram with specified colors and no numbers
chordDiagram(
  data,
  transparency = 0.5,
  annotationTrack = NULL, # Removes numbers
  annotationTrackHeight = 0.03, # Adjust the height of the source labels
  preAllocateTracks = 1, # Ensures there's space for labels
  col = source_colors[data$source], # Assign colors based on source
  annotationTrackMargin = 0.05 # Adjust the margin for labels
)

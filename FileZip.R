# Zips files to put on google drive

# Set your working directory to the Sampling folder
setwd("/Users/sks379/Desktop/GitHubProjects/DataAnalysis/data/sampling/grts/")

# Zip each folder
zip("ExistingPlots.zip", "legacy_points/")
zip("SamplingArea.zip", "sampling_frame/")
zip("Strata.zip", "strata/")

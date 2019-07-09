library(regimeDetectionMeasures)
# Munge original data -----------------------------------------------------
origData = munge_orig_dat(example = T)

# Visualize original data -------------------------------------------------
# Plot the original time series abundances
plot_orig_data(data = origData,
               example = F,
               print = T)

# Plot the distance between observations
plot_timeDiff(data = origData,
              example = F,
              print = T)

# Plot species richness over time
plot_richness(data = origData,
              example = F,
              print = T)

# Calculate distance travelled and derviatves of this ---------------------
distances <- calculate_distanceTravelled(origData, derivs = T)
head(distances, 5)

# Calculate FI, VI, and early warning signals --------------------------------------------------------
# Uses a moving window analysis to calculate FI and Vi within each window
results <-
    rdm_window_analysis(
        origData,
        winMove = 0.25,
        min.window.dat = 2,
        fi.equation = "7.12",
        to.calc = c('EWS', 'VI', 'FI')
    )

# Reults will return all results in a single data frame.
head(results,5)


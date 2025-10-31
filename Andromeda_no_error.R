library(ggplot2)

data <- read.csv("NGC.csv")

data1 <- data.frame(Radius <- data[,"Radius"],
                    Velocity <- data[,"Velocity"])

plot(Radius,Velocity)

ggplot(data1, aes(x = Radius, y = Velocity)) +
        geom_line(color = 'blue', size = 1) +
        theme_minimal() +
        labs(
             title = "Galactic Rotation Curve",
             x = "Radius (kpc)",
             y = "Velocity (km/s)" 
             )
)

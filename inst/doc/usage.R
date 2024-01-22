## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 6.5,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(ConconiAnaerobicThresholdTest)

## -----------------------------------------------------------------------------
fname = system.file("extdata/2023-01-16.tcx.gz", package = "ConconiAnaerobicThresholdTest")
tmp <- prepdata(gzfile(fname), startminutes = 0, endminutes = 100,
          useDeviceSpeed = TRUE)
plot(tmp$minutes, tmp$speed)
plot(tmp$minutes, tmp$cadence_running)
plot(tmp$minutes, tmp$heart_rate)

## -----------------------------------------------------------------------------
dat202301 <- prepdata(gzfile(fname), startminutes = 0.15, endminutes = 15,
          useDeviceSpeed = FALSE)
(dat202301$date = substr(dat202301$time[1], 1, 10))

## -----------------------------------------------------------------------------
plot(dat202301$minutes, dat202301$speed)
plot(dat202301$minutes, dat202301$cadence_running)
plot(dat202301$minutes, dat202301$heart_rate)

## -----------------------------------------------------------------------------
fitmodel(dat202301, alldata = TRUE, title = "January 2023, using all HR data")

## -----------------------------------------------------------------------------
fitmodel(dat202301, alldata = FALSE, title = "January 2023, using only last 5 HR measurements of each step")

## -----------------------------------------------------------------------------
fname = system.file("extdata/2023-09-15.tcx.gz", 
                    package = "ConconiAnaerobicThresholdTest")
dat202309 <- prepdata(gzfile(fname), startminutes = 23.8, endminutes = 40.1,
          useDeviceSpeed = FALSE)
dat202309$date = substr(dat202309$time[1], 1, 10)
with(dat202309, plot(minutes, speed))

## -----------------------------------------------------------------------------
fitmodel(dat202309, alldata = TRUE, title = "September 2023, using all HR data")
fitmodel(dat202309, alldata = FALSE, title = "September 2023, using only last 5 HR measurements of each step")

## -----------------------------------------------------------------------------
fname = system.file("extdata/2022-01-10.tcx.gz",
                    package = "ConconiAnaerobicThresholdTest")
dat202201 <- prepdata(gzfile(fname), startminutes = 26, endminutes = 38.99,
          useDeviceSpeed = FALSE)
dat202201$date = substr(dat202201$time[1], 1, 10)

## -----------------------------------------------------------------------------
fitmodel(dat202201, alldata = TRUE, title = "January 2022, using all HR data")
fitmodel(dat202201, alldata = FALSE, title = "January 2022, using only last 5 HR measurements of each step")

## -----------------------------------------------------------------------------
xall <- full_join(x=dat202309, y=dat202301) |>
    full_join(y=dat202201) |>
    mutate(date = factor(date)) |>
    mutate(speed = factor(speed))

## ----fig.width=6.5, fig.height=6.3--------------------------------------------
ggplot(xall, aes(x=speed, y=heart_rate, fill = date)) +
    geom_boxplot()  +
    scale_y_continuous(breaks=seq(90, 200, by=10))

## ----fig.width=6.5, fig.height=6.3--------------------------------------------
ggplot(xall, aes(x = minutes, y = heart_rate, color = date)) +
    geom_point(size = 0.5)  +
    geom_smooth() +
    scale_y_continuous(breaks = seq(90, 200, by = 10), name = "Heart Rate (bpm)") +
    scale_x_continuous(breaks = seq(0, 16.5, by = 1.5), name = "Time (minutes)", 
                       sec.axis = sec_axis( ~ . / 1.5 + 6, 
                                            name = "speed (km/h)", 
                                            breaks = seq(6, 16, by = 1)))

## ----fig.width=6.5, fig.height=6.3--------------------------------------------
filter(xall, date != "2022-01-10") |> # bad cadence data from 2022-01-10
    ggplot(aes(
        x = as.numeric(as.character(speed)),
        y = 2 * cadence_running,
        color = date
    )) +
    geom_point(size = 0.5)  +
    geom_smooth() +
    scale_y_continuous(breaks = seq(150, 200, by = 10), name = "Cadence (spm)") +
    scale_x_continuous(
        breaks = seq(0, 16.5, by = 1),
        name = "speed (km/h)",
        sec.axis = sec_axis(~ . / 1.5 + 6,
                            name = "speed (km/h)",
                            breaks = seq(6, 16, by = 1))
    )


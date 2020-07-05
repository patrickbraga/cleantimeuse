# sample plots
mtus_histogram <- function(country_data){
  #' Example histogram using the `gsdmtus` package
  # Calculate survey means:
  def_survey_objects(country_data)

  # Histogram of percent of time spent working from home (weighted)
  hist <- svyhist(~pct_home_work, svy_part_home, breaks=50,
                  main = "", yaxt = "none", ylab = "",
                  xaxt = "none",
                  xlab = "Share of work occurring at home\n(among those who do some, but not all, work at home)")
  axis(1,at=seq(0,1,by=0.1),labels =
         c("0","10%","20%","30%","40%","50%","60%","70%","80%","90%","100%"))
}

mtus_density <- function(country_data, new_plot = TRUE){
  def_survey_objects(country_data)
  #' Example density plot using the `gsdmtus` package
  line_color = "black"

  dens_part<-svysmooth(~work_time, svy_part_home,bandwidth = 30)
  dens_work<-svysmooth(~work_time, svy_all_work,bandwidth = 30)
  dens_home<-svysmooth(~work_time, svy_all_home,bandwidth = 30)

  # When new_plot == TRUE, the plot is reset from scratch.
  # When new-plot == FALSE, the mtus_density_ov function
  # can overlay lines from a second country onto the plot.
  if (new_plot == TRUE) {
    plot(dens_work,
         lty = 1,lwd=2, #col="red",
         yaxt="none",ylab="",xaxt="none",xlab="Total daily hours spent working",
         xlim=c(0,24*60), ylim=c(0,0.008))
  } else if (new_plot == FALSE) {
    line_color = "red"
  }

  lines(dens_work,lty=1,lwd=2, col = line_color)
  lines(dens_home,lty=2,lwd=1, col = line_color)
  lines(dens_part,lty=1,lwd=1, col = line_color)
  axis(1,at=seq(0,1440,by=60),labels = seq(0,24,by=1))
  legend("topright",inset=0,pt.cex=1,cex=.75, legend=
           c("People who worked exclusively away from home",
             "People who worked exclusively at home",
             "People who who did some, but not all, work at home"),
         lty=c(1,2,1),lwd=c(2,1,1))
}

mtus_cor_test <- function(country_data) {
  cor.test(country_data$work_time,country_data$pct_home_work)
  cor.test(country_data$work_time[country_data$work_time > 420],country_data$pct_home_work[country_data$work_time > 420])
  cor.test(country_data$work_time[country_data$work_time < 420],country_data$pct_home_work[country_data$work_time < 420])
}

mtus_svy_means <- function(country_data) {
  def_survey_objects(country_data)

  # Relative shares of workers all, part, or never from home
  svymean(~work_cat,svy_workers)
  svymean(~full_day,svy_workers)
  svymean(~interaction(full_day,work_cat),svy_workers)

  # Number of trips by group of workers
  svymean(~trips, svy_workers)
  svyby(~trips, ~work_cat, svy_workers, svymean)
  svyby(~trips, ~full_day, svy_workers, svymean)
  svyby(~trips, ~work_cat + full_day, svy_workers, svymean)

  # excluding work trips
  svymean(~fun_trips, svy_workers)
  svyby(~fun_trips, ~work_cat, svy_workers, svymean)
  svyby(~fun_trips, ~full_day, svy_workers, svymean)
  svyby(~fun_trips, ~work_cat + full_day, svy_workers, svymean)

  # just work trips
  svymean(~commutes, svy_workers)
  svyby(~commutes, ~work_cat, svy_workers, svymean)
  svyby(~commutes, ~full_day, svy_workers, svymean)
  svyby(~commutes, ~work_cat + full_day, svy_workers, svymean)
}

mtus_density_ov <- function(country1, country2){
  #' Overlay function for example density plot using the `gsdmtus` package
  mtus_density(country1)
  mtus_density(country2, new_plot = FALSE)
}

mtus_scatter <- function(country_data){
  #' Example scatterplot using the `gsdmtus` package
  ggplot(country_data[country_data$part_home,], aes(work_time, pct_home_work)) +
    geom_point(size=0.05,col="gray") +
    stat_smooth(col='black') +
    theme_bw() +
    scale_x_continuous(breaks = seq(0,1440,by=60),
                       labels = seq(0,24,by=1)) +
    scale_y_continuous(breaks = seq(0,1,by=0.05),
                       labels = paste(seq(0,100,by=5),"%",sep=""),
                       limits = c(0,NA)) +
    xlab("Daily hours spent working\n(among those who do some, but not all, work at home)") +
    ylab("Percent of work occurring at home")
}

mtus_trip_density <- function(country_data) {
  #' Example trip density plot using the `gsdmtus` package
  def_survey_objects(country_data)

  trip_dens_part<-svysmooth(~trips, svy_part_home, bandwidth = 1)
  trip_dens_work<-svysmooth(~trips, svy_all_work, bandwidth = 1)
  trip_dens_home<-svysmooth(~trips, svy_all_home, bandwidth = 1)

  trip_dens_all<-svysmooth(~trips, svy_workers, bandwidth = 1)

  plot(trip_dens_all,
       yaxt="none", ylab="",xaxt="none",xlab="Number of daily trips")
  axis(1,at=seq(0,32,by=2))

  plot(trip_dens_work,
       lty = 1,lwd=2,
       yaxt="none", ylab="",xaxt="none",xlab="Number of daily trips",
       xlim=c(0,30), ylim = c(0,0.5))
  lines(trip_dens_home,lty=2,lwd=1)
  lines(trip_dens_part,lty=1,lwd=1)
  axis(1,at=seq(0,32,by=2))
  legend("topright",inset=0.05,legend=
           c("People who worked exclusively away from home",
             "People who worked exclusively at home",
             "People who who did some, but not all, work at home"),
         lty=c(1,2,1),lwd=c(2,1,1))

}

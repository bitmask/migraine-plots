#!/usr/bin/env Rscript

library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)
library(cowplot)

# directories
base_dir <- getwd()
parsed_dir <- file.path(base_dir, "data", "parsed")
plots_dir <- file.path(base_dir, "plots")

# set timezone for posixct
Sys.setenv(TZ='UTC') 

# gather the data together

migr <- read.csv(file.path(parsed_dir, "migraine_start_end"), header=F, sep="\t", colClasses=c("POSIXct", "integer"))
colnames(migr) <- c("date", "migraine")
migr <- tbl_df(migr)
m <- migr
m$time <- format(m$date, "%H")
m$day <- format(m$date, "%Y-%m-%d")

# ensure data points are unique by hour, logging multiple things within an hour can cause multiple data points in the same hour
m$ymdh <- format(m$date, "%Y-%m-%d-%H")
m <- m %>% group_by(ymdh) %>% summarize(date=max(date), migraine=max(migraine), time=max(time), day=max(day), n=n())


drugs <- read.csv(file.path(parsed_dir, "drugs"), header=F, sep="\t", colClasses=c("POSIXct", "numeric", "character"))
colnames(drugs) <- c("date", "mg", "drug")
drugs <- tbl_df(drugs)
drugs$d <- 20
drugs$migraine <- 5
drugs$time <- format(drugs$date, "%H")
drugs$day <- format(drugs$date, "%Y-%m-%d")

clue <- read.csv(file.path(parsed_dir, "clue.read"), header=F, sep="\t", colClasses=c("POSIXct", "character"))
colnames(clue) <- c("date", "migraine")
clue$migraine <- as.factor(clue$migraine)
clue <- tbl_df(clue)
c <- clue %>% mutate(period = (migraine != "cramps")) %>% filter(period == TRUE)
c$migraine = as.character(c$migraine)
# convert to continuous scale
c[c$migraine == "heavy",]$migraine = "9" 
c[c$migraine == "medium",]$migraine = "9"
c[c$migraine == "light",]$migraine = "9"
c[c$migraine == "spotting",]$migraine = "1"
c$migraine = as.integer(c$migraine)
c <- c %>% filter(date > as.POSIXct("2017-08-15"))
c$time <- format(c$date, "%H")
c$day <- format(c$date, "%Y-%m-%d")

aura <- read.csv(file.path(parsed_dir, "aura"), header=F, sep="\t", colClasses=c("POSIXct", "character"))
colnames(aura) <- c("date", "aura")
aura <- tbl_df(aura)
aura$migraine <- aura$aura
aura[aura$aura == "Aura massive",]$migraine = "8" 
aura[aura$aura == "Aura",]$migraine = "8" 
aura$migraine <- as.integer(aura$migraine)
aura$time <- as.double(format(aura$date, "%H"))
aura$day <- format(aura$date, "%Y-%m-%d")

naus <- read.csv(file.path(parsed_dir, "nausea"), header=F, sep="\t", colClasses=c("POSIXct", "character"))
colnames(naus) <- c("date", "naus")
naus <- tbl_df(naus)
naus$migraine <- 10
naus$time <- as.double(format(naus$date, "%H"))
naus$day <- format(naus$date, "%Y-%m-%d")

# TODO: add pressure and spikes

make_migraine_plot <- function(start_date, end_date) {
 ggplot(m, aes(x=date, y=time, col=migraine)) +
 geom_point() +
 geom_point(data=drugs, aes(x=date, y=time, fill=migraine), shape=21, size=3, alpha=0.3) +
 #geom_point(data=aura, aes(x=date, y=time, fill=migraine), shape=21, size=3, alpha=0.7) +
 scale_colour_gradient(low="white", high="black") +
 scale_x_datetime(expand=c(0.01,0.01), lim=c(as.POSIXct(start_date),as.POSIXct(end_date)), breaks=pretty_breaks(n=4))+
 theme_minimal() +
 theme(axis.text.x= element_text(angle=0,hjust=0,vjust=0.5)) +
 theme(axis.text.y=element_text(size=rel(0.6))) +
 theme(axis.title.x=element_blank()) +
 theme(axis.title.y=element_blank()) + 
 theme(legend.position="none") +
 theme(axis.line = element_line(colour = "black", size=0.5, lineend="square")) +
 theme(axis.ticks.x=element_line(colour="black", size=0.5))  
 #coord_fixed(0.1*(max(as.numeric(m$date))-min(as.numeric(m$date)))/max(as.numeric(m$time)))
 #+ annotate("text", x=c(as.POSIXct("2017-09-01")), y=0, label=c("recording"))
}

make_clue_plot <- function(data, start_date, end_date, low_colour, high_colour, ytitle) {
 ggplot(data, aes(x=date, y=time, col=migraine)) +
 geom_point(alpha=0.8) +
 scale_colour_gradient(low=low_colour, high=high_colour) +
 scale_x_datetime(expand=c(0.01,0.01), lim=c(as.POSIXct(start_date),as.POSIXct(end_date)), breaks=pretty_breaks(n=4))+
 theme_minimal() +
 theme(axis.text.x=element_blank()) +
 theme(axis.text.y=element_blank()) + 
 theme(axis.title.x=element_blank()) + 
 #theme(axis.ticks.y=element_blank()) + 
 ylab(ytitle) +
 theme(legend.position="none") +
 theme(axis.line.y = element_line(colour = "black", size=0.5, lineend="square"))

 #coord_fixed(0.1*(max(as.numeric(m$date))-min(as.numeric(m$date)))/max(as.numeric(m$time)))
}

make_other_plot <- function(data, start_date, end_date, low_colour, high_colour, ytitle) {
 ggplot(data, aes(x=date, y=time, col=migraine)) +
 geom_point(alpha=0.8) +
 scale_colour_gradient(low=low_colour, high=high_colour) +
 scale_x_datetime(expand=c(0.01,0.01), lim=c(as.POSIXct(start_date),as.POSIXct(end_date)), breaks=pretty_breaks(n=4))+
 scale_y_continuous(breaks=c(8,16), limits=c(0,23)) +
 theme_minimal() +
 theme(axis.text.x=element_blank()) +
 theme(axis.text.y=element_blank()) + 
 theme(axis.title.x=element_blank()) + 
 #theme(axis.ticks.y=element_blank()) + 
 ylab(ytitle) +
 theme(legend.position="none") +
 theme(axis.line.y = element_line(colour = "black", size=0.5, lineend="square"))

 #coord_fixed(0.1*(max(as.numeric(m$date))-min(as.numeric(m$date)))/max(as.numeric(m$time)))
}

#TODO: generate these automatically from the range in the log

m2017q3 <- make_migraine_plot("2017-07-01", "2017-09-30")
m2017q4 <- make_migraine_plot("2017-10-01", "2017-12-31")
m2018q1 <- make_migraine_plot("2018-01-01", "2018-03-31")
m2018q2 <- make_migraine_plot("2018-04-01", "2018-06-30")
m2018q3 <- make_migraine_plot("2018-07-01", "2018-09-30")
m2018q4 <- make_migraine_plot("2018-10-01", "2018-12-31")
m2019q1 <- make_migraine_plot("2019-01-01", "2019-03-31")
m2019q2 <- make_migraine_plot("2019-04-01", "2019-06-30")
m2019q3 <- make_migraine_plot("2019-07-01", "2019-09-30")
m2019q4 <- make_migraine_plot("2019-10-01", "2019-12-31")
m2020q1 <- make_migraine_plot("2020-01-01", "2020-03-31")
m2020q2 <- make_migraine_plot("2020-04-01", "2020-06-30")
m2020q3 <- make_migraine_plot("2020-07-01", "2020-09-30")
m2020q4 <- make_migraine_plot("2020-10-01", "2020-12-31")

c2017q3 <- make_clue_plot(c, "2017-07-01", "2017-09-30", "tomato", "darkred", "")
c2017q4 <- make_clue_plot(c, "2017-10-01", "2017-12-31", "tomato", "darkred", "")
c2018q1 <- make_clue_plot(c, "2018-01-01", "2018-03-31", "tomato", "darkred", "")
c2018q2 <- make_clue_plot(c, "2018-04-01", "2018-06-30", "tomato", "darkred", "")
c2018q3 <- make_clue_plot(c, "2018-07-01", "2018-09-30", "tomato", "darkred", "")
c2018q4 <- make_clue_plot(c, "2018-10-01", "2018-12-31", "tomato", "darkred", "")
c2019q1 <- make_clue_plot(c, "2019-01-01", "2019-03-31", "tomato", "darkred", "")
c2019q2 <- make_clue_plot(c, "2019-04-01", "2019-06-30", "tomato", "darkred", "")
c2019q3 <- make_clue_plot(c, "2019-07-01", "2019-09-30", "tomato", "darkred", "")
c2019q4 <- make_clue_plot(c, "2019-10-01", "2019-12-31", "tomato", "darkred", "")
c2020q1 <- make_clue_plot(c, "2020-01-01", "2020-03-31", "tomato", "darkred", "")
c2020q2 <- make_clue_plot(c, "2020-04-01", "2020-06-30", "tomato", "darkred", "")
c2020q3 <- make_clue_plot(c, "2020-07-01", "2020-09-30", "tomato", "darkred", "")
c2020q4 <- make_clue_plot(c, "2020-10-01", "2020-12-31", "tomato", "darkred", "")

a2017q3 <- make_other_plot(aura, "2017-07-01", "2017-09-30", "lightgreen", "darkgreen", "aura")
a2017q4 <- make_other_plot(aura, "2017-10-01", "2017-12-31", "lightgreen", "darkgreen", "aura")
a2018q1 <- make_other_plot(aura, "2018-01-01", "2018-03-31", "lightgreen", "darkgreen", "aura")
a2018q2 <- make_other_plot(aura, "2018-04-01", "2018-06-30", "lightgreen", "darkgreen", "aura")
a2018q3 <- make_other_plot(aura, "2018-07-01", "2018-09-30", "lightgreen", "darkgreen", "aura")
a2018q4 <- make_other_plot(aura, "2018-10-01", "2018-12-31", "lightgreen", "darkgreen", "aura")

n2017q3 <- make_other_plot(naus, "2017-07-01", "2017-09-30", "orange", "orange", "nausea")
n2017q4 <- make_other_plot(naus, "2017-10-01", "2017-12-31", "orange", "orange", "nausea")
n2018q1 <- make_other_plot(naus, "2018-01-01", "2018-03-31", "orange", "orange", "nausea")
n2018q2 <- make_other_plot(naus, "2018-04-01", "2018-06-30", "orange", "orange", "nausea")
n2018q3 <- make_other_plot(naus, "2018-07-01", "2018-09-30", "orange", "orange", "nausea")
n2018q3 <- make_other_plot(naus, "2018-10-01", "2018-12-31", "orange", "orange", "nausea")

title_2017 <- ggdraw() + draw_label("2017", fontface='bold')
title_2018 <- ggdraw() + draw_label("2018", fontface='bold')
title_2019 <- ggdraw() + draw_label("2019", fontface='bold')
title_2020 <- ggdraw() + draw_label("2020", fontface='bold')

# by year
heights=c(1,1,6,2,1,6,2,1,6,2,1,6,2)
plot_grid(title_2017, NULL, NULL, NULL, NULL, NULL, NULL, c2017q3, m2017q3, NULL, c2017q4, m2017q4, align="v", nrow=14, rel_heights=heights)
ggsave(file.path(plots_dir, "migraine-plot-2017.pdf"), width=190, height=277, units="mm") # fit on A4 with 10cm borders on all sides
plot_grid(title_2018, c2018q1, m2018q1, NULL, c2018q2, m2018q2, NULL, c2018q3, m2018q3, NULL, c2018q4, m2018q4, align="v", nrow=14, rel_heights=heights)
ggsave(file.path(plots_dir, "migraine-plot-2018.pdf"), width=190, height=277, units="mm") # fit on A4 with 10cm borders on all sides
plot_grid(title_2019, c2019q1, m2019q1, NULL, c2019q2, m2019q2, NULL, c2019q3, m2019q3, NULL, c2019q4, m2019q4, align="v", nrow=14, rel_heights=heights)
ggsave(file.path(plots_dir, "migraine-plot-2019.pdf"), width=190, height=277, units="mm") # fit on A4 with 10cm borders on all sides
plot_grid(title_2020, c2020q1, m2020q1, NULL, c2020q2, m2020q2, NULL, c2020q3, m2020q3, NULL, c2020q4, m2020q4, align="v", nrow=14, rel_heights=heights)
ggsave(file.path(plots_dir, "migraine-plot-2020.pdf"), width=190, height=277, units="mm") # fit on A4 with 10cm borders on all sides

# detailed plots
# TODO: make these more useful
#heights=c(1,3,3,1,10,2,3,3,1,10)
#plot_grid( title_2017, a2017q3, n2017q3, c2017q3, m2017q3, NULL, a2017q4, n2017q4, c2017q4, m2017q4, align="v", nrow=10, rel_heights=heights)
#ggsave(file.path(plots_dir, "migraine-plot-details-2017.pdf"), width=190, height=277, units="mm") # fit on A4 with 10cm borders on all sides
#
#heights=c(1,2,2,1,8,2,2,2,1,8,2,2,2,1,8)
#plot_grid(title_2018, a2018q1, n2018q1, c2018q1, m2018q1, NULL, a2018q2, n2018q2, c2018q2, m2018q2, NULL, a2018q3, n2018q3, c2018q3, m2018q3, align="v", nrow=15, rel_heights=heights)
#ggsave(file.path(plots_dir, "migraine-plot-details-2018.pdf"), width=190, height=277, units="mm") # fit on A4 with 10cm borders on all sides
#
#heights=c(1,2,2,1,8,2,2,2,1,8,2,2,2,1,8)
#plot_grid(title_2017, a2017q3, n2017q3, c2017q3, m2017q3, NULL, a2017q4, n2017q4, c2017q4, m2017q4, title_2018, a2018q1, n2018q1, c2018q1, m2018q1, align="v", nrow=15, rel_heights=heights)
#ggsave(file.path(plots_dir, "migraine-plot-details-first3quarters.pdf"), width=190, height=277, units="mm") # fit on A4 with 10cm borders on all sides
#plot_grid(title_2018, a2018q2, n2018q2, c2018q2, m2018q2, NULL, a2018q3, n2018q3, c2018q3, m2018q3, NULL, NULL, NULL, NULL, NULL, align="v", nrow=15, rel_heights=heights)
#ggsave(file.path(plots_dir, "migraine-plot-details-second3quarters.pdf"), width=190, height=277, units="mm") # fit on A4 with 10cm borders on all sides

# all the data -- breaks need to be adjusted
# TODO: make this more useful
#start_date = "2017-07-01"
#end_date = "2018-06-30"
#m_all_1 <- make_migraine_plot(start_date, end_date)
#c_all_1 <- make_clue_plot(c, start_date, end_date, "tomato", "darkred", "")
#a_all_1 <- make_other_plot(aura, start_date, end_date, "lightgreen", "darkgreen", "aura")
#n_all_1 <- make_other_plot(naus, start_date, end_date, "orange", "darkorange", "nausea")
#start_date = "2018-07-01"
#end_date = "2019-06-30"
#m_all_2 <- make_migraine_plot(start_date, end_date)
#c_all_2 <- make_clue_plot(c, start_date, end_date, "tomato", "darkred", "")
#a_all_2 <- make_other_plot(aura, start_date, end_date, "lightgreen", "darkgreen", "aura")
#n_all_2 <- make_other_plot(naus, start_date, end_date, "orange", "darkorange", "nausea")
#plot_grid(a_all_1, n_all_1, c_all_1, m_all_1, NULL, a_all_2, n_all_2, c_all_2, m_all_2, NULL, align="v", nrow=10, rel_heights=c(2,2,1,8,3,2,2,1,8,3))
#ggsave(file.path(plots_dir, "migraine-plot-all.pdf"), width=277, height=190, units="mm") # fit on A4 with 10cm borders on all sides


# how many days has this been going on for?
days_elapsed <- as.numeric(as.POSIXct(max(m$day)) - as.POSIXct(min(m$day)))
days_with <- length(unique(m$day))
print("percent days with migraine since Aug 21 2017")
100 * days_with/days_elapsed
times <- format(m$date, "%Y-%m-%d %H")
print("percent hours with migraine since Aug 21 2017")
100 * length(unique(times)) / (days_with*24)

days_elapsed_2018 <- as.numeric(as.POSIXct(max(m$day)) - as.POSIXct("2018-01-01"))
d2018 <- m %>% filter(day > as.POSIXct("2018-01-01"))

# stats for the last n days
n <- 30
latest <- as.POSIXct(max(m$day))
last_thirty <- m %>% filter(day > (latest-n*24*60*60))
print(paste("percent days with migraine in the last ", n, "days"))
100 * length(unique(last_thirty$day))/n

# migraine days per month
# standardize the data
m[m$migraine > 6,]$migraine <- 6
m[m$migraine == 5,]$migraine <- 4
m[m$migraine == 3,]$migraine <- 4
m[m$migraine < 2,]$migraine <- 2
m$migraine <- factor(m$migraine, levels=c(6,4,2))
m$ym <- format(m$date, "%y/%m")
ggplot(m, aes(x=ym, fill=migraine)) + geom_bar() + labs(y="hours", x="month") + theme_bw() + theme(axis.text.x = element_text(size = 6))
ggsave(file.path(plots_dir, "migraine_bar.pdf"), width=277, height=190, units="mm")

m$ym <- format(m$date, "%y/%m")
migraine_per_month <- m %>% group_by(ym) %>% summarize(n=n(), mdays=length(unique(day)))
ggplot(migraine_per_month, aes(x=ym, y=mdays)) + geom_point() + theme_bw() + theme(axis.title=element_blank()) + theme(axis.line = element_line(colour = "black", size=0.5, lineend="square")) + expand_limits(y=0)
ggsave(file.path(plots_dir, "migraine_per_month.pdf"))

# aura
days_elapsed <- as.numeric(as.POSIXct(max(aura$day)) - as.POSIXct(min(aura$day)))
days_with <- length(unique(aura$day))
print("percent days with aura since Aug 21 2017")
100 * days_with/days_elapsed
times <- format(aura$date, "%Y-%m-%d %H")




### Set up libraries and directories we need

#install.packages("patchwork")
#remotes::install_github("wilkelab/ggtext")
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggtext)


OutputDataFolder <- paste0(getwd(),"/data/")

### Load clean long attendance data

Attendance_long <- read.csv(file = paste0(OutputDataFolder, "AttendanceDataForAnalysisLong.csv"))

### Load clean long recordings data

Recordings_long <- read.csv(file = paste0(OutputDataFolder, "RecordingsDataForAnalysisLong.csv"))

# Donut chart with % of students who never accessed any recordings
# Code inspired by https://github.com/jakelawlor/TidyTuesday_JL/blob/master/CodeFiles/Mar17.20.office.R

donutpal <- c("#000000","#aaa4b0")
bgcol <- "white"
titlefont <- "Helvetica"
subtitlefont <- "Helvetica"
legendfont <- "Helvetica"

donut <- Recordings_long %>% 
  replace_na(list(TimesViewed = 0, PerCentViewed = 0)) %>% 
  group_by(ParticipantStudyID) %>% 
  summarise(AvgViewings = mean(TimesViewed)) %>% 
  mutate(ViewedSomething = case_when(AvgViewings == 0 ~ "No",
                                     AvgViewings != 0 ~ "Yes")) %>% 
  group_by(ViewedSomething) %>% 
  summarise(count=n()) %>%
  mutate(percent = round(count/n_distinct(Recordings_long$ParticipantStudyID)*100),
         lab.pos = cumsum(percent) - .5*percent, # position of labels
         ViewedSomething= factor(ViewedSomething,levels=c("Yes", "No"))
  ) %>% 
  ggplot(aes(x=2,y=percent))+
  geom_bar(aes(x=2,fill=ViewedSomething),stat="identity",show.legend = F) +  
  coord_polar("y",start=0.3)+ # start is rotation clockwise
  geom_text(aes(y=lab.pos, label = paste(percent,"%", sep = "")), col = c("white","white")) +
  xlim(-0.15,2.5)+ # shape of the donut
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title="How many students watch recordings?",
       subtitle = "20% of our students didn't watch any recordings")+
  scale_fill_manual(values = donutpal) +
  geom_richtext(aes(label="<span style = 'color:#aaa4b0'>Watched<br>nothing<br></span><span style = 'color:#000000'> Watched <br>something </span>",
                    x=-.15,y=0),
                fill=NA, label.color=NA,
                family="American Typewriter Bold",
                size=5)

### Histogram of the number of views (with zeros filtered out)

RecViewsHistogram <- Recordings_long %>%
  filter(TimesViewed > 0) %>%
  mutate(TimesViewed = ifelse(TimesViewed > 10, 10, TimesViewed)) %>% 
  ggplot(aes(x=TimesViewed)) +
  geom_histogram(binwidth = 1, color = "black", fill = "black") +
  scale_x_continuous(breaks=seq(1, 10, 1), label = c(1:9, "10+")) +
  expand_limits(y=c(0,2000)) +
  labs(title = "How many times are recordings watched?", 
       subtitle = "Most students will watch a recording only once",
       x = "No of times watched") +
  theme(panel.grid.minor = element_blank())

### Histogram of the length of viewings (with zeros filtered out)

RecPercentHistogram <- Recordings_long %>%
  filter(PerCentViewed > 0) %>%
  ggplot(aes(x=PerCentViewed)) +
  geom_histogram(binwidth = 10, center = 5, color = "black", fill = "black") +
  scale_x_continuous(breaks=seq(0,100,10)) +
  expand_limits(y=c(0,2000)) +
  labs(title = "How much of the recording is watched?", 
       subtitle = "When a recording is watched, it is usually watched in full",
       x = "% of video duration watched") +
  theme(panel.grid.minor = element_blank())

### Joining the recordings file with attendance file

Attendance_Use <- Attendance_long %>%
  full_join(Recordings_long, by = c("ParticipantStudyID", "Date")) %>%
  replace_na(list(TimesViewed = 0, PerCentViewed = 0)) %>%
  mutate(Watched = ifelse(TimesViewed == 0, 0, 1)) %>%
  mutate(WatchedOverHalf = ifelse(PerCentViewed > 49, 1, 0))

WatchedvsAttendedProportions_ByLecture <- rbind(Attendance_Use %>%
                                                  drop_na() %>%
                                                  dplyr::filter(Attendance == 1) %>%
                                                  dplyr::group_by(Date) %>%
                                                  dplyr::summarise(Watch = mean(as.numeric(as.character(Watched)), na.rm = T)) %>%
                                                  dplyr::mutate(Attend = "Yes"),
                                                Attendance_Use %>%
                                                  drop_na() %>%
                                                  dplyr::filter(Attendance == 0) %>%
                                                  dplyr::group_by(Date) %>%
                                                  dplyr::summarise(Watch = mean(as.numeric(as.character(Watched)), na.rm = T)) %>%
                                                  dplyr::mutate(Attend = "No")) %>% 
  mutate(Date = substr(Date, 6, 10))

### Line chart with proportions of students who watched each recording, by attendance status

AttendanceVsWatched <- ggplot(WatchedvsAttendedProportions_ByLecture, aes(x=as.factor(Date), y=Watch, group=Attend)) +
  geom_line(aes(linetype=as.factor(Attend), color=as.factor(Attend)), size=1) +
  geom_point(aes(color=as.factor(Attend))) +
  labs(x = "Lecture date", y = "Students who accessed recording") +
  theme_bw() +
  expand_limits(y=c(0,1)) +
  scale_y_continuous(labels = scales::percent, breaks=seq(0,1,0.2)) +
  scale_colour_manual(values=c("black", "#aaa4b0"),
                      name  ="Lecture Attended",
                      breaks=c("No", "Yes"),
                      labels=c("No", "Yes")) +
  scale_linetype_manual(values=c("solid", "dashed"),
                        name  ="Lecture Attended",
                        breaks=c("No", "Yes"),
                        labels=c("No", "Yes")) +
  theme(
    legend.position = "none",
    axis.line= element_line(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(color="black", size=16, face = "bold"),
    axis.text.x = element_text(color="black", size=16, angle = 45, hjust = 1),
    axis.text.y = element_text(color="black", size=16))

AttendanceVsWatched <- AttendanceVsWatched +
  annotate(geom = "text", x = 1, y = .65, 
           label = "% of students who didn't attend and then watched the video", hjust = "left", size = 7) +
  annotate(geom = "text", x = 1, y = .10, label = "% of students who attended and then watched the video", 
           hjust = "left", size = 7, colour = "#aaa4b0") 

### Combining everything on one page using patchwork

row1 <- donut + RecViewsHistogram + RecPercentHistogram + plot_layout(widths = c(0.33, 0.33, 0.33))
row2 <- AttendanceVsWatched

fullplot <- row1 / row2

fullplot <- fullplot+
  plot_annotation(title="How do students use lecture recordings?",
                  subtitle = "Case study in a first-year psychology course (n = 327) \n",
                  caption = "R packages: ggplot & patchwork | Vis: @edinkasia",
                  theme = theme(text=element_text(family="Helvetica"),
                                plot.background = element_rect(fill=bgcol,color=bgcol),
                                plot.title = element_text(size=30,hjust=.5),
                                plot.subtitle = element_markdown(size=18,hjust=.5, family="Helvetica"),
                                plot.caption = element_text(hjust=.5),
                                plot.margin = margin(20,10,15,10)) 
  )

fullplot

########## Buena Park Historical Water Loss Analysis (Form 38) ##########

##### Set Up Workspace ----------
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(gdata)
options(scipen = 9999)
copy <- function(x) {
  write.table(x, "clipboard", sep="\t", row.names = F) 
} #Used to very quickly copy dataframes to paste into excel.
f1 <-   function(x, select = T) { 
  runs <- rle(x) 
  with(runs, max(lengths[values == select], 0)) 
}
na.scan <- function(x) {
  sum(is.na(x)) # or... x %>% is.na() %>% sum()
}
plot.dir <- "C:\\Users\\Kris\\Google Drive\\640 - MWDOC\\Buena Park\\Buena Park Data Sharing\\Received\\R_Output"

##### Load Data ----------

form.2010 <- read.xls("C:\\Users\\Kris\\Google Drive\\640 - MWDOC\\Buena Park\\Buena Park Data Sharing\\Received\\CA State Form 38\\38_one_2010.xls", stringsAsFactors = F)
form.2011 <- read.xls("C:\\Users\\Kris\\Google Drive\\640 - MWDOC\\Buena Park\\Buena Park Data Sharing\\Received\\CA State Form 38\\38_one_2011.xls", stringsAsFactors = F)
form.2012 <- read.xls("C:\\Users\\Kris\\Google Drive\\640 - MWDOC\\Buena Park\\Buena Park Data Sharing\\Received\\CA State Form 38\\38_one_2012.xls", stringsAsFactors = F)
form.2013 <- read.xls("C:\\Users\\Kris\\Google Drive\\640 - MWDOC\\Buena Park\\Buena Park Data Sharing\\Received\\CA State Form 38\\38_one_2013.xls", stringsAsFactors = F)
form.2014 <- read.xls("C:\\Users\\Kris\\Google Drive\\640 - MWDOC\\Buena Park\\Buena Park Data Sharing\\Received\\CA State Form 38\\38_one_2014.xls", stringsAsFactors = F)
form.2015 <- read.xls("C:\\Users\\Kris\\Google Drive\\640 - MWDOC\\Buena Park\\Buena Park Data Sharing\\Received\\CA State Form 38\\38_one_2015.xls", stringsAsFactors = F)
form.list <- c("form.2010", "form.2011", "form.2012", "form.2013", "form.2014", "form.2015")
year.list <- c(2010, 2011, 2012, 2013, 2014, 2015)
output <- data.frame(
  file = as.character("test"),
  month = as.character("test"),
  year = as.numeric(0),
  well.prod = as.numeric(0),
  mwd.prod = as.numeric(0),
  sf.cons = as.numeric(0),
  mf.cons = as.numeric(0),
  com.cons = as.numeric(0),
  ind.cons = as.numeric(0),
  li.cons = as.numeric(0),
  oth.cons = as.numeric(0)
)


for (i in seq_along(form.list)) {
  file <- as.character(form.list[i])
  well.prod <- as.numeric(as.vector(get(form.list[i])[15, 5:16]))
  mwd.prod <- as.numeric(as.vector(get(form.list[i])[17, 5:16]))
  sf.cons <- as.numeric(as.vector(get(form.list[i])[25, 5:16]))
  mf.cons <- as.numeric(as.vector(get(form.list[i])[26, 5:16]))
  com.cons <- as.numeric(as.vector(get(form.list[i])[27, 5:16]))
  ind.cons <- as.numeric(as.vector(get(form.list[i])[28, 5:16]))
  li.cons <- as.numeric(as.vector(get(form.list[i])[29, 5:16]))
  oth.cons <- as.numeric(as.vector(get(form.list[i])[30, 5:16]))
  month <- as.character(as.vector(get(form.list[i])[14, 5:16]))
  year <- as.numeric(as.vector(year.list[i]))
  cleaned <- cbind(file,
                   month,
                   year,
                   well.prod, 
                   mwd.prod,
                   sf.cons,
                   mf.cons,
                   com.cons,
                   ind.cons,
                   li.cons,
                   oth.cons
                  )
  output <- rbind(output, cleaned)
}

for (i in 3:ncol(output)){
  output[,i] <- as.numeric(output[, i])
}
output <- output[-1, ] 
output[is.na(output)] <- 0
output$total.prod <- output$well.prod + output$mwd.prod
output$total.cons <- with(output, sf.cons + mf.cons + com.cons + ind.cons + li.cons + oth.cons)
output$date <- paste(output$year, output$month, "01", sep = "-")
output$date <- as.Date(output$date, format = "%Y-%b-%d")
output$simpdate <- strftime(output$date, format = "2015-%m-%d")
output$simpdate <- as.Date(output$simpdate)
output$year <- strftime(output$date, format = "%Y")
output$year <- as.numeric(output$year)
cat.cons <- melt(output, id.vars = c("date", "simpdate", "year"), measure.vars = c("sf.cons", "mf.cons", "com.cons", "ind.cons", "li.cons", "oth.cons" ))
cat.prod <- melt(output, id.vars = c("date", "simpdate", "year"), measure.vars = c("well.prod", "mwd.prod"))
sum <- melt(output, id.vars = c("date", "simpdate", "year"), measure.vars = c("total.prod", "total.cons"))
year.sum <- sum %>% 
  group_by(variable, year) %>% 
  select(value) %>% 
  summarize(
    value.sum = sum(value)
  )

p.cat.cons <- ggplot(cat.cons, aes(simpdate, value, fill = variable)) +
  facet_wrap(facets = c("year"), ncol = 1) +
  geom_bar(stat = "identity") +
  ggtitle("Form 38 Consumption Over Time") +
  scale_fill_brewer(palette = "Set1", 
                    name="Consumption Type",
                    breaks=c("sf.cons", "mf.cons", "com.cons", "ind.cons", "li.cons", "oth.cons"),
                    labels=c("Single Family", "Multi Family", "Commercial", "Industrial", "Landscape", "Other")) +
  ylab("Volume (AF)") +
  xlab("Month") +
  scale_x_date(labels = date_format("%b"), breaks = "months") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave("bp_form38_consumption.png", width = 6, height = 9, path = plot.dir)

p.cat.prod <- ggplot(cat.prod, aes(simpdate, value, fill = variable)) +
  facet_wrap(facets = c("year"), ncol = 1) +
  geom_bar(stat = "identity") +
  ggtitle("Form 38 Production Over Time") +
  scale_fill_brewer(palette = "Set1", 
                    name="Production Source",
                    breaks=c("well.prod", "mwd.prod"),
                    labels=c("Wells", "MWD Import")) +
  ylab("Volume (AF)") +
  xlab("Month") +
  scale_x_date(labels = date_format("%b"), breaks = "months") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave("bp_form38_production.png", width = 6, height = 9, path = plot.dir)

p.tot <- ggplot(sum, aes(simpdate, value, fill = variable)) +
  facet_wrap(facets = c("year"), ncol = 1) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Form 38 Production and Consumption Over Time") +
  scale_fill_brewer(palette = "Set1", 
                    name="",
                    breaks=c("total.prod", "total.cons"),
                    labels=c("Production", "Consumption")) +
  ylab("Volume (AF)") +
  xlab("Month") +
  scale_x_date(labels = date_format("%b"), breaks = "months") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave("bp_form38_comparison.png", width = 6, height = 9, path = plot.dir)


p.tot.year <- ggplot(year.sum, aes(year, value.sum, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Form 38 Production and Consumption Over Time") +
  scale_fill_brewer(palette = "Set1", 
                    name="",
                    breaks=c("total.prod", "total.cons"),
                    labels=c("Production", "Consumption")) +
  scale_x_continuous(breaks = c(2010:2015)) +
  ylab("Volume (AF)") +
  xlab("Year") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave("bp_form38_yearlysummary.png", width = 6, height = 4, path = plot.dir)


p.2015 <- p.tot <- ggplot(sum[sum$year == 2015, ], aes(simpdate, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Form 38 Production and Consumption 2015") +
  scale_fill_brewer(palette = "Set1", 
                    name="",
                    breaks=c("total.prod", "total.cons"),
                    labels=c("Production", "Consumption")) +
  ylab("Volume (AF)") +
  xlab("Month") +
  scale_x_date(labels = date_format("%b"), breaks = "months") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave("bp_form38_2015.png", width = 6, height = 4, path = plot.dir)

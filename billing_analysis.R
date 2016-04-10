########## CLIENT NAME Billing Analysis ##########
# Audit period:
# Units:

##### Set Up Workspace ----------
library(plyr)
library(dplyr)
library(reshape2)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(ggally)
library(xlsx)
library(lubridate)
options(scipen = 9999)
copy <- function(x) {
  write.table(x, "clipboard", sep="\t", row.names = F) 
} #Used to very quickly copy dataframes to paste into excel.
runs.zero <-   function(x, select = T) { 
  runs <- rle(x) 
  with(runs, max(lengths[values == select], 0)) 
}
na.scan <- function(x) {
  sum(is.na(x)) # or... x %>% is.na() %>% sum()
}
plot.dir <- "\\FILE PATH\\"

##### Clean Data ----------
start.date <- as.Date("") # remember to use "2015-01-01" format here.
end.date <- as.Date("")
billing <- read.csv("\\FILE PATH\\", stringsAsFactors = F, header = T)
billing$location.id <- as.character(billing$loc_no)
billing$act.id <- as.character(billing$serv_id)
billing$meter.id <- as.character(billing$meter_no)
billing$meter.size <- revalue(billing$desc, c(
  "1 METER\"" = "1 Inch",
  "1 1/2 METER\"" = "1 1/2 Inch",
  "2 METER\"" = "2 Inch",
  "4 METER\"" = "4 Inch",
  "6 METER\"" = "6 Inch",
  "8 METER\"" = "8 Inch",
  "3/4 METER\"" = "3/4 Inch",
  "5/8 METER\"" = "5/8 Inch",
  "OVE" = "OVE",
  "10 METER\"" = "10 Inch",
  "2 1/2 FLOODING METER\"" = "2 1/2 Inch Flooding"))
billing$meter.size <- factor(billing$meter.size, levels = c("OVE",
                                                            "5/8 Inch", 
                                                            "3/4 Inch", 
                                                            "1 Inch",  
                                                            "1 1/2 Inch", 
                                                            "2 Inch",
                                                            "2 1/2 Inch Flooding",
                                                            "4 Inch", 
                                                            "6 Inch", 
                                                            "8 Inch", 
                                                            "10 Inch" ), ordered = T )
billing$service.type <- as.character(billing$service)
billing$read.date <- as.Date(billing$cur_date, format = "")
billing$prev.date <- as.Date(billing$prior_date, format = "")
billing$read.month <- strftime(billing$read.date, format = "%Y-%m-01")
billing$read.month <- as.Date(billing$read.month)
billing$read.val <- as.numeric(billing$cur_read)
billing$prev.val <- as.numeric(billing$prior_read)
billing$water.cons <- as.numeric(billing$act_usage)
billing$adj.cons <- as.numeric(billing$adj_usage)
billing$adj.flag <- as.character(billing$type_str)
billing$audit <- F
billing$audit[billing$read.date >=  start.date & billing$read.date <= end.date] <- T
apply(billing, 2, na.scan) 


########## EVALUATING DATA COMPLETENESS ----------

##### Count of Accounts per Month ----------
acts.month <- billing %>% 
  group_by(month = read.month) %>% 
  select(act.id, audit) %>% 
  summarize(
    count = n_distinct(act.id),
    audit = last(audit)
  )

p.acts <- ggplot(data = acts.month, aes(month, count, fill = audit)) +
  geom_bar(stat= "identity") +
  ggtitle("Count of Accounts by Read Month") +
  scale_fill_manual("", values = c("grey", "steelblue"), labels = c("Out of Audit", "In Audit")) +
  ylab("Count") +
  xlab("Month") +
  scale_x_date(labels = date_format("%b-%Y"), breaks = "months") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )
ggsave("ActsbyMonth.png",  width = 6, height = 3, path = plot.dir)

##### Count of Records per Month ----------
recs.month <- billing %>% 
  group_by(month = read.month) %>% 
  select(audit) %>% 
  summarize(
    count = n(),
    audit = last(audit)
  )
p.rec <- ggplot(data = recs.month, aes(month, count, fill = audit)) +
  geom_bar(stat= "identity") +
  ggtitle("Count of Records by Read Month") +
  scale_fill_manual("", values = c("grey", "steelblue"), labels = c("Out of Audit", "In Audit")) +
  ylab("Count") +
  xlab("Month") +
  scale_x_date(labels = date_format("%b-%Y"), breaks = "months") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )
ggsave("RecsbyMonth.png",  width = 6, height = 3, path = plot.dir)

##### Count of Bills per Account ----------

rec.cust <- billing %>% 
  filter(audit == T) %>% 
  group_by(act.id) %>% # You may consider grouping by location and meter ID as an alternative
  summarize(
    rec = n()
  ) %>% 
  group_by(rec) %>% 
  summarise(
    count = n()
  ) %>% 
  mutate(prop = count/sum(count))

p.rec.act <- ggplot(rec.cust, aes(rec, count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Records Per Account ID") + # remember to change this depending on how you grouped.
  ylab("Count of Accounts") + # remember to change this depending on how you grouped.
  xlab("Count of Records") +
  theme_minimal()
ggsave("RecsperAct.png",  width = 6, height = 3, path = plot.dir)


########## DATABASE INTEGRITY CHECKS ----------

##### Duplicate Reads ----------
dup.fields <- c("act.id", "read.val", "read.date", "water.cons")
dup.one <- duplicated(billing[ , dup.fields], fromLast = TRUE)
dup.two <- duplicated(billing[ , dup.fields], fromLast = FALSE)
dup.reads <- billing[dup.one | dup.two, ] 
# lists duplicate reads, defined as reads with the same value, the same read date, and the same Account ID. 
#The two calls of the duplicated command run through the rows in reverse directions to catch all instances -- similar to excels "remove duplicates" command.

write.csv(dup.reads, paste(plot.dir, "\\dup.csv", sep = ""))

##### Comparing Consumption to Readings -------
billing$calc.cons <- billing$read.val - billing$prev.val
cond <- which(billing$read.val < billing$prev.val & !is.na(billing$prev.val) & !is.na(billing$read.val))
billing$calc.cons[cond] <- (10^nchar(billing$prev.val[cond])-billing$prev.val[cond]) + billing$read.val[cond]
calc.chk <- billing[billing$water.cons != billing$calc.cons, ]
calc.chk <- calc.chk[!is.na(calc.chk$calc.cons),] 
# Note that this section can be susceptable to floating point error if the values are very close.

write.csv(temp, paste(plot.dir, "\\calc_check.csv", sep = ""))

##### Negative Reads ----------
neg <- billing[billing$water.cons < 0 & billing$audit == T,] 
# Creates a dataframe of all records that show negative consumption.

##### Meters with Multiple Sizes ----------
meter.size.count <- billing %>% 
  filter(audit == T) %>% 
  group_by(meter.id) %>% 
  select(meter.size) %>% 
  summarize(
    size.count = n_distinct(meter.size),
    size.list = as.character(list(unique(as.character(meter.size))))
  ) %>% 
  filter(size.count > 1)

##### Accounts with No Consumption ----------
no.cons <- billing %>% # take billing and then...
  filter(audit == T) %>% 
  group_by(act.id) %>% # group by cust ID and then...
  select(water.cons, service.type) %>% # use water consumption and then...
  summarise(
    loc.total = sum(water.cons),
    service = as.character(list(unique(as.character(service.type))))) %>% 
  filter(loc.total == 0)

##### Consecutive Zero Reads ----------
billing <- arrange(billing, act.id, read.date) # You might consider sorting by both location and meter ID depending on what is available.

consec <- billing %>% # take billing and then...
  filter(audit == T) %>% 
  group_by(act.id) %>% # group by account ID and then...
  select(water.cons) %>% # use water consumption and then...
  summarise(
    max.consec.zero = runs.zero(water.cons),
    act.total = sum(water.cons))


consec.sum <- consec %>% 
  filter(act.total > 0) %>% # Note this excludes instances where the audit total is zero.
  group_by(max.consec.zero) %>% 
  summarize(
    count = n()
  ) %>% 
  mutate(prop = count / sum(count))

consec.list <- c(unique(unlist(as.character(consec$act.id[consec$max.consec.zero >= 6 & consec$act.total != 0])))) 
# Create a list of accounts that have more than 11 consecutive zero reads for an account pair
for (i in seq_along(consec.list)) { # plot these accounts with more than 11 consecutive zero reads but also have water consumption.
  ggplot(billing[billing$act.id == paste(consec.list[i]),], aes(read.date, water.cons, color = meter.id)) + 
    geom_point() + 
    ggtitle(paste(consec.list[i])) + ylab("Volume") + xlab("Date") +
    theme_minimal()
  ggsave(paste("consec", consec.list[i], ".png", sep = ""), height = 4, width = 6, path = plot.dir)
}

##### Large Readings Analysis ----------
# create a list of the account numbers that have the most consumption for the audit period.
big.cons <- as.character(unique(top_n(billing, 200, water.cons)$act.id))
for (i in seq_along(big.cons)){ # plot the list of accounts
  ggplot() +
    geom_point(data = billing[billing$act.id == big.cons[i],], aes(read.date, water.cons, color = meter.id)) + 
    ggtitle(paste(big.cons[i])) + 
    ylab("Volume") + 
    xlab("Date") +
    theme_minimal() 
  ggsave(paste("large", big.cons[i], ".png", sep = ""), width = 6, height = 4, path = plot.dir)
}



# List of suspicious accounts from visual inspection of the graphs.
susp.list <- c("2400232", "2449306", "2459796")

high.cons.sum <- billing %>% 
  filter(act.id %in% susp.list) %>% 
  group_by(act.id) %>%
  summarize(
    total = sum(water.cons),
    svc = as.character(list(unique(as.character((service.type))))),
    avg = mean(water.cons),
    max.cons = max(water.cons),
    max.date = read.date[which(water.cons == max(water.cons))]
  )  %>% 
  mutate(diff.mean = max.cons - avg)

for (i in seq_along(susp.list)){ # plot the list of accounts
  ggplot() +
    geom_point(data = billing[billing$act.id == susp.list[i],], aes(read.date, water.cons, color = meter.id)) + 
    ggtitle(paste(susp.list[i])) + 
    ylab("Volume") + 
    xlab("Date") +
    theme_minimal() 
  ggsave(paste("susp_large", susp.list[i], ".png", sep = ""), width = 6, height = 4, path = plot.dir)
}


########## CALCULATING BMAC, APPORTIONMENT, AND TIME SENSITIVITY ANALYSIS ----------
##### BMAC per Month ----------
read.month <- billing %>% 
  group_by(date = read.month) %>% 
  select(water.cons, audit) %>% 
  summarize(
    total = sum(water.cons, na.rm = T),
    audit = last(audit)
  )
p.bmac <- ggplot(data = read.month, aes(date, total, fill = audit)) +
  geom_bar(stat= "identity") +
  ggtitle("BMAC by Read Month") +
  scale_fill_manual("", values = c("grey", "steelblue"), labels = c("Out of Audit", "In Audit")) +
  ylab("Volume") +
  xlab("Month") +
  scale_x_date(labels = date_format("%b-%Y"), breaks = "months") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )
ggsave("BMACbyMonth.png",  width = 6, height = 3, path = plot.dir)

##### Apportionment ----------
sum(billing$read.date == billing$prev.date) # This is important to check before running apportionment
sum(billing$read.date < billing$prev.date) # This is important to check before running apportionment
billing$prev.date[billing$prev.date == billing$read.date] <- billing$prev.date[billing$prev.date == billing$read.date] - 1
billing$days <- difftime(billing$read.date, billing$prev.date, units = "days")
billing$days <- as.numeric(billing$days)
billing$ave.flow <- billing$water.cons/billing$days

max.date <- max(billing$read.date)
day(max.date) <- 1
min.date <- as.Date(billing$prev.date)
day(min.date) <- 1
months <- seq.Date(min.date, max.date, by = "months")
months <- strftime(months, format = "%B.%Y")
eom <- seq.Date(min.date, max.date, by = "months")
day(eom) <- days_in_month(eom)
bom <- seq.Date(min.date, max.date, by = "months")
bom <- bom - 1 

# Check to make sure there aren't a ton of months to apportion over (>100)

aprtn <- data.frame(
  NewCol = rep(as.numeric(NA), nrow(billing))
)

for (i in seq_along(months)){
  aprtn$NewCol <- NA
  a <- which(billing$prev.date <= bom[i] & billing$read.date >= eom[i] & !is.na(billing$prev.date) & !is.na(billing$read.date))
  aprtn$NewCol[a] <- billing$ave.flow[a]*as.numeric(difftime(eom[i], bom[i], units = "days"))
  b <- which(billing$prev.date <= bom[i] & billing$read.date <= eom[i] & !is.na(billing$prev.date) & !is.na(billing$read.date))
  aprtn$NewCol[b] <- billing$ave.flow[b]*as.numeric(difftime(billing$read.date[b], bom[i], units = "days"))
  c <- which(billing$prev.date > bom[i] & billing$read.date >= eom[i] & !is.na(billing$prev.date) & !is.na(billing$read.date))
  aprtn$NewCol[c] <- billing$ave.flow[c]*as.numeric(difftime(eom[i], billing$prev.date[c], units = "days"))
  d <- which(billing$prev.date >= bom[i] & billing$read.date <= eom[i] & !is.na(billing$prev.date) & !is.na(billing$read.date))
  aprtn$NewCol[d] <- billing$ave.flow[d]*as.numeric(difftime(billing$read.date[d], billing$prev.date[d], units = "days"))
  aprtn$NewCol[billing$prev.date > eom[i]] <- 0
  aprtn$NewCol[billing$read.date < bom[i]] <- 0
  colnames(aprtn)[ncol(aprtn)] <- paste(months[i])
}

aprt <- melt(aprtn) # Note the close names here between aprt and aprtn.
colnames(aprt) <- c("month", "cons")
aprt$date <- paste(aprt$month, ".01", sep = "")
aprt$date <- as.Date(aprt$date, format = "%B.%Y.%d")

aprt.sum <- aprt %>% 
  group_by(date) %>% 
  select(cons) %>% 
  summarize(
    aprt.total = sum(cons)
  )
aprt.sum$audit <- F
aprt.sum$audit[aprt.sum$date >= start.date & aprt.sum$date <= end.date] <- T

p.aprt.vol <- ggplot(data = aprt.sum, aes(date, aprt.total, fill = audit)) +
  geom_bar(stat= "identity") +
  ggtitle("Apportioned Volume Consumed") +
  ylab("Volume") +
  xlab("Month") +
  scale_x_date(labels= date_format("%b-%Y"), breaks = "months") +
  scale_fill_manual("", values = c("grey", "steelblue"), labels = c("Out of Audit", "In Audit")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave("BMACAprt.png",  width = 6, height = 3, path = plot.dir)

cons.sum <- join(aprt.sum, read.month, by = c("date"))
cons.melt <- melt(cons.sum, id.vars = c("date", "audit"), measure.vars = c("aprt.total", "total"))

p.cons.sum <- ggplot(data = cons.melt, aes(date, value, fill = variable, alpha = audit)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Read Month vs. Apportioned Consumption") +
  scale_alpha_manual("", breaks = c("TRUE", "FALSE"), values = c(.5, 1), labels = c("In Audit", "Out of Audit")) +
  scale_fill_brewer("", breaks = c("aprt.total", "total"), palette = "Set1", labels = c("Apportioned", "By Read Date")) +
  ylab("Volume") +
  xlab("Month") +
  scale_x_date(labels= date_format("%b-%Y"), breaks = "months")+
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) 
ggsave("BMACAprt_ReadMonth.png",  width = 6, height = 3, path = plot.dir)

##### Time Range Sensitivity Analysis ----------
start.sens <- start.date - months(2)
end.sens <- end.date
day(end.sens) <- 1
end.sens <- end.sens - months(1)
start.seq <- seq.Date(start.sens, start.sens + months(4), by = "months")
end.seq <- seq.Date(end.sens, end.sens + months(5) - days(1), by = "months")
end.seq <- end.seq - 1 

output <- data.frame(
  start = as.character("test"),
  end = as.character("test"),
  bmac = as.numeric(0),
  aprt.bmac = as.numeric(0)
)

# This works but is a good candidate for rewriting.
for (i in 1:length(start.seq)){
  start <- as.character(start.seq[i])
  end <- as.character(end.seq[i])
  bmac <- sum(cons.melt$value[cons.melt$variable == "total" & cons.melt$date >= start.seq[i] & cons.melt$date <= end.seq[i]])
  aprt.bmac <- sum(cons.melt$value[cons.melt$variable == "aprt.total" & cons.melt$date >= start.seq[i] & cons.melt$date <= end.seq[i]])
  cleaned <- data.frame(
    start = start,
    end = end,
    bmac = bmac,
    aprt.bmac = aprt.bmac
  )
  output <- rbind(output, cleaned)
}
output <- output[-1, ]
sens <- output
sens <- sens %>% 
  mutate(pct.diff.bmac = (bmac-bmac[3])/bmac[3], pct.diff.aprt = (aprt.bmac-aprt.bmac[3])/aprt.bmac[3])

##### Service Type Consumption Breakdown ----------
svc.sum <- billing %>%
  group_by(service.type, read.month) %>% 
  select(water.cons, act.id, meter.id, audit) %>% 
  summarize(
    svc.total = sum(water.cons, na.rm = T),
    recs = n(),
    act = n_distinct(act.id),
    mtr = n_distinct(meter.id),
    audit = last(audit)
  )

p.svc.type <- ggplot(data = svc.sum, aes(read.month, svc.total, fill = service.type, alpha = audit)) +
  geom_bar(stat = "identity", position = "stack") +
  ggtitle("BMAC by Read Month and Service Type") +
  ylab("Volume") +
  xlab("Month") +
  scale_alpha_manual("", breaks = c("TRUE", "FALSE"), values = c(.5, 1), labels = c("In Audit", "Out of Audit")) +
  scale_fill_brewer("Service Type", palette = "Set1") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("BMACbyServiceTypeandMonth.png",  width = 6, height = 3, path = plot.dir)

##### Account Consumption Distribution ----------

#distribution by customers
dist <-
  billing %>% 
  group_by(act.id) %>% 
  select(water.cons) %>% 
  summarize(
    total = sum(water.cons)
  ) %>% 
  mutate(pct = ntile(total, 100)) %>% 
  group_by(pct) %>% 
  summarize(total = sum(total)) %>% 
  mutate(total.pct = total/sum(total)*100)

p <- ggplot() +
  geom_bar(data = dist, aes(pct, total.pct), stat = "identity", fill = "steelblue") +
  ggtitle("Total Consumption by Percentile") +
  xlab("Percentile") +
  ylab("Percent of Total Consumption") +
  theme_minimal()
ggsave("cust_distribution.png",  width = 6, height = 3, path = plot.dir)


########## METER CONSUMPTION RANGE ANALYSIS ----------

##### Breakdown of Meter Stock ----------

meter.sum <- billing  %>% 
  filter(audit == T) %>% 
  group_by(size = meter.size)  %>% 
  select(meter.id, water.cons)  %>% 
  summarize(
    rec = n(),
    count = n_distinct(meter.id),
    volume = sum(water.cons)
  )

p.mtr.count <- ggplot(data = meter.sum, aes(size, count)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  ggtitle("Counts of Meter by Size") +
  ylab("Count") +
  xlab("Meter Size") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) 
  ) # This is misleading becuase there are ~6000 records where there is no meter ID listed.
ggsave("meter.count.png", width = 6, height = 4, path = plot.dir)

p.mtr.cons <- ggplot(data = meter.sum, aes(size, volume)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  ggtitle("Water Consumption by by Meter Size") +
  ylab("Volume") +
  xlab("Meter Size") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) 
ggsave("meter.sum.png", width = 6, height = 4, path = plot.dir)

##### Average Flow by Meter Size ----------
billing$days <- difftime(billing$read.date, billing$prev.date, units = "days")
billing$days <- as.numeric(billing$days)
billing$ave.flow <- billing$water.cons/billing$days

sizes <- c("Unknown", "5/8 Inch", "3/4 Inch", "1 Inch", "1 1/2 Inch", "2 Inch", "3 Inch", "4 Inch", "6 Inch", "8 Inch")
size.files <- c("Unknown", ".625 Inch" ,".75 Inch", "1 Inch", "1.5 Inch", "2 Inch", "3 Inch", "4 Inch", "6 Inch", "8 Inch")
meter.cons <- billing %>% 
  filter(audit == T) %>% 
  group_by(meter.id) %>% 
  select(water.cons, meter.size, days, service.type) %>% 
  summarize(
    total  = sum(water.cons),
    flow.day = sum(water.cons)/sum(days),
    size = last(meter.size),
    percent.bmac = sum(water.cons)/bmac*100,
    type = last(service.type),
    count.type = n_distinct(service.type)
  )

for (i in 1:length(sizes)) {
  p <- ggplot(meter.cons[meter.cons$size == sizes[i], ], aes(total))+
    geom_density(fill = "steelblue", binwidth = 10) +
    ggtitle(sizes[i]) +
    ylab("Count") +
    xlab("Total Consumption") +
    theme_minimal()
  print(p)
}

top.meters.pct <- as.character(top_n(meter.cons, 100, meter.cons$percent.bmac)$meter.id)
meter.cons$out[meter.cons$meter.id %in% top.meters.pct] <- "Top 100"
meter.cons$out[!meter.cons$meter.id %in% top.meters.pct] <- ""
rectangles <- data.frame(
  meter.size = factor(sizes, levels = sizes, ordered = T),
  ymax = 100,
  fill = rep(c("grey", "white"), 5)
)

p.range <- ggplot() +
  geom_bar(data = rectangles, 
           aes(meter.size, ymax, fill = fill), 
           alpha = 0.75,
           width = 1,
           stat = "identity") +
  geom_jitter(data = meter.cons[meter.cons$total > 0 & !is.na(meter.cons$size) & meter.cons$meter.id != "1582977", ], 
              aes(size, flow.day, color = out),
              alpha = .5,  
              size = 3) +
  scale_color_manual("", values = c("steelblue", "red"), breaks = "Top 100") +
  scale_fill_manual("", values = c("gray97", "white")) +
  guides(fill = FALSE) +
  ggtitle("Distribution of FY15 Consumption by Meter Size") +
  xlab("Meter Size") +
  ylab("Average Consumption per Day") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )
ggsave("ConsumptionRange.png", width = 9, height = 5, path = plot.dir)

p.range.svc <- ggplot() +
  geom_bar(data = rectangles, 
           aes(meter.size, ymax, fill = fill), 
           alpha = 0.75,
           width = 1,
           stat = "identity") +
  geom_jitter(data = meter.cons[meter.cons$total > 0 & !is.na(meter.cons$size), ], 
              aes(size, flow.day, color = type),
              alpha = .5,  
              size = 3) +
  scale_fill_manual("", values = c("gray97", "white")) +
  guides(fill = FALSE) +
  ggtitle("Distribution of FY15 Consumption by Meter Size") +
  xlab("Meter Size") +
  ylab("Average Consumption per Day") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )
ggsave("ConsumptionRange_Service.png", width = 9, height = 5, path = plot.dir)

for (i in 1:length(sizes)) {
  p <- ggplot() +
    geom_jitter(data = meter.cons[meter.cons$total > 0 & !is.na(meter.cons$size) & meter.cons$size == sizes[i],], 
                aes(size, flow.day, color = out),
                size = 3,
                alpha = .5) +
    scale_color_manual("", values = c("steelblue", "red"), breaks = "Top 100") +
    ggtitle(paste("Distribution of FY15 Consumption\nfor ", sizes[i], " Meters", sep = "")) +
    xlab("Meter Size") +
    ylab("Consumption per Day (CCF/Day)") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) 
  print(p)
  Sys.sleep(2)
}

##### Alternate Consumption Range Analysis
# We need to write a version that mimics the old methodology
# It should use the cut command to categorize average flow into groups and then plot the counts by size class


### Question 2
# urls to download files
link_1 = 'https://data.nola.gov/api/views/28ec-c8d6/rows.csv?accessType=DOWNLOAD'
link_2 = 'https://data.nola.gov/api/views/rv3g-ypg7/rows.csv?accessType=DOWNLOAD'
link_3 = 'https://data.nola.gov/api/views/5fn8-vtui/rows.csv?accessType=DOWNLOAD'
link_4 = 'https://data.nola.gov/api/views/jsyu-nz5r/rows.csv?accessType=DOWNLOAD'
link_5 = 'https://data.nola.gov/api/views/w68y-xmk6/rows.csv?accessType=DOWNLOAD'

# dowload files into directory
dir.create('challenge') # create new directory called challenge
setwd('challenge')      # move into newly created directory

download.file(link_1, '2011.csv')
download.file(link_2, '2012.csv')
download.file(link_3, '2013.csv')
download.file(link_4, '2014.csv')
download.file(link_5, '2015.csv')

## create aggregate of all files
first = read.csv('2011.csv', stringsAsFactors = FALSE)
second = read.csv('2012.csv', stringsAsFactors = FALSE)
third = read.csv('2013.csv', stringsAsFactors = FALSE)
fourth = read.csv('2014.csv', stringsAsFactors = FALSE)
fifth = read.csv('2015.csv', stringsAsFactors = FALSE)

# cleaning typos ----------------------------------------------------------
# cleaning the least ambiguous typos. saved excel sheet with stuff to remove
# https://github.com/buckeye76guy/MISC/blob/master/typos.csv
typos = read.csv('typos.csv', stringsAsFactors = F, header = F)
to_remove = typos[,1] # the words that should be replaced by another
replace_by = typos[,2] # replacement
for (j in 1:length(to_remove)) {
  ind = which(first$TypeText == to_remove[j])
  if (length(ind) > 0) first$TypeText[ind] = replace_by[j]
  
  ind = which(second$TypeText == to_remove[j])
  if (length(ind) > 0) second$TypeText[ind] = replace_by[j]
  
  ind = which(third$TypeText == to_remove[j])
  if (length(ind) > 0) third$TypeText[ind] = replace_by[j]
  
  ind = which(fourth$TypeText == to_remove[j])
  if (length(ind) > 0) fourth$TypeText[ind] = replace_by[j]
  
  ind = which(fifth$TypeText == to_remove[j])
  if (length(ind) > 0) fifth$TypeText[ind] = replace_by[j]
}
###
aggregate_data = rbind(first, second, third, fourth, fifth)

save.image(file = "question2.RData") # save now and never have to read all files again

# i -----------------------------------------------------------------------

types = aggregate_data['TypeText']      # call types
counts = as.data.frame(table(types))    # obtain a tabular data on each call type
fraction = max(counts$Freq)/nrow(types) # obtain most common call type's fraction
print(format(fraction, digits = 10))    # print result to console


# ii ----------------------------------------------------------------------

dispatch_data = aggregate_data[,8:9]  # obtained the dispatch and arrival times
# convert to datetime
dispatch_data$TimeDispatch = strptime(dispatch_data$TimeDispatch, 
                                      format="%m/%d/%Y %I:%M:%S %p", tz = "UTC")
dispatch_data$TimeArrive = strptime(dispatch_data$TimeArrive,
                                    format="%m/%d/%Y %I:%M:%S %p", tz = "UTC")
indices = is.na(dispatch_data$TimeDispatch) # dispatch time na removed
dispatch_data = dispatch_data[!indices,]
indices = is.na(dispatch_data$TimeArrive) # arrival time na removed
dispatch_data = dispatch_data[!indices,]
# compute time difference between dispatch time and arrival time
dispatch_data$difference = as.numeric(difftime(dispatch_data$TimeArrive,
                                               dispatch_data$TimeDispatch, units = 'sec'))
# remove any negatie time difference
dispatch_data = dispatch_data[dispatch_data$difference >= 0,]
med_response_time = median(dispatch_data$difference)  # median wait time
print(format(med_response_time, digits = 10))         # print result to console


# iii ----------------------------------------------------------------------

# great things dataset has rownames: No need to filter aggregate data again
rnames = rownames(dispatch_data) # match the retained rows with aggregate data
indices = as.numeric(rnames)    # these indices correspond to non-negative wait times
new_aggregate = aggregate_data[indices,c(8,9,15)] # new format of data
any(is.na(new_aggregate$PoliceDistrict)) # no missing district in new_aggregate

library(dplyr) # load dplyr
# group by district
new_aggregate$difference = dispatch_data$difference
new_aggregate_district = group_by(new_aggregate, PoliceDistrict) %>%
  mutate(average = mean(difference)) # average wait time per disctrict
dif = max(new_aggregate_district$average) - min(new_aggregate_district$average)
print(format(dif, digits = 10)) # print result to console


# iv ---------------------------------------------------------------------

# select events that occured more than 100 times overall
event_aggregate = aggregate_data[aggregate_data$PoliceDistrict != 0,] %>% 
  group_by(TypeText) %>% mutate(counts = n()) %>% filter(counts > 100)
events = unique(event_aggregate$TypeText) # these events will be used to match groups below
# unconditional probability: nber of occurences of an event / total number of events
event_aggregate$uncondProb = event_aggregate$counts/nrow(event_aggregate)

# total number of events in each district
aggregate_district = aggregate_data[aggregate_data$PoliceDistrict != 0,] %>% 
  filter(TypeText %in% events) %>% group_by(PoliceDistrict) %>% mutate(counts = n())

# total number of events of specific type per district
event_aggregate_district = aggregate_data[aggregate_data$PoliceDistrict != 0,] %>% 
  filter(TypeText %in% events) %>% group_by(PoliceDistrict, TypeText) %>% mutate(counts = n())
# conditional probability: total nber of specific event in district x over total nber of events
# in district x
event_aggregate_district$condProb = event_aggregate_district$counts/aggregate_district$counts
# ungroup so that district is no longer a factor in the grouping. then group by typetext only
event_aggregate_district = ungroup(event_aggregate_district) %>% group_by(TypeText)
## compute the ratio for each event
# summary is good enough to produce a dataframe with same events but with
# the unconditional and conditional probabilities for grab.
event_summary = as.data.frame(summarise(event_aggregate, max(uncondProb)))
event_summary_1 = as.data.frame(summarise(event_aggregate_district, max(condProb)))
ratio = event_summary_1$`max(condProb)`/event_summary$`max(uncondProb)` # biggest ratio
print(format(max(ratio), digits = 10)) # print to console


# v ---------------------------------------------------------------------
# simply group by types and count by group. Performing a summary with max will
# be a quick way to retrieve count per group since there is only one count per group

# total number of each event in 2011
counts_2011 = group_by(first, TypeText) %>% mutate(counts = n())
agg_counts_2011 = as.data.frame(summarise(counts_2011, max(counts)))

# total number of each event in 2012
counts_2012 = group_by(second, TypeText) %>% mutate(counts = n())
agg_counts_2012 = as.data.frame(summarise(counts_2012, max(counts)))
# some events occured in 2012 but not in 2011

# total number of each event in 2013
counts_2013 = group_by(third, TypeText) %>% mutate(counts = n())
agg_counts_2013 = as.data.frame(summarise(counts_2013, max(counts)))

# total number of each event in 2014
counts_2014 = group_by(fourth, TypeText) %>% mutate(counts = n())
agg_counts_2014 = as.data.frame(summarise(counts_2014, max(counts)))

# total number of each event in 2015
counts_2015 = group_by(fifth, TypeText) %>% mutate(counts = n())
agg_counts_2015 = as.data.frame(summarise(counts_2015, max(counts)))

# to make matters easy, two vectors will be created. one will hold the event names
# the other percentage for each such events. vectors are flexible, we can append as we go
# it seems we should only worry about events that occured in 2011. a third vector will aid
# in maintaining information about the years of the percentage decrease
encountered_event = character(0) # store events with decreased volume
percentage = numeric(0) # store percentage decrease
year_occured = numeric(0) # store year of specific percentage decrease

# percentage change from 2011 to 2012
for (event in agg_counts_2011$TypeText) {     # for each event that occured in 2011
  if (event %in% agg_counts_2012$TypeText){   # if such event occured in 2012
    # obtain count for the event in 2012
    temp_frame1 = filter(agg_counts_2012, TypeText == event)
    # obtain count for the event in 2011
    temp_frame2 = filter(agg_counts_2011, TypeText == event)
    # calculate percentae decrease
    perc_val = (temp_frame1[,2] - temp_frame2[,2])/temp_frame2[,2]
    # we must only worry about negative percentages
    if (perc_val < 0){
      encountered_event = c(encountered_event, event) # append event
      percentage = c(percentage, perc_val)            # append percentage for event
      year_occured = c(year_occured, 2012)            # append year where decrease occured
    }
  }
}

# percentage change from 2012 to 2013
for (event in agg_counts_2012$TypeText){
  if (event %in% agg_counts_2013$TypeText){
    temp_frame1 = filter(agg_counts_2013, TypeText == event)
    temp_frame2 = filter(agg_counts_2012, TypeText == event)
    perc_val = (temp_frame1[,2] - temp_frame2[,2])/temp_frame2[,2]
    # we must only worry about negative percentages
    if (perc_val < 0){
      if (event %in% encountered_event){  # if this event was alread noted.
        ind = which(encountered_event == event)
        # only worry about the case when the current decrease is larger than the previous
        if (abs(perc_val) > percentage[ind]) {
          percentage[ind] = perc_val
          year_occured[ind] = 2013
        } 
      } # else{
      #   encountered_event = c(encountered_event, event)
      #   percentage = c(percentage, perc_val)
      # } # because only events that previously occurred count
    }
  }
}

# percentage change from 2013 to 2014
for (event in agg_counts_2013$TypeText){
  if (event %in% agg_counts_2014$TypeText){
    temp_frame1 = filter(agg_counts_2014, TypeText == event)
    temp_frame2 = filter(agg_counts_2013, TypeText == event)
    perc_val = (temp_frame1[,2] - temp_frame2[,2])/temp_frame2[,2]
    # we must only worry about negative percentages
    if (perc_val < 0){
      if (event %in% encountered_event){
        ind = which(encountered_event == event)
        if (abs(perc_val) > percentage[ind]) {
          percentage[ind] = perc_val
          year_occured[ind] = 2014
          }
      }# else{
      #   encountered_event = c(encountered_event, event)
      #   percentage = c(percentage, perc_val)
      # }
    }
  }
}

# percentage change from 2014 to 2015
for (event in agg_counts_2014$TypeText){
  if (event %in% agg_counts_2015$TypeText){
    temp_frame1 = filter(agg_counts_2015, TypeText == event)
    temp_frame2 = filter(agg_counts_2014, TypeText == event)
    perc_val = (temp_frame1[,2] - temp_frame2[,2])/temp_frame2[,2]
    # we must only worry about negative percentages
    if (perc_val < 0){
      if (event %in% encountered_event){
        ind = which(encountered_event == event)
        if (abs(perc_val) > percentage[ind]) {percentage[ind] = perc_val
        year_occured[ind] = 2015
        }
      } #else{
      #   encountered_event = c(encountered_event, event)
      #   percentage = c(percentage, perc_val)
      # }
    }
  }
}

# now that we have percentage and events, we can get the event with max decrease
guilty_event = encountered_event[which.max(abs(percentage))] # the event with most decrease
print(year_occured[which(encountered_event == guilty_event)]) # year it occured : 2014
event_2011 = filter(agg_counts_2011, TypeText == guilty_event)[,2] # how many call types in 2011
event_2014 = filter(agg_counts_2014, TypeText == guilty_event)[,2] # in 2014
print(format(event_2014/event_2011))


# vi ----------------------------------------------------------------------

# Only consider the hour in TimeCreate
temp_vector = aggregate_data$TimeCreate   # obtain TimeCreate vector
temp_list = strsplit(temp_vector, ' ')    # split in ordr to eliminate the day
for (j in 1:length(temp_vector)) {
  # this will eliminate the dates. Since we only care about the hours
  # we can round each time we obtain and pretend they are for the same day.
  temp_vector[j] = paste(temp_list[[j]][2], temp_list[[j]][3])
}
# convert to datetime object and round to hour
temp_vector = strptime(temp_vector, format="%I:%M:%S %p", tz = "UTC")
temp_vector = round(temp_vector, units = "hour")
disposition_data = aggregate_data[,c('DispositionText', 'TimeCreate')]
disposition_data$TimeCreate = as.character(temp_vector)
# group data by DispositionText to get total number of occurences of such disposition
disposition_data_group1 = group_by(disposition_data, DispositionText) %>% mutate(counts = n())
# get the total number of occurences of each disposition
group1_summary = summarise(disposition_data_group1, max(counts))

# group data by DispositionText and hour
disposition_data_group2 = group_by(disposition_data, DispositionText, TimeCreate) %>%
  mutate(counts = n())
group2_summary = summarise(disposition_data_group2, max(counts))

# create a data frame to hold the 
disposition_data_sd = data.frame(DispositionText = group2_summary$DispositionText,
                                 fraction = NA)
for (j in 1:nrow(group2_summary)) {
  disp = group2_summary$DispositionText[j] # disposition taken
  # obtain the total count for that disposition type
  tot_disposition = as.numeric(filter(group1_summary, DispositionText == disp)[,2])
  # compute fraction hourly / overall
  disposition_data_sd$fraction[j] = group2_summary$`max(counts)`[j]/tot_disposition
}
# first used sd(fraction) but settled for max - min
disposition_data_sd = group_by(disposition_data_sd, DispositionText) %>%
  mutate(std =  max(fraction) - min(fraction)) # compute standard deviation of fraction
# get the disposition with highest variance in a day
ind = which.max(disposition_data_sd$std)
print(format(disposition_data_sd$std[ind], digits = 10)) # print result to console


# vii ---------------------------------------------------------------------

location_data = aggregate_data[,c(15, 16)]
temp_vector1 = numeric(nrow(location_data))   # longitude vector
temp_vector2 = numeric(length(temp_vector1))  # latitude vector
temp_list = strsplit(location_data$Location, ',')        # break location into components
# this function separates longitude and latitude
quick_fun <- function(string) return(as.numeric(gsub('\\(|\\)| ', '', string)))
temp = lapply(temp_list, quick_fun)
# using functions because lapply and sapply can be more efficient than loops at times
quick_fun1 <- function(x) return(x[1])  # gets first element of a vector
quick_fun2 <- function(x) return(x[2])  # second element
temp_vector1 = sapply(temp, quick_fun1) # longitude
temp_vector2 = sapply(temp, quick_fun2) # latitude
location_data$longitude = temp_vector1  # create longitude column
location_data$latitude = temp_vector2   # create latitude column
location_data = location_data[complete.cases(location_data),]
# group by district and get mean and sd of longitude and latitude
location_data_grouped = group_by(location_data, PoliceDistrict) %>% 
  mutate(longavg = mean(longitude), longsd = sd(longitude), 
         latavg = mean(latitude), latsd = sd(latitude))
# stuck


# viii --------------------------------------------------------------------
# obtain priority and call type. group by TypeText and Priority
priority_data = aggregate_data[,3:4] %>% group_by(TypeText, Priority) %>% 
  mutate(counts = n())
# summarize in order to find common priority for each type
priority_data_summary = ungroup(priority_data) %>% group_by(TypeText) %>%
  summarise(max(counts))
# this vector will contain the fractions
fractions = numeric(nrow(priority_data_summary))
for (j in 1:length(fractions)) {
  event = priority_data_summary$TypeText[j]   # call type
  total_call = nrow(filter(priority_data, TypeText == event)) # total nber of such calls
  fractions[j] = priority_data_summary$`max(counts)`[j]/total_call # fraction
}
print(format(min(fractions), digits=10)) # print to console
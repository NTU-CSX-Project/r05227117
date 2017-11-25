# install.packages("dplyr")
library(dplyr)
# read csv and header
cpu <- read.csv(file="CPU.csv", header = TRUE, stringsAsFactors=FALSE)
memory <- read.csv(file="Memory.csv", header = TRUE, stringsAsFactors=FALSE)
# str(cpu)
# str(memory)

colnames(cpu) <- c("machine_id", "data", "ts", "cpu_ghz", "cpu_usage", "cpu_idle", "cpu_ip", "cpu_total")
colnames(memory) <- c("machine_id", "data", "ts", "memory_free_mb", "memory_ip", "memory_usage", "memory_total")
# remove repeated column
cpu = cpu[-2]
memory = memory[-2]
cpu$machine_id <- substr(cpu$machine_id, start = 1, stop = 1)
memory$machine_id <- substr(memory$machine_id, start = 1, stop = 1)
# 
# table(cpu$machine_id)
# table(memory$machine_id)
# 

cpu$ts <- as.POSIXct(cpu$ts)
memory$ts <- as.POSIXct(memory$ts)

data_select_start <- "2017-08-04 07:09:00"
data_select_end <-"2017-08-08 02:48:00"

# "2017-08-04 07:09:00"
# "2017-08-08 02:48:00"

##############################################################
# check min time in each machine
#
# cpu %>% group_by(machine_id) %>% slice(which.min(ts))
# memory %>% group_by(machine_id) %>% slice(which.min(ts))
# ################################################################################################
# check max time in each machine
#
# cpu %>% group_by(machine_id) %>% slice(which.max(ts))
# memory %>% group_by(machine_id) %>% slice(which.max(ts))
##################################################################################################
#choose same start and end time from each dataframe
cpu_same_t <- cpu[with(cpu, cpu$ts >= data_select_start & cpu$ts <=data_select_end), ]
memory_same_t <- memory[with(memory, memory$ts >= data_select_start & memory$ts <=data_select_end), ]

# cpu_same_t %>% group_by(machine_id) %>% slice(which.min(ts))
# memory_same_t %>% group_by(machine_id) %>% slice(which.min(ts))
# 
# cpu_same_t %>% group_by(machine_id) %>% slice(which.max(ts))
# memory_same_t %>% group_by(machine_id) %>% slice(which.max(ts))
# table(cpu_same_t$machine_id)
# table(memory_same_t$machine_id)
# 
########################################################################################
#Group data by machine_id
# cpu_m_a = cpu_same_t[with(cpu_same_t, cpu_same_t$machine_id == "a"), ]
# cpu_m_b = cpu_same_t[with(cpu_same_t, cpu_same_t$machine_id == "b"), ]
# cpu_m_c = cpu_same_t[with(cpu_same_t, cpu_same_t$machine_id == "c"), ]
# cpu_m_i = cpu_same_t[with(cpu_same_t, cpu_same_t$machine_id == "i"), ]
# cpu_m_t = cpu_same_t[with(cpu_same_t, cpu_same_t$machine_id == "t"), ]
# cpu_m_z = cpu_same_t[with(cpu_same_t, cpu_same_t$machine_id == "z"), ]
# memory_m_a = memory_same_t[with(memory_same_t, memory_same_t$machine_id == "a"), ]
# memory_m_b = memory_same_t[with(memory_same_t, memory_same_t$machine_id == "b"), ]
# memory_m_c = memory_same_t[with(memory_same_t, memory_same_t$machine_id == "c"), ]
# memory_m_i = memory_same_t[with(memory_same_t, memory_same_t$machine_id == "i"), ]
# memory_m_t = memory_same_t[with(memory_same_t, memory_same_t$machine_id == "t"), ]
# memory_m_z = memory_same_t[with(memory_same_t, memory_same_t$machine_id == "z"), ]
########################################################################################
# check if there is null value in each datarame
#
# which(is.na(match(cpu_m_a$ts, memory_m_a$ts)))
# which(is.na(match(memory_m_a$ts, cpu_m_a$ts)))
# which(is.na(match(cpu_m_b$ts, memory_m_b$ts)))
# which(is.na(match(memory_m_b$ts, cpu_m_b$ts)))
# which(is.na(match(cpu_m_c$ts, memory_m_c$ts)))
# which(is.na(match(memory_m_c$ts, cpu_m_c$ts)))
# which(is.na(match(cpu_m_i$ts, memory_m_i$ts)))
# which(is.na(match(memory_m_i$ts, cpu_m_i$ts)))
# which(is.na(match(cpu_m_t$ts, memory_m_t$ts)))
# which(is.na(match(memory_m_t$ts, cpu_m_t$ts)))
# which(is.na(match(cpu_m_z$ts, memory_m_z$ts)))
# which(is.na(match(memory_m_z$ts, cpu_m_z$ts)))

#####################################################################################################
# 2 sec to 1 min
onemin_breaks = seq(as.POSIXlt(data_select_start), 
                    as.POSIXlt(data_select_end), by="30 s")
cpu_onemin = cpu_same_t %>% group_by(machine_id, ts = cut(ts, breaks = onemin_breaks)) %>%
  summarise_at(c("cpu_ghz", "cpu_usage", "cpu_idle", "cpu_total"), mean) 
memory_onemin = memory_same_t %>% group_by(machine_id, ts = cut(ts, breaks = onemin_breaks)) %>%
  summarise_at(c("memory_free_mb", "memory_usage", "memory_total"), mean) 

cm <- merge(cpu_onemin,memory_onemin,all.x=T)

cm_a <- cm[with(cm, cm$machine_id == "a"), ]
cm_b <- cm[with(cm, cm$machine_id == "b"), ]
cm_c <- cm[with(cm, cm$machine_id == "c"), ]
cm_i <- cm[with(cm, cm$machine_id == "i"), ]
cm_t <- cm[with(cm, cm$machine_id == "t"), ]
cm_z <- cm[with(cm, cm$machine_id == "z"), ]

#########alter based on row number of each machine data on different time scale.
cm_a <- cm_a[-10999,]
cm_i <- cm_i[-10999,]

cm_all <- rbind(cm_a, cm_b, cm_c, cm_i, cm_t, cm_z)

cm_all <- cm_all %>% group_by(cm_all$ts) %>% summarise_each(funs(mean),
  cpu_ghz, cpu_usage, cpu_idle, cpu_total,memory_free_mb, memory_usage, memory_total
  )
cm_all_s  <- cm_all %>% summarise_each(funs(mean,sd),
    cpu_ghz, cpu_usage, cpu_idle, cpu_total,memory_free_mb, memory_usage, memory_total
)

colnames(cm_all)[1] <- 'ts' 
cm_all$ts <- as.POSIXct(cm_all$ts)

########hour to morning,noon~
heure <- as.integer(substr(cm_all$ts, 12, 13))
time_block <- data.frame(period=cut(heure, c(-Inf, 7, 10, 12, 17, Inf),
                                    labels=c("night", "morning", "noon", "afternoon", "evening")))
cm_all <- cbind(cm_all,time_block)


#weekday
library(lubridate)
cm_all$weekday <- wday(cm_all$ts)

#####################################################
#cpu_usage_tag
cm_all <- cm_all %>% mutate(cpu_usage_tag = ifelse(cpu_usage > cm_all_s$cpu_usage_mean + 4* cm_all_s$cpu_usage_sd,4,(
                                  ifelse(cpu_usage > cm_all_s$cpu_usage_mean + 3* cm_all_s$cpu_usage_sd,3,(
                                  ifelse(cpu_usage > cm_all_s$cpu_usage_mean + 2* cm_all_s$cpu_usage_sd,2,(
                                  ifelse(cpu_usage > cm_all_s$cpu_usage_mean + 1* cm_all_s$cpu_usage_sd,1,(
                                  ifelse(cpu_usage > cm_all_s$cpu_usage_mean + 0* cm_all_s$cpu_usage_sd,0,(
                                    ifelse(cpu_usage > cm_all_s$cpu_usage_mean - 1* cm_all_s$cpu_usage_sd,-1,(
                                      ifelse(cpu_usage > cm_all_s$cpu_usage_mean - 2* cm_all_s$cpu_usage_sd,-2,-3)
                                    )
                                  ))
                                  ))))))))))
#memory_usage_tag
cm_all <- cm_all %>% mutate(memory_usage_tag = ifelse(memory_usage > cm_all_s$memory_usage_mean + 4* cm_all_s$memory_usage_sd,4,(
  ifelse(memory_usage > cm_all_s$memory_usage_mean + 3* cm_all_s$memory_usage_sd,3,(
    ifelse(memory_usage > cm_all_s$memory_usage_mean + 2* cm_all_s$memory_usage_sd,2,(
      ifelse(memory_usage > cm_all_s$memory_usage_mean + 1* cm_all_s$memory_usage_sd,1,(
        ifelse(memory_usage > cm_all_s$memory_usage_mean + 0* cm_all_s$memory_usage_sd,0,(
          ifelse(memory_usage > cm_all_s$memory_usage_mean - 1* cm_all_s$memory_usage_sd,-1,(
            ifelse(memory_usage > cm_all_s$memory_usage_mean - 2* cm_all_s$memory_usage_sd,-2,-3)
          )
          ))
        ))))))))))



  
# cm_all <- cm_all %>% mutate(cpu_usage_tag = ifelse(cpu_usage > cm_all_s$cpu_usage_mean + 4* cm_all_s$cpu_usage_sd,4,(
#   ifelse(cpu_usage > cm_all_s$cpu_usage_mean + 3* cm_all_s$cpu_usage_sd,3,(
#     ifelse(cpu_usage > cm_all_s$cpu_usage_mean + 2* cm_all_s$cpu_usage_sd,2,(
#       ifelse(cpu_usage > cm_all_s$cpu_usage_mean + 1* cm_all_s$cpu_usage_sd,1,0))))))))


require(ggplot2)
plot_c_all <- ggplot()+ 
  geom_line(aes(x = cm_all$ts, y = cm_all$cpu_usage_tag), data = cm_all)
plot_m_all <- ggplot()+ 
  geom_line(aes(x = cm_all$ts, y = cm_all$memory_usage_tag), data = cm_all)

# summary(cpu_onemin)
# summary(cpu)
# which(is.na(cpu_onemin$ts))


# summary(memory_onemin)
# summary(memory)


#######################################################################################################
#save data
write.csv(cm_all,"cm_all.csv")



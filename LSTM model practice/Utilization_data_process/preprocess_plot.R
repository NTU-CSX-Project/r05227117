
# EDA plot
data_select_start <- "2017-08-04 7:09:00"
data_select_end <-"2017-08-08 02:48:00"
onemin_breaks <- seq(as.POSIXlt(data_select_start), 
                    as.POSIXlt(data_select_end), by="60 min")

# "2017-08-04 07:09:00"
# "2017-08-08 02:48:00"
cpu_QQ <- cpu[with(cpu, cpu$ts >= data_select_start & cpu$ts <= data_select_end), ]
memory_QQ <- memory[with(memory, memory$ts >= data_select_start & memory$ts <= data_select_end), ]

cpu_QQ <- cpu_QQ %>% group_by(machine_id, ts = cut(ts, breaks = onemin_breaks)) %>%
  summarise_at(c("cpu_ghz", "cpu_usage", "cpu_idle", "cpu_total"), mean) 
memory_QQ <- memory_QQ %>% group_by(machine_id, ts = cut(ts, breaks = onemin_breaks)) %>%
  summarise_at(c("memory_free_mb", "memory_usage", "memory_total"), mean)

cpu_QQ$ts <- as.POSIXct(cpu_QQ$ts)
memory_QQ$ts <- as.POSIXct(memory_QQ$ts)

memory_used_mb <- memory_QQ %>% group_by(machine_id, ts = cut(ts, breaks = onemin_breaks)) %>%
  summarise("memory_used_mb"=memory_total-memory_free_mb)

memory_used_mb$ts <- as.POSIXct(memory_used_mb$ts)
memory_QQ <- cbind(memory_QQ,memory_used_mb)

cpu_aa <- cpu_QQ[with(cpu_QQ, cpu_QQ$machine_id == "a" & cpu_QQ$ts >= data_select_start & cpu_QQ$ts <= data_select_end), ]
cpu_bb <- cpu_QQ[with(cpu_QQ, cpu_QQ$machine_id == "b" & cpu_QQ$ts >= data_select_start & cpu_QQ$ts <= data_select_end), ]
cpu_cc <- cpu_QQ[with(cpu_QQ, cpu_QQ$machine_id == "c" & cpu_QQ$ts >= data_select_start & cpu_QQ$ts <= data_select_end), ]
cpu_ii <- cpu_QQ[with(cpu_QQ, cpu_QQ$machine_id == "i" & cpu_QQ$ts >= data_select_start & cpu_QQ$ts <= data_select_end), ]
cpu_tt <- cpu_QQ[with(cpu_QQ, cpu_QQ$machine_id == "t" & cpu_QQ$ts >= data_select_start & cpu_QQ$ts <= data_select_end), ]
cpu_zz <- cpu_QQ[with(cpu_QQ, cpu_QQ$machine_id == "z" & cpu_QQ$ts >= data_select_start & cpu_QQ$ts <= data_select_end), ]

memory_aa <- memory_QQ[with(memory_QQ, memory_QQ$machine_id == "a" & memory_QQ$ts >= data_select_start & memory_QQ$ts <= data_select_end), ]
memory_bb <- memory_QQ[with(memory_QQ, memory_QQ$machine_id == "b" & memory_QQ$ts >= data_select_start & memory_QQ$ts <= data_select_end), ]
memory_cc <- memory_QQ[with(memory_QQ, memory_QQ$machine_id == "c" & memory_QQ$ts >= data_select_start & memory_QQ$ts <= data_select_end), ]
memory_ii <- memory_QQ[with(memory_QQ, memory_QQ$machine_id == "i" & memory_QQ$ts >= data_select_start & memory_QQ$ts <= data_select_end), ]
memory_tt <- memory_QQ[with(memory_QQ, memory_QQ$machine_id == "t" & memory_QQ$ts >= data_select_start & memory_QQ$ts <= data_select_end), ]
memory_zz <- memory_QQ[with(memory_QQ, memory_QQ$machine_id == "z" & memory_QQ$ts >= data_select_start & memory_QQ$ts <= data_select_end), ]

# cor(cpu_aa$cpu_usage,memory_aa$memory_usage)
# cor(cpu_bb$cpu_usage,memory_bb$memory_usage)
# cor(cpu_cc$cpu_usage,memory_cc$memory_usage)
# cor(cpu_ii$cpu_usage,memory_ii$memory_usage)
# cor(cpu_tt$cpu_usage,memory_tt$memory_usage)
# cor(cpu_zz$cpu_usage,memory_zz$memory_usage)

require(ggplot2)


plot_cpu_QQ <- ggplot(aes(x = cpu_QQ$ts, y = cpu_QQ$cpu_usage), data = cpu_QQ) + geom_line(aes(color= factor(machine_id)))

plot_cpu_aa <- ggplot() + geom_line(aes(x = cpu_aa$ts, y = cpu_aa$cpu_usage), data = cpu_aa)
plot_cpu_bb <- ggplot() + geom_line(aes(x = cpu_bb$ts, y = cpu_bb$cpu_usage), data = cpu_bb)
plot_cpu_cc <- ggplot() + geom_line(aes(x = cpu_cc$ts, y = cpu_cc$cpu_usage), data = cpu_cc)
plot_cpu_ii <- ggplot() + geom_line(aes(x = cpu_ii$ts, y = cpu_ii$cpu_usage), data = cpu_ii)
plot_cpu_tt <- ggplot() + geom_line(aes(x = cpu_tt$ts, y = cpu_tt$cpu_usage), data = cpu_tt)
plot_cpu_zz <- ggplot() + geom_line(aes(x = cpu_zz$ts, y = cpu_zz$cpu_usage), data = cpu_zz)


plot_memory_QQ <- ggplot(aes(x = memory_QQ$ts, y = memory_QQ$memory_usage), data = memory_QQ) + geom_line(aes(colour = factor(machine_id)))

plot_memory_aa <- ggplot(aes(x = memory_aa$ts, y = memory_aa$memory_usage), data = memory_aa) + geom_line()
plot_memory_bb <- ggplot(aes(x = memory_bb$ts, y = memory_bb$memory_usage), data = memory_bb) + geom_line()
plot_memory_cc <- ggplot(aes(x = memory_cc$ts, y = memory_cc$memory_usage), data = memory_cc) + geom_line()
plot_memory_ii <- ggplot(aes(x = memory_ii$ts, y = memory_ii$memory_usage), data = memory_ii) + geom_line()
plot_memory_tt <- ggplot(aes(x = memory_tt$ts, y = memory_tt$memory_usage), data = memory_tt) + geom_line()
plot_memory_zz <- ggplot(aes(x = memory_zz$ts, y = memory_zz$memory_usage), data = memory_zz) + geom_line()

#cm usage
plot_cm_aa <- ggplot() + geom_line(aes(x = cpu_aa$ts, y = cpu_aa$cpu_usage), data = cpu_aa,color="red")+
  geom_line(aes(x = memory_aa$ts, y = memory_aa$memory_usage), data = memory_aa,size=2)

plot_cm_bb <- ggplot() + geom_line(aes(x = cpu_bb$ts, y = cpu_bb$cpu_usage), data = cpu_bb,color="red")+
  geom_line(aes(x = memory_bb$ts, y = memory_bb$memory_usage), data = memory_bb,size=2)

plot_cm_cc <- ggplot() + geom_line(aes(x = cpu_cc$ts, y = cpu_cc$cpu_usage), data = cpu_cc,color="red")+
  geom_line(aes(x = memory_cc$ts, y = memory_cc$memory_usage), data = memory_cc,size=2)

plot_cm_ii <- ggplot() + geom_line(aes(x = cpu_ii$ts, y = cpu_ii$cpu_usage), data = cpu_ii,color="red")+
  geom_line(aes(x = memory_ii$ts, y = memory_ii$memory_usage), data = memory_ii,size=2)

plot_cm_tt <- ggplot() + geom_line(aes(x = cpu_tt$ts, y = cpu_tt$cpu_usage), data = cpu_tt,color="red")+
  geom_line(aes(x = memory_tt$ts, y = memory_tt$memory_usage), data = memory_tt,size=2)

plot_cm_zz <- ggplot() + geom_line(aes(x = cpu_zz$ts, y = cpu_zz$cpu_usage), data = cpu_zz,color="red")+ 
  geom_line(aes(x = memory_zz$ts, y = memory_zz$memory_usage), data = memory_zz,size=2)

#cm used
plot_c_mused_aa <- ggplot() + geom_line(aes(x = memory_aa$ts, y = memory_aa$memory_used_mb), data = memory_aa)+ geom_line(aes(x = cpu_aa$ts, y = cpu_aa$cpu_usage), data = cpu_aa,color="red")
plot_c_mused_bb <- ggplot() + geom_line(aes(x = memory_bb$ts, y = memory_bb$memory_used_mb), data = memory_bb)+ geom_line(aes(x = cpu_bb$ts, y = cpu_bb$cpu_usage), data = cpu_bb,color="red")
plot_c_mused_cc <- ggplot() + geom_line(aes(x = memory_cc$ts, y = memory_cc$memory_used_mb), data = memory_cc)+ geom_line(aes(x = cpu_cc$ts, y = cpu_cc$cpu_usage), data = cpu_cc,color="red")
plot_c_mused_ii <- ggplot() + geom_line(aes(x = memory_ii$ts, y = memory_ii$memory_used_mb), data = memory_ii)+ geom_line(aes(x = cpu_ii$ts, y = cpu_ii$cpu_usage), data = cpu_ii,color="red")
plot_c_mused_tt <- ggplot() + geom_line(aes(x = memory_tt$ts, y = memory_tt$memory_used_mb), data = memory_tt)+ geom_line(aes(x = cpu_tt$ts, y = cpu_tt$cpu_usage), data = cpu_tt,color="red")
plot_c_mused_zz <- ggplot() + geom_line(aes(x = memory_zz$ts, y = memory_zz$memory_used_mb), data = memory_zz)+ geom_line(aes(x = cpu_zz$ts, y = cpu_zz$cpu_usage), data = cpu_zz,color="red")

plot_memory_QQ1 <- ggplot(aes(x = memory_QQ$ts, y = memory_QQ$memory_usage), data = memory_QQ)  + geom_line(aes(colour = factor(machine_id)))
plot_memory_QQ <- ggplot(aes(x = memory_QQ$ts, y = memory_QQ$memory_used_mb), data = memory_QQ)  + geom_line(aes(colour = factor(machine_id)))


cpu_QQ <- cpu[with(cpu, cpu$ts >= data_select_start & cpu$ts <= data_select_end), ]
memory_QQ <- memory[with(memory, memory$ts >= data_select_start & memory$ts <= data_select_end), ]

cpu_total <- cpu_QQ %>% group_by(ts = cut(ts, breaks = onemin_breaks)) %>%
  summarise_at(c("cpu_ghz", "cpu_usage", "cpu_idle", "cpu_total"), mean) 
memory_total <- memory_QQ %>% group_by(ts = cut(ts, breaks = onemin_breaks)) %>%
  summarise_at(c("memory_free_mb", "memory_usage", "memory_total"), mean)

cpu_total$ts <- as.POSIXct(cpu_total$ts)
memory_total$ts <- as.POSIXct(memory_total$ts)


plot_memory_total = ggplot()+ geom_line(aes(x = memory_total$ts, y = memory_total$memory_usage), data = memory_total)

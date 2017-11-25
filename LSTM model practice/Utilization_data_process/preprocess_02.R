onemin_breaks = seq(as.POSIXlt(data_select_start), 
                    as.POSIXlt(data_select_end), by="1 min")

cpu_machine_summary = cpu_same_t %>% group_by(machine_id) %>%
  summarise(n_cpu_usage = n(), mean_cpu_usage = mean(cpu_usage), sd_cpu_usage = sd(cpu_usage), 
            cpu_peak_thresh = mean_cpu_usage + sd_cpu_usage*3) 

cpu_peak_freq = cpu_same_t %>% group_by(machine_id, ts = cut(ts, breaks = onemin_breaks)) %>%
  summarise(n_cpu_usage = n(), n_cpu_usage_peak = sum(cpu_usage >= cpu_machine_summary$cpu_peak_thresh)) 




onemin_breaks = seq(as.POSIXlt(data_start), 
                    as.POSIXlt(data_end), by="1 min")

cpu_machine_summary = cpu_same_t %>% group_by(machine_id) %>%
  summarise(n_cpu_usage = n(), mean_cpu_usage = mean(cpu_usage), sd_cpu_usage = sd(cpu_usage), 
            cpu_peak_thresh = mean_cpu_usage + sd_cpu_usage*3) 
cpu_a_peak_freq = cpu_same_t %>% filter(machine_id == "a") %>% group_by(ts = cut(ts, breaks = onemin_breaks)) %>%
  summarise(maching_id = "a", n_cpu_usage = n(), n_cpu_usage_peak = sum(cpu_usage >= cpu_machine_summary$cpu_peak_thresh[1])) 
cpu_b_peak_freq = cpu_same_t %>% filter(machine_id == "b") %>% group_by(ts = cut(ts, breaks = onemin_breaks)) %>%
  summarise(maching_id = "b", n_cpu_usage = n(), n_cpu_usage_peak = sum(cpu_usage >= cpu_machine_summary$cpu_peak_thresh[2])) 
cpu_c_peak_freq = cpu_same_t %>% filter(machine_id == "a") %>% group_by(ts = cut(ts, breaks = onemin_breaks)) %>%
  summarise(maching_id = "c", n_cpu_usage = n(), n_cpu_usage_peak = sum(cpu_usage >= cpu_machine_summary$cpu_peak_thresh[3])) 
cpu_i_peak_freq = cpu_same_t %>% filter(machine_id == "i") %>% group_by(ts = cut(ts, breaks = onemin_breaks)) %>%
  summarise(maching_id = "i", n_cpu_usage = n(), n_cpu_usage_peak = sum(cpu_usage >= cpu_machine_summary$cpu_peak_thresh[4])) 
cpu_t_peak_freq = cpu_same_t %>% filter(machine_id == "t") %>% group_by(ts = cut(ts, breaks = onemin_breaks)) %>%
  summarise(maching_id = "t", n_cpu_usage = n(), n_cpu_usage_peak = sum(cpu_usage >= cpu_machine_summary$cpu_peak_thresh[5])) 
cpu_z_peak_freq = cpu_same_t %>% filter(machine_id == "z") %>% group_by(ts = cut(ts, breaks = onemin_breaks)) %>%
  summarise(maching_id = "z", n_cpu_usage = n(), n_cpu_usage_peak = sum(cpu_usage >= cpu_machine_summary$cpu_peak_thresh[6])) 
cpu_peak_freq = rbind(cpu_a_peak_freq, cpu_b_peak_freq, cpu_c_peak_freq, cpu_i_peak_freq, cpu_t_peak_freq, cpu_z_peak_freq)



cpu_machine_summary = cpu_same_t %>% group_by(machine_id) %>%
  summarise(n_cpu_usage = n(), mean_cpu_usage = mean(cpu_usage), sd_cpu_usage = sd(cpu_usage), 
            cpu_peak_thresh = mean_cpu_usage + sd_cpu_usage*3) 
cpu_a_peak_freq = cpu_same_t %>% filter(machine_id == "a") %>% group_by(ts = cut(ts, breaks = onemin_breaks)) %>%
  summarise(maching_id = "a", n_cpu_usage = n(), n_cpu_usage_peak = sum(cpu_usage >= cpu_machine_summary$cpu_peak_thresh[1])) 
cpu_b_peak_freq = cpu_same_t %>% filter(machine_id == "b") %>% group_by(ts = cut(ts, breaks = onemin_breaks)) %>%
  summarise(maching_id = "b", n_cpu_usage = n(), n_cpu_usage_peak = sum(cpu_usage >= cpu_machine_summary$cpu_peak_thresh[2])) 
cpu_c_peak_freq = cpu_same_t %>% filter(machine_id == "a") %>% group_by(ts = cut(ts, breaks = onemin_breaks)) %>%
  summarise(maching_id = "c", n_cpu_usage = n(), n_cpu_usage_peak = sum(cpu_usage >= cpu_machine_summary$cpu_peak_thresh[3])) 
cpu_i_peak_freq = cpu_same_t %>% filter(machine_id == "i") %>% group_by(ts = cut(ts, breaks = onemin_breaks)) %>%
  summarise(maching_id = "i", n_cpu_usage = n(), n_cpu_usage_peak = sum(cpu_usage >= cpu_machine_summary$cpu_peak_thresh[4])) 
cpu_t_peak_freq = cpu_same_t %>% filter(machine_id == "t") %>% group_by(ts = cut(ts, breaks = onemin_breaks)) %>%
  summarise(maching_id = "t", n_cpu_usage = n(), n_cpu_usage_peak = sum(cpu_usage >= cpu_machine_summary$cpu_peak_thresh[5])) 
cpu_z_peak_freq = cpu_same_t %>% filter(machine_id == "z") %>% group_by(ts = cut(ts, breaks = onemin_breaks)) %>%
  summarise(maching_id = "z", n_cpu_usage = n(), n_cpu_usage_peak = sum(cpu_usage >= cpu_machine_summary$cpu_peak_thresh[6])) 
cpu_peak_freq = rbind(cpu_a_peak_freq, cpu_b_peak_freq, cpu_c_peak_freq, cpu_i_peak_freq, cpu_t_peak_freq, cpu_z_peak_freq)
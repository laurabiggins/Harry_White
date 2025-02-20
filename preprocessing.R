library(tidyverse)

ds1 <- readr::read_tsv("ydall.txt")
ds2 <- readr::read_tsv("umtboth.txt")
 

aa_lengths1 <- ds1 %>%
  #select(V_CALL, CDR3_IGBLAST_AA) %>%
  select(CDR3_IGBLAST_AA) %>%
  rename(AA = CDR3_IGBLAST_AA) %>%
  mutate(n_aa = nchar(AA)) %>%
  count(n_aa)

aa_lengths2 <- ds2 %>%
  #select(V_CALL, CDR3_IGBLAST_AA) %>%
  select(CDR3_IGBLAST_AA) %>%
  rename(AA = CDR3_IGBLAST_AA) %>%
  mutate(n_aa = nchar(AA)) %>%
  count(n_aa)

# aa_counts <- summary %>%
#   group_by(V_CALL, pos) %>%
#   count(value) %>%
#   ungroup() 

summary1 <- ds1 %>%
  select(V_CALL, CDR3_IGBLAST_AA) %>%
  #select(CDR3_IGBLAST_AA) %>%
  rename(AA = CDR3_IGBLAST_AA) %>%
  mutate(n_aa = nchar(AA)) %>%
  relocate(n_aa, .before=AA) %>%
  filter(nchar(AA) < 17 & nchar(AA) >=7) %>%
  separate(AA, sep = 1:15, into = as.character(1:16), fill = "right", remove = FALSE) %>%
  pivot_longer(-(V_CALL:AA), names_to = "pos") %>%
  filter(value != "") %>%
  mutate(pos = forcats::as_factor(pos)) 

summary2 <- ds2 %>%
  select(V_CALL, CDR3_IGBLAST_AA) %>%
  #select(CDR3_IGBLAST_AA) %>%
  rename(AA = CDR3_IGBLAST_AA) %>%
  mutate(n_aa = nchar(AA)) %>%
  relocate(n_aa, .before=AA) %>%
  filter(nchar(AA) < 17 & nchar(AA) >=7) %>%
  separate(AA, sep = 1:15, into = as.character(1:16), fill = "right", remove = FALSE) %>%
  pivot_longer(-(V_CALL:AA), names_to = "pos") %>%
  filter(value != "") %>%
  mutate(pos = forcats::as_factor(pos)) 


pos_counts1 <- summary1 %>%
  add_count(V_CALL, pos, value, name = "aa_count1") %>%
  add_count(V_CALL, pos, name = "total") %>%
  mutate(aa_percent1 = (aa_count1/total)*100) %>%
  select(-AA, -total, -n_aa) %>%
  distinct() # if we don't do this we get loads of repetition due to the n_aa lengths

pos_counts2 <- summary2 %>%
  add_count(V_CALL, pos, value, name = "aa_count2") %>%
  add_count(V_CALL, pos, name = "total") %>%
  mutate(aa_percent2 = (aa_count2/total)*100) %>%
  select(-AA, -total, -n_aa) %>%
  distinct()


# there are lots of different lengths, so we need to remove those from the join or the table size gets ridiculous. If we're going to filter on length, it'll have to be upstream.
# I've removed n_aa from pos_counts
joined <- pos_counts1 %>%
  full_join(pos_counts2, by = c("V_CALL", "pos", "value")) %>%
  #replace(is.na(.), 0)
  mutate(aa_count1 = replace_na(aa_count1, 0)) %>%
  mutate(aa_percent1 = replace_na(aa_percent1, 0)) %>%
  mutate(aa_count2 = replace_na(aa_count2, 0)) %>%
  mutate(aa_percent2 = replace_na(aa_percent2, 0)) %>%
  mutate(percent_diff = aa_percent1-aa_percent2) %>%
  mutate(raw_diff = aa_count1-aa_count2)


saveRDS(aa_lengths1, "data/aa_lengths1.rds")
saveRDS(aa_lengths2, "data/aa_lengths2.rds")
saveRDS(summary1, "data/summary1.rds")
saveRDS(summary2, "data/summary2.rds")
saveRDS(joined, file="data/joined.rds")



library(plotly)
joined %>%
  dplyr::filter(V_CALL == "IGHV10-1") %>%
  dplyr::filter(abs(percent_diff) > 2) %>%
  plotly::plot_ly(x= ~pos, y= ~percent_diff, color= ~value) %>%
  plotly::add_text(
    text = ~value,
    #hovertext = ~name,
    #hoverinfo = "text",
    size = I(20)
  )

joined %>%
  dplyr::filter(V_CALL == "IGHV10-1") %>%
  group_by(value) %>%
  summarise(
    median_diff = median(abs(percent_diff)),
    mean_diff = mean(abs(percent_diff)),
    sd_diff = sd(percent_diff)
  ) %>%
  #arrange(desc(mean_diff))
  arrange(desc(sd_diff))

#my_colours <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_distinct(joined$value))

joined %>%
  dplyr::filter(V_CALL == "IGHV10-1") %>%
  dplyr::filter(abs(percent_diff) > 1) %>%
  plotly::plot_ly(x = ~pos, y= ~value, z= ~ percent_diff) %>%
  plotly::add_heatmap() #+
  #scale_color_manual(values = my_colours)




# aa_counts1 <- summary1 %>%
#   group_by(V_CALL, pos) %>%
#   count(value) %>%
#   ungroup() 
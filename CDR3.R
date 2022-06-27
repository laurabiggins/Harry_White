#ds1 <- readr::read_tsv("ydall.txt") 
ds2 <- readr::read_tsv("umtboth.txt")

aa_lengths <- ds2 %>%
  #select(V_CALL, CDR3_IGBLAST_AA) %>%
  select(CDR3_IGBLAST_AA) %>%
  rename(AA = CDR3_IGBLAST_AA) %>%
  mutate(n_aa = nchar(AA)) %>%
  count(n_aa)

aa_lengths %>%
  ggplot(aes(x=n_aa, y=n)) +
  geom_col()

summary <- ds2 %>%
  select(V_CALL, CDR3_IGBLAST_AA) %>%
  #select(CDR3_IGBLAST_AA) %>%
  rename(AA = CDR3_IGBLAST_AA) %>%
  mutate(n_aa = nchar(AA)) %>%
  relocate(n_aa, .before=AA) %>%
  filter(nchar(AA) < 17 & nchar(AA) >=7) %>%
  #filter(nchar(AA) == 10) %>%
  separate(AA, sep = 1:15, into = as.character(1:16), fill = "right") %>%
  pivot_longer(-(V_CALL:n_aa), names_to = "pos") %>%
  filter(value != "") %>%
  mutate(pos = forcats::as_factor(pos)) %>%
  group_by(V_CALL, pos) %>%
  count(value) %>%
  ungroup() 

summary %>%
  filter(V_CALL == "IGHV3-1") %>%
  ggplot(aes(x = pos, y=n, fill=value)) +
  geom_col() 

data_filt <- summary %>%
  filter(V_CALL == "IGHV3-1") #%>%
  #filter(value == "G")

x <- data_filt %>%
  ggplot(aes(x = pos, y=n, colour =value)) +
  geom_point(aes(shape=factor(value)), size = 3) +
  scale_shape_manual(values = unique(data_filt$value))

plotly::ggplotly(x)

#https://plotly-r.com/working-with-symbols.html
# https://plotly-r.com/linking-views-with-shiny.html#event-priority
# heat map example with plotly
data_filt %>%
  plotly::plot_ly(x= ~pos, y= ~n, color= ~value) %>%
  plotly::add_text(
    text = ~value,
    #hovertext = ~name,
    #hoverinfo = "text",
    size = I(20)
  )

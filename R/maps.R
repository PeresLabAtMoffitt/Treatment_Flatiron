# Map

library(scatterpie)

Frontline <- read_rds("/Users/colinccm/Documents/GitHub/Peres/Treatment_Flatiron/Frontline.rds")
us <- map_data('state')

state_coor <- read_csv("/Users/colinccm/Documents/GitHub/Peres/data/statelatlong.csv") %>% 
  right_join(., Frontline, by = c("State" = "state"))

state_coor <- state_coor %>% 
  group_by(State, practiceid) %>% 
  mutate(nbr_patient_by_practice_id = n()) %>% 
  mutate(attendance = ifelse(nbr_patient_by_practice_id > 150, "popular", "countryside")) %>% 
  select(State, nbr_patient_by_practice_id, attendance, everything())

state_coor1 <- state_coor %>% 
  mutate(p = 1) %>% 
  group_by(State, Longitude, Latitude, RDI_grp) %>% 
  select(State, Longitude, Latitude, RDI_grp, p) %>% 
  summarize(across(c("p"), ~ sum(.x, na.rm = TRUE))) %>% 
  group_by(State, Longitude, Latitude) %>% 
  pivot_wider(names_from = "RDI_grp", values_from = "p") %>% 
  ungroup()

state_coor2 <- state_coor1 %>% 
  mutate(total_pop = rowSums( select(., "RDI >= 0.85":"RDI < 0.85") , na.rm = TRUE))
  # rowwise() %>%
  # mutate(sum = sum(select(.,NHWhite:Other), na.rm = T))


ggplot(us, aes(long, lat)) +
  geom_map(map=us, aes(map_id=region), fill="grey97", color="grey") +
  geom_scatterpie(data = state_coor2, 
                  aes(Longitude, Latitude, r = .7),
                  # Try to calculate ration with state size
                  # or ratio with total pop sqrt(total_pop)/100 
                  cols = c("RDI >= 0.85","RDI < 0.85"), 
                  alpha = 0.5) +
  scale_fill_manual(
    breaks = c("RDI >= 0.85","RDI < 0.85"),
    labels = c("right dose or higher","underdose"),
    values = c("RDI >= 0.85" = "lightgreen",
               "RDI < 0.85" = "red")
  ) +
  labs(title = "Flatiron",
       # subtitle = "RDI",
       caption = "Source: Flatiron",
       fill = NULL) +
  coord_fixed() +
  theme_bw() +
  theme(#legend.position = c(1, 0.02),
        legend.justification = c(1, 0),
        panel.grid = element_blank(),
        # panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

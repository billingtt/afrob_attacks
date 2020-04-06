
get_africa <- function(level) {
  
  afnames <- raster::ccodes() %>% 
    filter(continent == "Africa") %>% 
    dplyr::select(NAME)
  
  temp0 <- vector("list", 0)
  for(i in afnames$NAME) {
    print(i)
    temp0[[i]] <- raster::getData(name = "GADM", 
                                  country = i, 
                                  download = T, 
                                  level = level, 
                                  path = here::here("shapefiles"))
  }
  
  list(temp0, makeUniqueIDs = T) %>% 
    purrr::flatten() %>% 
    do.call(rbind, .) %>% 
    st_as_sf()
  
  
}

plot_coefs <- function(data, dv, title = NULL, subtitle = NULL, iv = NULL) {
  
  
  
  ggplot(data = data) +
    geom_hline(yintercept = 0, color = "darkred") +
    geom_pointrange(aes(x = spatial_window, y = estimate, ymin = estimate - 1.96 * std.error,
                        ymax = estimate + 1.96 * std.error),
                    size = 0.75, fatten = 2) +
    facet_wrap(~temporal_window, nrow = 1) +
    theme_minimal() +
    theme(strip.background = element_rect(fill = "gray20", color = "gray20"),
          panel.grid = element_line(color = "gray95"),
          strip.text = element_text(color = "white"),
          panel.background = element_rect(color = "gray20"),
          legend.position = "bottom") +
    labs(x = "Spatial Window",
         y = paste(iv, "Coefficient"),
         title = title,
         subtitle = subtitle, 
         caption = paste0("Round(s) ",
                          paste(substr(unique(data$data[[1]]$round[!is.na(data$data[[1]][dv])]), 2,2), 
                                collapse = ", ")))
}


lmer_ester <- function(data, dv){
  
  myform <- formula(
  paste0(dv,"~", "log1p(attacks) + factor(DM_fem) + 
  factor(DM_urban) + DM_age + DM_edu + WF_nofood + factor(round) + l
         og1p(lag_nlights_mean) + log1p(lag_pop_gpw_sum) + log1p(lag_capdist) + 
         (1|twnvill) + (1|country)")
  )
  
  
  data %>% 
    mutate(
      # Estimate models
      out = map(data, ~lmer(myform, data = .x)),
      
      # Pull out attack coefs and ses
      outtab = map(out, ~broom::tidy(.x)),
      mycoef = map(outtab, ~filter(.x, term == "log1p(attacks)") %>% dplyr::select(estimate)),
      myse = map(outtab, ~filter(.x, term == "log1p(attacks)") %>% dplyr::select(std.error)))
      
      # # Pull out varying intercepts and ses
      # vcepts = map(out, ~data.frame(est = coef(.x)[[2]][,1],
      #                                             se = se.coef(.x)[[3]][,1],
      #                                             country = rownames(se.coef(.x)[[3]])
      
  }

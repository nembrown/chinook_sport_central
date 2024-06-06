# Coverage Plotting

yearMonth2_season_old <- plyr::ddply(Sport_mark_rate_finescale, c( "YEAR", "MONTH", "finescale_fishery_old"), summarise,
                                 sum_creel = sum(creel, na.rm = TRUE), sum= sum(creel_plus, na.rm = TRUE), sum_historic= sum(historic_plus, na.rm = TRUE), sum_catch_estimate = sum(catch_estimate, na.rm = TRUE), sum_irec = sum(irec_calibrated, na.rm = TRUE)) %>% filter(finescale_fishery_old!="NA") %>% mutate(across(where(is.numeric), ~na_if(., 0)))


yearMonth_catch_estimate_2_season_old<-yearMonth2_season_old %>% filter(sum_catch_estimate!=sum,sum_catch_estimate!=sum_historic, sum_catch_estimate!=sum_irec)

yearMonth_irec_2_season_old<-yearMonth2_season_old %>% filter(sum_catch_estimate==sum_irec)


yearMonth2_season_south_old<- yearMonth2_season_old %>% filter(!str_detect(finescale_fishery_old, "CBC|NBC"))




  p1<-ggplot()+
    geom_tile(data=yearMonth2_season_south_old %>% filter(finescale_fishery_old=="NGS S"), aes(x=YEAR, y=as.factor(MONTH), fill = sum),colour = "white") +
    scale_y_discrete(limits=rev)+
    geom_tile(data=yearMonth2_season_south_old  %>% filter(finescale_fishery_old=="NGS S"), aes(x=YEAR, y=as.factor(MONTH), fill = sum_historic),colour = "white") +
    guides(fill=guide_legend(title="creel and logbook estimates")) +
    scale_fill_gradient(low = col1, high = col2, na.value = colclear)+
    labs(title = paste("Coverage for", "NGS S"),
         x = "Year", y = "Month") +
    theme_bw() + theme_minimal() + geom_vline(xintercept = 2013)

  p2<-ggplot()+
    geom_tile(data=yearMonth2_season_south_old %>% filter(finescale_fishery_old=="NGS S"), aes(x=YEAR, y=as.factor(MONTH), fill = sum),colour = "white") +
    scale_fill_gradient(low = col1, high = col2, na.value = colclear) +
    scale_y_discrete(limits=rev)+
    guides(fill=guide_legend(title="creel and logbook estimates")) +
    geom_tile(data=yearMonth2_season_south_old%>% filter(finescale_fishery_old=="NGS S"), aes(x=YEAR, y=as.factor(MONTH), fill = sum_historic),colour = "white") +
    scale_fill_gradient(low = col1, high = col2, na.value = colclear) +
    new_scale_fill() +
    geom_tile(data=yearMonth_catch_estimate_2_season_old%>% filter(finescale_fishery_old=="NGS S"), aes(x=YEAR, y=as.factor(MONTH), fill = sum_catch_estimate),colour = "white") +
    scale_fill_gradient(low = col3, high = col4, na.value = colclear)+
    guides(fill=guide_legend(title="creel plus iREC estimates")) +
    new_scale_fill() +
    geom_tile(data=yearMonth_irec_2_season_old%>% filter(finescale_fishery_old=="NGS S"), aes(x=YEAR, y=as.factor(MONTH), fill = sum_catch_estimate),colour = "white") +
    scale_fill_gradient(low = col5, high = col6, na.value = colclear)+
    guides(fill=guide_legend(title="iREC estimates")) +
    labs(
      x = "Year", y = "Month") +
    theme_bw() + theme_minimal() + geom_vline(xintercept = 2013)


  p<-p1+p2 + plot_layout(guides = "collect")
  p

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



  #### Modelling plotting


  #### Testing two different plots:
  ggplot(Season_south_combined %>% filter(finescale_fishery=="CA JDF S FALL"), aes(x=creel_plus_summer, y= catch_estimate_predicted+1, col=mark_status, fill=mark_status))+
    geom_point(aes(shape=pred_cat, size=creel_effort))+
    geom_smooth(method="glm", method.args = list(family= Gamma(link = "log")), fullrange=TRUE) + facet_wrap(~mark_status, scales="free") +
    ggtitle(paste("CA JDF S FALL")) + theme_bw() +
    scale_colour_viridis_d(option = "turbo")+  scale_fill_viridis_d(option = "turbo") +scale_size_continuous(range=c(1,3))

  family.set <- family(Season_model_gamma_full_spec)
  ilink.family.set<- family.set$linkinv

  want_marked_kept <- seq(1, nrow(Season_south_no_nas %>% filter(status == "marked_Kept_total")), length.out = 1000)
  want_marked_released <- seq(1, nrow(Season_south_no_nas %>% filter(status == "marked_Released_total")), length.out = 1000)
  want_umarked_kept <- seq(1, nrow(Season_south_no_nas %>% filter(status == "unmarked_Kept_total")), length.out = 1000)
  want_umarked_released <- seq(1, nrow(Season_south_no_nas %>% filter(status == "unmarked_Released_total")), length.out = 1000)

  mod<-Season_model_gamma_full_spec
  ndata_marked_kept <- with(Season_south_no_nas %>% filter(status == "marked_Kept_total"), data_frame(creel_plus_summer= seq(min(creel_plus_summer), max(creel_plus_summer),
                                                                                                                             length = 1000),  status = "marked_Kept_total", season = season[want_marked_kept], finescale_fishery_old=finescale_fishery_old[want_marked_kept], creel_effort=creel_effort[want_marked_kept], finescale_fishery=finescale_fishery[want_marked_kept]))

  ndata_marked_released <- with(Season_south_no_nas %>% filter(status == "marked_Released_total"), data_frame(creel_plus_summer= seq(min(creel_plus_summer), max(creel_plus_summer),
                                                                                                                                     length = 1000),  status = "marked_Released_total", season = season[want_marked_released], finescale_fishery_old=finescale_fishery_old[want_marked_released], creel_effort=creel_effort[want_marked_released], finescale_fishery=finescale_fishery[want_marked_released]))
  ndata_unmarked_kept <- with(Season_south_no_nas %>% filter(status == "unmarked_Kept_total"), data_frame(creel_plus_summer= seq(min(creel_plus_summer), max(creel_plus_summer),
                                                                                                                                 length = 1000),  status = "unmarked_Kept_total", season = season[want_umarked_kept], finescale_fishery_old=finescale_fishery_old[want_umarked_kept], creel_effort=creel_effort[want_umarked_kept], finescale_fishery=finescale_fishery[want_umarked_kept]))
  ndata_unmarked_released <- with(Season_south_no_nas %>% filter(status == "unmarked_Released_total"), data_frame(creel_plus_summer= seq(min(creel_plus_summer), max(creel_plus_summer),
                                                                                                                                         length = 1000),  status = "unmarked_Released_total", season = season[want_umarked_released], finescale_fishery_old=finescale_fishery_old[want_umarked_released], creel_effort=creel_effort[want_umarked_released], finescale_fishery=finescale_fishery[want_umarked_released]))

  ndata<- bind_rows(ndata_marked_kept,ndata_marked_released, ndata_unmarked_kept, ndata_unmarked_released)

  ## add the fitted values by predicting from the model for the new data
  ndata<- add_column(ndata, fit = predict(mod, newdata = ndata, type = 'response'))
  ndata<- bind_cols(ndata, setNames(as_tibble(predict(mod, ndata, se.fit = TRUE)[1:2]),
                                    c('fit_link','se_link')))

  ndata <- mutate(ndata,
                  fit_resp  = ilink.family.set(fit_link),
                  right_upr = ilink.family.set(fit_link + (2 * se_link)),
                  right_lwr = ilink.family.set(fit_link - (2 * se_link)))

  dataminmax<- Season_south_no_nas %>% filter(finescale_fishery=="CA JDF S SUMMER", status=="marked_Kept_total")
  ### This show different data: This is the correct fit.
  ggplot(Season_south_no_nas %>% filter(finescale_fishery=="CA JDF S SUMMER", status=="marked_Kept_total"), aes(x=creel_plus_summer,  y= catch_estimate, col=status))+
    geom_point(aes(size=creel_effort))+
    geom_line(ndata %>% filter(finescale_fishery=="CA JDF S SUMMER", status=="marked_Kept_total"), mapping=aes(y= fit, x=creel_plus_summer, col=status))+
    geom_ribbon(ndata %>% filter(finescale_fishery=="CA JDF S SUMMER", status=="marked_Kept_total"), mapping=aes(y= fit,x=creel_plus_summer, ymin = right_lwr, ymax = right_upr, fill=status), alpha = 0.10)+
    ggtitle(paste("CA JDF S SUMMER")) + theme_bw() +
    scale_colour_viridis_d()+  scale_fill_viridis_d() +scale_size_continuous(range=c(1,3))+
    scale_x_continuous(limits=c(min(dataminmax$creel_plus_summer)-1000,max(dataminmax$creel_plus_summer)+1000)) +
    scale_y_continuous(limits=c(0,max(dataminmax$catch_estimate)+10000))


  dataminmax_marked_kept<- Season_south_no_nas %>% filter(finescale_fishery=="JNST S FALL", status=="marked_Kept_total")
  dataminmax_marked_released<- Season_south_no_nas %>% filter(finescale_fishery=="JNST S FALL", status=="marked_Released_total")
  dataminmax_unmarked_kept<- Season_south_no_nas %>% filter(finescale_fishery=="JNST S FALL", status=="unmarked_Kept_total")
  dataminmax_unmarked_released<- Season_south_no_nas %>% filter(finescale_fishery=="JNST S FALL", status=="unmarked_Released_total")

  g1<-ggplot(Season_south_no_nas %>% filter(finescale_fishery=="JNST S FALL", status=="marked_Kept_total"), aes(x=creel_plus_summer,  y= catch_estimate, col=status))+
    geom_point(aes(size=creel_effort))+
    geom_line(ndata %>% filter(finescale_fishery=="JNST S FALL", status=="marked_Kept_total"), mapping=aes(y= fit, x=creel_plus_summer, col=status))+
    geom_ribbon(ndata %>% filter(finescale_fishery=="JNST S FALL", status=="marked_Kept_total"), mapping=aes(y= fit,x=creel_plus_summer, ymin = right_lwr, ymax = right_upr, fill=status), alpha = 0.10)+
    theme_bw() +
    scale_size_continuous(range=c(1,3))+ scale_colour_manual(values=c("#440154"))+  scale_fill_manual(values=c("#440154"))+
    coord_cartesian(ylim = c(0,max(dataminmax_marked_kept$catch_estimate)), xlim = c(0,max(dataminmax_marked_kept$creel_plus_summer)))

  g2<-ggplot(Season_south_no_nas %>% filter(finescale_fishery=="JNST S FALL", status=="marked_Released_total"), aes(x=creel_plus_summer,  y= catch_estimate, col=status))+
    geom_point(aes(size=creel_effort))+
    geom_line(ndata %>% filter(finescale_fishery=="JNST S FALL", status=="marked_Released_total"), mapping=aes(y= fit, x=creel_plus_summer, col=status))+
    geom_ribbon(ndata %>% filter(finescale_fishery=="JNST S FALL", status=="marked_Released_total"), mapping=aes(y= fit,x=creel_plus_summer, ymin = right_lwr, ymax = right_upr, fill=status), alpha = 0.10)+
    theme_bw() +
    scale_size_continuous(range=c(1,3))+ scale_colour_manual(values=c("#31688e"))+  scale_fill_manual(values=c("#31688e"))+
    coord_cartesian(ylim = c(0,max(dataminmax_marked_released$catch_estimate)), xlim = c(0,max(dataminmax_marked_released$creel_plus_summer)))

  g3<-ggplot(Season_south_no_nas %>% filter(finescale_fishery=="JNST S FALL", status=="unmarked_Kept_total"), aes(x=creel_plus_summer,  y= catch_estimate, col=status))+
    geom_point(aes(size=creel_effort))+
    geom_line(ndata %>% filter(finescale_fishery=="JNST S FALL", status=="unmarked_Kept_total"), mapping=aes(y= fit, x=creel_plus_summer, col=status))+
    geom_ribbon(ndata %>% filter(finescale_fishery=="JNST S FALL", status=="unmarked_Kept_total"), mapping=aes(y= fit,x=creel_plus_summer, ymin = right_lwr, ymax = right_upr, fill=status), alpha = 0.10)+
    theme_bw() +
    scale_size_continuous(range=c(1,3))+ scale_colour_manual(values=c("#35b779"))+  scale_fill_manual(values=c("#35b779"))+
    coord_cartesian(ylim = c(0,max(dataminmax_unmarked_kept$catch_estimate)), xlim = c(0,max(dataminmax_unmarked_kept$creel_plus_summer)))

  g4<-ggplot(Season_south_no_nas %>% filter(finescale_fishery=="JNST S FALL", status=="unmarked_Released_total"), aes(x=creel_plus_summer,  y= catch_estimate, col=status))+
    geom_point(aes(size=creel_effort))+
    geom_line(ndata %>% filter(finescale_fishery=="JNST S FALL", status=="unmarked_Released_total"), mapping=aes(y= fit, x=creel_plus_summer, col=status))+
    geom_ribbon(ndata %>% filter(finescale_fishery=="JNST S FALL", status=="unmarked_Released_total"), mapping=aes(y= fit,x=creel_plus_summer, ymin = right_lwr, ymax = right_upr, fill=status, col=status), alpha = 0.10)+
    theme_bw() +
    scale_size_continuous(range=c(1,3))+ scale_colour_manual(values=c("#fde725"))+  scale_fill_manual(values=c("#fde725"))+
    coord_cartesian(ylim = c(0,max(dataminmax_unmarked_released$catch_estimate)), xlim = c(0,max(dataminmax_unmarked_released$creel_plus_summer)))

  g<-g1+g2 + g3+g4 + plot_layout(guides = "collect")
  g

  Season_south_combined_2 <- add_column(Season_south_combined, fit = predict(Season_model_gamma_drop_kept_spec, newdata = Season_south_combined, type = 'response'))
  Season_south_combined_2  <- bind_cols(Season_south_combined_2 , setNames(as_tibble(predict(Season_model_gamma_drop_kept_spec, Season_south_combined , se.fit = TRUE)[1:2]), c('fit_link','se_link')))

  family.set <- family(Season_model_gamma_drop_kept_spec)
  ilink.family.set<- family.set$linkinv

  Season_south_combined_2  <- mutate(Season_south_combined_2 ,
                                     fit_resp  = ilink.family.set(fit_link),
                                     right_upr = ilink.family.set(fit_link + (2 * se_link)),
                                     right_lwr = ilink.family.set(fit_link - (2 * se_link)))


  ggplot(Season_south_combined_2 %>% filter(finescale_fishery=="CA JDF S FALL"), aes(x=creel_plus_summer, y= catch_estimate_predicted+1, col=mark_status, fill=mark_status))+
    geom_point(aes(shape=pred_cat, size=creel_effort))+
    geom_line()+
    geom_ribbon(aes(ymin = right_lwr, ymax = right_upr, fill=mark_status), alpha = 0.10)+
    facet_wrap(~mark_status, scales="free") +
    ggtitle(paste("CA JDF S FALL")) + theme_bw() +
    scale_colour_viridis_d(option = "turbo")+  scale_fill_viridis_d(option = "turbo") +scale_size_continuous(range=c(1,3))


  plt.gam.hydroid <- ggplot(ndata.hydroid, aes(x = min.10.pH.unscaled, y = fit)) +
    geom_line(aes(colour=oFood.quality)) +
    geom_point(aes(y = hydroid.001, shape=CO2, colour=oFood.quality), data = food.exp.data.12.2019_zscores)+
    xlab(expression("Minimum" ~"10"^"th"~"percentile pH")) + ylab(expression(atop(NA,atop(textstyle(italic("Obelia")~ "abundance"), textstyle("(proportion cover)")))))+
    scale_color_manual(values=colorset2, guide = guide_legend(title="Food supplement", title.position = "top"))+
    scale_fill_manual(values=colorset2, guide = FALSE)+
    scale_shape_manual(values=c(19,17), labels=c("Ambient", "Low pH"), guide = guide_legend(title="pH treatment", title.position = "top"))+
    geom_ribbon(data = ndata.hydroid,aes(ymin = right_lwr, ymax = right_upr, fill=oFood.quality), alpha = 0.10)+
    theme(legend.position='none')
  plt.gam.hydroid
  ggsave("C:Data//Graphs March 2020//hydroid_pred.png")



  #
  # ggplot(Sport_mark_rate_finescale_combined %>% filter(season=="summer")%>% filter(!str_detect(finescale_fishery, "CBC|NBC")), aes(y=creel_plus_summer+1, x=creel_effort)) +
  #   geom_point() + geom_smooth(method="glm", method.args = list(family= Gamma(link = "log"))) + facet_wrap(~finescale_fishery, scales="free")
  #
  # ggplot(Sport_mark_rate_finescale_combined %>% filter(season=="summer")%>% filter(str_detect(finescale_fishery, "CBC|NBC")), aes(y=historic_summer+1, x=historic_effort)) +
  #   geom_point() + geom_smooth(method="glm", method.args = list(family= Gamma(link = "log"))) + facet_wrap(~finescale_fishery, scales="free")







  col1 = "#d8e1cf"
  col2 = "#438484"
  col3 = "lightblue"
  col4 = "lightblue4"
  col5 = "pink"
  col6 = "darkred"
  ####
  yearMonth <- plyr::ddply(Sport_mark_rate_finescale, c( "YEAR", "MONTH", "finescale_fishery_old"), summarise,
                           sum= sum(creel_plus, na.rm = TRUE), sum_historic= sum(historic_plus, na.rm = TRUE), sum_catch_estimate = sum(catch_estimate, na.rm = TRUE), sum_irec = sum(irec_calibrated, na.rm = TRUE))

  yearMonth_creel_1<-yearMonth %>% filter(sum!=0, finescale_fishery_old!="NA")
  yearMonth_historic_1<-yearMonth %>% filter(sum_historic!=0, finescale_fishery_old!="NA")


  ggplot()+
    geom_tile(data=yearMonth_creel_1, aes(x=YEAR, y=as.factor(MONTH), fill = sum),colour = "white") +
    scale_fill_gradient(low = col1, high = col2) +
    scale_y_discrete(limits=rev)+
    guides(fill=guide_legend(title="creel only estimates")) +
    new_scale_fill() +
    geom_tile(data=yearMonth_historic_1, aes(x=YEAR, y=as.factor(MONTH), fill = sum_historic),colour = "white") +
    scale_fill_gradient(low = col1, high = col2)+
    facet_wrap(~finescale_fishery_old)+
    labs(title = "Coverage by finescale fishery",
         x = "Year", y = "Month") +
    theme_bw() + theme_minimal() + geom_vline(xintercept = 2012)


  yearMonth_creel_2<-yearMonth %>% filter(sum!=0, sum_catch_estimate==sum, finescale_fishery_old!="NA")
  yearMonth_catch_estimate_1<-yearMonth %>% filter(sum_catch_estimate!=sum, sum_catch_estimate!=sum_irec, finescale_fishery_old!="NA")
  yearMonth_irec_1<-yearMonth %>% filter(sum_irec!=0, sum_catch_estimate==sum_irec, finescale_fishery_old!="NA")




  ggplot()+
    geom_tile(data=yearMonth_creel_2, aes(x=YEAR, y=as.factor(MONTH), fill = sum_catch_estimate),colour = "white") +
    scale_fill_gradient(low = col1, high = col2) +
    scale_y_discrete(limits=rev)+
    guides(fill=guide_legend(title="creel only estimates")) +
    new_scale_fill() +
    geom_tile(data=yearMonth_catch_estimate_1, aes(x=YEAR, y=as.factor(MONTH), fill = sum_catch_estimate),colour = "white") +
    scale_fill_gradient(low = col3, high = col4)+
    guides(fill=guide_legend(title="creel plus irec estimates")) +
    new_scale_fill() +
    geom_tile(data=yearMonth_irec_1, aes(x=YEAR, y=as.factor(MONTH), fill = sum_catch_estimate),colour = "white") +
    scale_fill_gradient(low = col5, high = col6)+
    facet_wrap(~finescale_fishery_old)+
    guides(fill=guide_legend(title="irec only estimates")) +
    labs(title = "Coverage by finescale fishery",
         x = "Year", y = "Month") +
    theme_bw() + theme_minimal() + geom_vline(xintercept = 2012)


  #Marked Kept only
  ggplot()+
    geom_tile(data=yearMonth_creel %>% filter(status == "marked_Kept_total"), aes(x=YEAR, y=as.factor(MONTH), fill = sum),colour = "white") +
    scale_fill_gradient(low = col1, high = col2) +
    new_scale_fill() +
    geom_tile(data=yearMonth_irec, aes(x=YEAR, y=as.factor(MONTH), fill = sum_irec),colour = "white") +
    scale_fill_gradient(low = col3, high = col4) +
    facet_wrap(~finescale_fishery)+
    scale_y_discrete(limits=rev)+
    guides(fill=guide_legend(title="iREC estimates")) +
    labs(title = "Coverage by finescale fishery - marked_Kept_total",
         x = "Year", y = "Month") +
    theme_bw() + theme_minimal() + geom_vline(xintercept = 2012)

  # Marked Released
  ggplot()+
    geom_tile(data=yearMonth_creel %>% filter(status == "marked_Released_total"), aes(x=YEAR, y=as.factor(MONTH), fill = sum),colour = "white") +
    scale_fill_gradient(low = col1, high = col2) +
    new_scale_fill() +
    geom_tile(data=yearMonth_irec, aes(x=YEAR, y=as.factor(MONTH), fill = sum_irec),colour = "white") +
    scale_fill_gradient(low = col3, high = col4) +
    facet_wrap(~finescale_fishery)+
    scale_y_discrete(limits=rev)+
    guides(fill=guide_legend(title="iREC estimates")) +
    labs(title = "Coverage by finescale fishery - marked_Released_total",
         x = "Year", y = "Month") +
    theme_bw() + theme_minimal() + geom_vline(xintercept = 2012)

  #Unmarked Kept only
  ggplot()+
    geom_tile(data=yearMonth_creel %>% filter(status == "unmarked_Kept_total"), aes(x=YEAR, y=as.factor(MONTH), fill = sum),colour = "white") +
    scale_fill_gradient(low = col1, high = col2) +
    new_scale_fill() +
    geom_tile(data=yearMonth_irec, aes(x=YEAR, y=as.factor(MONTH), fill = sum_irec),colour = "white") +
    scale_fill_gradient(low = col3, high = col4) +
    facet_wrap(~finescale_fishery)+
    scale_y_discrete(limits=rev)+
    guides(fill=guide_legend(title="iREC estimates")) +
    labs(title = "Coverage by finescale fishery - unmarked_Kept_total",
         x = "Year", y = "Month") +
    theme_bw() + theme_minimal() + geom_vline(xintercept = 2012)


  #Unmarked Released
  ggplot()+
    geom_tile(data=yearMonth_creel %>% filter(status == "unmarked_Released_total"), aes(x=YEAR, y=as.factor(MONTH), fill = sum),colour = "white") +
    scale_fill_gradient(low = col1, high = col2) +
    new_scale_fill() +
    geom_tile(data=yearMonth_irec, aes(x=YEAR, y=as.factor(MONTH), fill = sum_irec),colour = "white") +
    scale_fill_gradient(low = col3, high = col4) +
    facet_wrap(~finescale_fishery)+
    scale_y_discrete(limits=rev)+
    guides(fill=guide_legend(title="iREC estimates")) +
    labs(title = "Coverage by finescale fishery - unmarked_Released_total",
         x = "Year", y = "Month") +
    theme_bw() + theme_minimal() + geom_vline(xintercept = 2012)

  ################## TERMINAL

  yearMonth_term <- plyr::ddply(Sport_mark_rate_finescale, c( "YEAR", "MONTH", "finescale_fishery_term"), summarise,
                                sum = sum(creel_plus), sum_catch_estimate = sum(catch_estimate), sum_irec = sum(irec_calibrated))

  yearMonth_creel_term<-yearMonth_term %>% filter(sum!=0, sum_catch_estimate==sum, finescale_fishery_term!="NA")
  yearMonth_catch_estimate_term<-yearMonth_term %>% filter(sum_catch_estimate!=sum, sum_catch_estimate!=sum_irec, finescale_fishery_term!="NA")
  yearMonth_irec_term<-yearMonth_term %>% filter(sum_irec!=0, sum_catch_estimate==sum_irec, finescale_fishery_term!="NA")


  #overall summary
  ggplot()+
    geom_tile(data=yearMonth_creel_term, aes(x=YEAR, y=as.factor(MONTH), fill = sum_catch_estimate),colour = "white") +
    scale_fill_gradient(low = col1, high = col2) +
    scale_y_discrete(limits=rev)+
    guides(fill=guide_legend(title="creel only estimates")) +
    new_scale_fill() +
    geom_tile(data=yearMonth_catch_estimate_term, aes(x=YEAR, y=as.factor(MONTH), fill = sum_catch_estimate),colour = "white") +
    scale_fill_gradient(low = col3, high = col4)+
    guides(fill=guide_legend(title="creel plus irec estimates")) +
    new_scale_fill() +
    geom_tile(data=yearMonth_irec_term, aes(x=YEAR, y=as.factor(MONTH), fill = sum_catch_estimate),colour = "white") +
    scale_fill_gradient(low = col5, high = col6)+
    facet_wrap(~finescale_fishery_term)+
    guides(fill=guide_legend(title="irec only estimates")) +
    labs(title = "Coverage by terminal finescale fishery",
         x = "Year", y = "Month") +
    theme_bw() + theme_minimal() + geom_vline(xintercept = 2012)





  ### MRR / UKR PLots


  ggplot() +
    geom_point(data=Sport_mark_rate_mrr, aes(y=unmarked_release_corrected, x=YEAR, col="lightblue")) + geom_line(data=Sport_mark_rate_mrr, aes(y=unmarked_release_corrected, x=YEAR, col="lightblue"))+
    geom_point(data=Sport_mark_rate_mrr, aes(y=mrr_corrected, x=YEAR, col="lightblue4")) + geom_line(data=Sport_mark_rate_mrr, aes(y=mrr_corrected, x=YEAR, col="lightblue4"))+
    # geom_point(data=Sport_mark_rate_mrr_creel, aes(y=ukr, x=YEAR, col="lightgreen")) + geom_line(data=Sport_mark_rate_mrr_creel, aes(y=ukr, x=YEAR, col="lightgreen"))+
    # geom_point(data=Sport_mark_rate_mrr_creel, aes(y=mrr, x=YEAR, col="darkgreen")) + geom_line(data=Sport_mark_rate_mrr_creel, aes(y=mrr, x=YEAR, col="darkgreen"))+
    scale_color_manual(values=c("lightblue", "lightblue4", "lightgreen", "darkgreen"),  labels = c("URR", "MRR", "URR", "MRR"))+
    theme(legend.position="bottom")+
    ylab("Proportion")+
    facet_wrap(~finescale_fishery)+
    theme_bw()+ geom_vline(xintercept = 2012)

  ### by month plot:
  ggplot(Sport_mark_rate_mrr %>% filter(finescale_fishery_old=="CA JDF S")) +
    geom_point( aes(y=mrr, x=month(MONTH, label=TRUE),  col="lightblue4", group=finescale_fishery_old)) +
    geom_line( aes(y=mrr, x=month(MONTH, label=TRUE),  col="lightblue4", group=finescale_fishery_old))+
    geom_point(aes(y=unmarked_release, x=month(MONTH, label=TRUE),  col="lightblue", group=finescale_fishery_old)) +
    geom_line( aes(y=unmarked_release, x=month(MONTH, label=TRUE),  col="lightblue", group=finescale_fishery_old))+
    scale_color_manual(values=c("lightblue", "lightblue4", "lightgreen", "darkgreen"),  labels = c("Unmarked Release Rate", "Marked Release Rate", "UKR", "MRR"))+
    facet_wrap(~YEAR) +scale_x_discrete(guide = guide_axis(angle = 90)) +
    xlab("Month") + ylab("MRR")+ theme_bw()+ geom_vline(xintercept = 2012)


  Sport_mark_rate_mrr_creel<-Sport_mark_rate_finescale_combined %>%
    pivot_wider(id_cols = c(YEAR, finescale_fishery), names_from=status, values_from = creel_plus) %>%
    mutate(mrr=marked_Released_total/(marked_Kept_total+marked_Released_total),
           ukr=unmarked_Kept_total/(unmarked_Kept_total+unmarked_Released_total)) %>%
    mutate(mrrplusukr=mrr + ukr)

  ggplot(Sport_mark_rate_mrr_creel) + geom_point(aes(y=ukr, x=YEAR, col="lightblue")) + geom_line(aes(y=ukr, x=YEAR, col="lightblue"))+
    geom_point(aes(y=mrr, x=YEAR, col="lightblue4")) + geom_line(aes(y=mrr, x=YEAR, col="lightblue4"))+
    scale_color_manual(values=c("lightblue", "lightblue4"),  labels = c("UKR", "MRR"))+
    theme(legend.position="bottom")+
    ylab("Proportion")+
    facet_wrap(~finescale_fishery)+
    theme_bw()+ geom_vline(xintercept = 2012)


  ggplot(Sport_mark_rate_finescale_combined, aes(y=catch_estimate, x=YEAR, col=as.factor(status))) + geom_point() + geom_line()+ facet_wrap(~finescale_fishery)


  ggplot(Sport_mark_rate_finescale_combined %>% filter(YEAR==2023, status=="marked_Kept_total"), aes(x=creel_plus, y=catch_estimate, col=as.factor(finescale_fishery))) + geom_point()
  ggplot(Sport_mark_rate %>% filter(YEAR==2022), aes(x=creel_plus, y=catch_estimate)) + geom_point()



  ##
  ##maybe exclude 2016?
  Sport_mark_kept<-Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023), status == "marked_Kept_total")
  ggplot(Sport_mark_kept, aes(x=creel_plus_summer, y= catch_estimate))+geom_point()+geom_abline(slope=1)+
    geom_smooth(method="lm") + facet_wrap(~finescale_fishery, scales="free") + ggtitle("marked Kept")


  Sport_mark_released<-Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023), status == "marked_Released_total")
  ggplot(Sport_mark_released, aes(x=creel_plus_summer, y= catch_estimate))+geom_point()+geom_abline(slope=1)+
    geom_smooth(method="lm") + facet_wrap(~finescale_fishery, scales="free") + ggtitle("marked Released")

  Sport_unmarked_released<-Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023), status == "unmarked_Released_total")
  ggplot(Sport_unmarked_released, aes(x=creel_plus_summer, y= catch_estimate))+geom_point()+geom_abline(slope=1)+
    geom_smooth(method="lm") + facet_wrap(~finescale_fishery, scales="free") + ggtitle("unmarked Released")


  Sport_unmarked_kept<-Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023), status == "unmarked_Kept_total")
  ggplot(Sport_unmarked_kept, aes(x=creel_plus_summer, y= catch_estimate))+geom_point()+geom_abline(slope=1)+
    geom_smooth(method="lm") + facet_wrap(~finescale_fishery, scales="free") + ggtitle("unmarked Kept")


  ggplot(Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023), finescale_fishery=="WCVI AABM S SUMMER"), aes(x=creel_plus_summer, y= catch_estimate))+geom_point()+geom_abline(slope=1)+
    geom_smooth(method="lm") + facet_wrap(~status, scales="free") + ggtitle("WCVI AABM S SUMMER") + theme_bw()

  ggplot(Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023), finescale_fishery_old=="WCVI AABM S"), aes(x=creel_plus_summer, y= catch_estimate, col=status, fill=status))+geom_point()+geom_abline(slope=1)+
    geom_smooth(method="lm") + facet_wrap(~finescale_fishery, scales="free") + ggtitle("WCVI AABM S") + theme_bw() + scale_colour_viridis_d() + scale_fill_viridis_d()



  ggplot(Sport_mark_rate_finescale_combined %>% filter(YEAR %in% c(2013:2023)), aes(x=creel_plus_summer, y= catch_estimate, col=status, fill=status))+geom_point()+geom_abline(slope=1)+
    geom_smooth(method="lm") + facet_wrap(~finescale_fishery, scales="free")+ theme_bw() + scale_colour_viridis_d() + scale_fill_viridis_d()




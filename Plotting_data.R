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







  family.set <- family(Summer_north_aabm_model_gamma_spec)
  ilink.family.set<- family.set$linkinv

  mod<-Summer_north_aabm_model_gamma_spec

  ndata<-Summer_north_aabm_no_nas %>% group_by(status, finescale_fishery_old, finescale_fishery, season) %>%  tidyr::expand(historic_summer = seq(0, max(historic_summer), length=100))


  ## add the fitted values by predicting from the model for the new data
  ndata<- add_column(ndata, fit = predict(mod, newdata = ndata, type = 'response'))
  ndata<- bind_cols(ndata, setNames(as_tibble(predict(mod, ndata, se.fit = TRUE)[1:2]),
                                    c('fit_link','se_link')))

  ndata <- mutate(ndata,
                  fit_resp  = ilink.family.set(fit_link),
                  right_upr = ilink.family.set(fit_link + (2 * se_link)),
                  right_lwr = ilink.family.set(fit_link - (2 * se_link)))



  yearMonth2_season_north_aabm<- yearMonth2_season %>% filter(str_detect(finescale_fishery, "NBC AABM"))
  fishery_name_north_aabm<- sort(unique(yearMonth2_season_north_aabm$finescale_fishery))

    p1<-ggplot()+
      geom_tile(data=yearMonth2_season_north_aabm %>% filter(finescale_fishery=="NBC AABM S SUMMER"), aes(x=YEAR, y=as.factor(MONTH), fill = sum),colour = "white") +
      scale_y_discrete(limits=rev)+
      geom_tile(data=yearMonth2_season_north_aabm  %>% filter(finescale_fishery=="NBC AABM S SUMMER"), aes(x=YEAR, y=as.factor(MONTH), fill = sum_historic),colour = "white") +
      guides(fill=guide_legend(title="creel and logbook estimates")) +
      scale_fill_gradient(low = col1, high = col2, na.value = colclear)+
      labs(title = paste("Coverage for", "NBC AABM S SUMMER"),
           x = "Year", y = "Month") +
      theme_bw() + theme_minimal() + geom_vline(xintercept = 2013)

    p2<-ggplot()+
      geom_tile(data=yearMonth2_season_north_aabm %>% filter(finescale_fishery=="NBC AABM S SUMMER"), aes(x=YEAR, y=as.factor(MONTH), fill = sum),colour = "white") +
      scale_fill_gradient(low = col1, high = col2, na.value = colclear) +
      scale_y_discrete(limits=rev)+
      guides(fill=guide_legend(title="creel and logbook estimates")) +
      geom_tile(data=yearMonth2_season_north_aabm%>% filter(finescale_fishery=="NBC AABM S SUMMER"), aes(x=YEAR, y=as.factor(MONTH), fill = sum_historic),colour = "white") +
      scale_fill_gradient(low = col1, high = col2, na.value = colclear) +
      new_scale_fill() +
      geom_tile(data=yearMonth_catch_estimate_2_season%>% filter(finescale_fishery=="NBC AABM S SUMMER"), aes(x=YEAR, y=as.factor(MONTH), fill = sum_catch_estimate),colour = "white") +
      scale_fill_gradient(low = col3, high = col4, na.value = colclear)+
      guides(fill=guide_legend(title="creel plus iREC estimates")) +
      new_scale_fill() +
      geom_tile(data=yearMonth_irec_2_season%>% filter(finescale_fishery=="NBC AABM S SUMMER"), aes(x=YEAR, y=as.factor(MONTH), fill = sum_catch_estimate),colour = "white") +
      scale_fill_gradient(low = col5, high = col6, na.value = colclear)+
      guides(fill=guide_legend(title="iREC estimates")) +
      labs(
        x = "Year", y = "Month") +
      theme_bw() + theme_minimal() + geom_vline(xintercept = 2013)


    p<-p1+p2 + plot_layout(guides = "collect") & theme(legend.position = 'none')




    #####

    dataminmax_marked_kept<- Summer_north_aabm_no_nas %>% filter(finescale_fishery=="NBC AABM S SUMMER", status=="marked_Kept_total")
    dataminmax_marked_released<- Summer_north_aabm_no_nas %>% filter(finescale_fishery=="NBC AABM S SUMMER", status=="marked_Released_total")
    dataminmax_unmarked_kept<- Summer_north_aabm_no_nas %>% filter(finescale_fishery=="NBC AABM S SUMMER", status=="unmarked_Kept_total")
    dataminmax_unmarked_released<- Summer_north_aabm_no_nas %>% filter(finescale_fishery=="NBC AABM S SUMMER", status=="unmarked_Released_total")

    dataminmax_marked_kept<- Summer_north_aabm_no_nas %>% filter(finescale_fishery=="NBC AABM S SUMMER", status=="marked_Kept_total")
    dataminmax_marked_released<- Summer_north_aabm_no_nas %>% filter(finescale_fishery=="NBC AABM S SUMMER", status=="marked_Released_total")
    dataminmax_unmarked_kept<- Summer_north_aabm_no_nas %>% filter(finescale_fishery=="NBC AABM S SUMMER", status=="unmarked_Kept_total")
    dataminmax_unmarked_released<- Summer_north_aabm_no_nas %>% filter(finescale_fishery=="NBC AABM S SUMMER", status=="unmarked_Released_total")

    g1<-ggplot(Summer_north_aabm_no_nas %>% filter(finescale_fishery=="NBC AABM S SUMMER", status=="marked_Kept_total"), aes(x=historic_summer,  y= catch_estimate, col=status))+
      geom_point()+
      geom_line(ndata %>% filter(finescale_fishery=="NBC AABM S SUMMER", status=="marked_Kept_total"), mapping=aes(y= fit, x=historic_summer, col=status))+
      geom_ribbon(ndata %>% filter(finescale_fishery=="NBC AABM S SUMMER", status=="marked_Kept_total"), mapping=aes(y= fit,x=historic_summer, ymin = right_lwr, ymax = right_upr, fill=status), alpha = 0.10)+
      theme_bw() +
      scale_size_continuous(range=c(1,3))+ scale_colour_manual(values=c("#440154"))+  scale_fill_manual(values=c("#440154"))+
      coord_cartesian(ylim = c(0,max(dataminmax_marked_kept$catch_estimate)), xlim = c(0,max(dataminmax_marked_kept$historic_summer)))

    g2<-ggplot(Summer_north_aabm_no_nas %>% filter(finescale_fishery=="NBC AABM S SUMMER", status=="marked_Released_total"), aes(x=historic_summer,  y= catch_estimate, col=status))+
      geom_point()+
      geom_line(ndata %>% filter(finescale_fishery=="NBC AABM S SUMMER", status=="marked_Released_total"), mapping=aes(y= fit, x=historic_summer, col=status))+
      geom_ribbon(ndata %>% filter(finescale_fishery=="NBC AABM S SUMMER", status=="marked_Released_total"), mapping=aes(y= fit,x=historic_summer, ymin = right_lwr, ymax = right_upr, fill=status), alpha = 0.10)+
      theme_bw() +
      scale_size_continuous(range=c(1,3))+ scale_colour_manual(values=c("#31688e"))+  scale_fill_manual(values=c("#31688e"))+
      coord_cartesian(ylim = c(0,max(dataminmax_marked_released$catch_estimate)), xlim = c(0,max(dataminmax_marked_released$historic_summer)))

    g3<-ggplot(Summer_north_aabm_no_nas %>% filter(finescale_fishery=="NBC AABM S SUMMER", status=="unmarked_Kept_total"), aes(x=historic_summer,  y= catch_estimate, col=status))+
      geom_point()+
      geom_line(ndata %>% filter(finescale_fishery=="NBC AABM S SUMMER", status=="unmarked_Kept_total"), mapping=aes(y= fit, x=historic_summer, col=status))+
      geom_ribbon(ndata %>% filter(finescale_fishery=="NBC AABM S SUMMER", status=="unmarked_Kept_total"), mapping=aes(y= fit,x=historic_summer, ymin = right_lwr, ymax = right_upr, fill=status), alpha = 0.10)+
      theme_bw() +
      scale_size_continuous(range=c(1,3))+ scale_colour_manual(values=c("#35b779"))+  scale_fill_manual(values=c("#35b779"))+
      coord_cartesian(ylim = c(0,max(dataminmax_unmarked_kept$catch_estimate)), xlim = c(0,max(dataminmax_unmarked_kept$historic_summer)))

    g4<-ggplot(Summer_north_aabm_no_nas %>% filter(finescale_fishery=="NBC AABM S SUMMER", status=="unmarked_Released_total"), aes(x=historic_summer,  y= catch_estimate, col=status))+
      geom_point()+
      geom_line(ndata %>% filter(finescale_fishery=="NBC AABM S SUMMER", status=="unmarked_Released_total"), mapping=aes(y= fit, x=historic_summer, col=status))+
      geom_ribbon(ndata %>% filter(finescale_fishery=="NBC AABM S SUMMER", status=="unmarked_Released_total"), mapping=aes(y= fit,x=historic_summer, ymin = right_lwr, ymax = right_upr, fill=status, col=status), alpha = 0.10)+
      theme_bw() +
      scale_size_continuous(range=c(1,3))+ scale_colour_manual(values=c("#fde725"))+  scale_fill_manual(values=c("#fde725"))+
      coord_cartesian(ylim = c(0,max(dataminmax_unmarked_released$catch_estimate)), xlim = c(0,max(dataminmax_unmarked_released$historic_summer)))

    g<-g1+g2 + g3+g4 + plot_layout(guides = "collect")
    g

    resize(g, 7, 5)




    m<-ggplot(Season_north_aabm_combined %>% filter(finescale_fishery=="NBC AABM S SUMMER")) +
      geom_point(size=2.5,  aes(y=historic, x=YEAR,col=status, fill=status, shape=pred_cat))+
      geom_line(aes(y=historic, x=YEAR,col=status, linetype = "Unfiltered creel and logbook"))+
      scale_shape_manual(values=c(1,1))+
      new_scale("shape")+
      scale_colour_viridis_d()+
      geom_point(size=2.5, aes(y=catch_estimate_predicted, x=YEAR,col=status, fill=status, shape=pred_cat))+
      geom_line(aes(y=catch_estimate_predicted, x=YEAR,col=status, linetype = "iREC included"))+
      scale_shape_manual(values=c(16,17))+
      scale_linetype_manual(values=c(1,2))+
      facet_wrap(~status, scales="free") + ggtitle(paste("NBC AABM S SUMMER")) + theme_bw()





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

############ Old finescale

 models_combined_summ_by_kept <-models_combined %>%
    group_by(YEAR, finescale_fishery_old, pred_cat, kept_status) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE)



ggplot(models_combined_summ_by_kept) +
      geom_point(size=2.5,  aes(y=creel_unfiltered_plus, x=YEAR,col=kept_status, fill=kept_status, shape=pred_cat))+
      geom_line(aes(y=creel_unfiltered_plus, x=YEAR,col=kept_status, linetype = " Unfiltered creel and logbook"))+
      scale_shape_manual(values=c(1,1))+
      new_scale("shape")+
      scale_colour_viridis_d()+
      geom_point(size=2.5, aes(y=catch_estimate_predicted, x=YEAR,col=kept_status, fill=kept_status, shape=pred_cat))+
      geom_line(aes(y=catch_estimate_predicted, x=YEAR,col=kept_status, linetype = "iREC included"))+
      scale_shape_manual(values=c(16,17))+
      scale_linetype_manual(values=c(2,1))+
      facet_wrap(~finescale_fishery_old + kept_status, scales="free")  + theme_bw()




  ### MRR / UKR PLots
cnr_canada_sport<-read.csv("cnr_canada_sport.csv") %>% as_tibble() %>% filter(year>2004)

fishery_map<-models_combined %>% dplyr::select(finescale_fishery_old, finescale_fishery) %>%
             mutate(erafishery = case_when(
             finescale_fishery_old %in% c("NWCVI S AABM", "SWCVI S AABM") ~ "WCVI AABM S",
             finescale_fishery_old %in% c("NGS S", "SGS S") ~  "GEO ST S",
             finescale_fishery_old == "CA JDF S" ~ "BC JF S",
             finescale_fishery_old %in%  c("NWCVI S ISBM", "SWCVI S ISBM") ~ "WCVI ISBM S",
             TRUE ~ finescale_fishery_old
             )) %>% unique()

cnr_canada_sport<-cnr_canada_sport %>% left_join(fishery_map) %>% rename(YEAR=year)

Sport_mark_rate_mrr<- Sport_mark_rate_mrr %>% left_join(cnr_canada_sport)
Sport_mark_rate_mrr<-Sport_mark_rate_mrr %>% mutate(Kept_total = marked_Kept_total + unmarked_Kept_total,
                                                    Released_total = marked_Released_total + unmarked_Released_total)


  ggplot() +
    geom_point(data=Sport_mark_rate_mrr, aes(y=unmarked_release, x=YEAR, col="lightblue")) + geom_line(data=Sport_mark_rate_mrr, aes(y=unmarked_release, x=YEAR, col="lightblue"))+
    geom_point(data=Sport_mark_rate_mrr, aes(y=mrr, x=YEAR, col="lightblue4")) + geom_line(data=Sport_mark_rate_mrr, aes(y=mrr, x=YEAR, col="lightblue4"))+
    geom_point(data=Sport_mark_rate_mrr, aes(y=Release_rate, x=YEAR, col="lightgreen")) + geom_line(data=Sport_mark_rate_mrr, aes(y=Release_rate, x=YEAR, col="lightgreen"))+
    scale_color_manual(values=c("lightblue", "lightblue4", "lightgreen"),  labels = c("URR", "MRR", "RR"))+
    theme(legend.position="bottom")+
    ylab("Proportion")+
    facet_wrap(~finescale_fishery)+
    theme_bw()+ geom_vline(xintercept = 2012)


#this one:
  ggplot() +
    geom_point(data=Sport_mark_rate_mrr, aes(y=unmarked_release_corrected, x=YEAR, col="lightblue")) + geom_line(data=Sport_mark_rate_mrr, aes(y=unmarked_release_corrected, x=YEAR, col="lightblue"))+
    geom_point(data=Sport_mark_rate_mrr, aes(y=mrr_corrected, x=YEAR, col="lightblue4")) + geom_line(data=Sport_mark_rate_mrr, aes(y=mrr_corrected, x=YEAR, col="lightblue4"))+
    geom_point(data=Sport_mark_rate_mrr, aes(y=Release_rate, x=YEAR, col="lightgreen")) + geom_line(data=Sport_mark_rate_mrr, aes(y=Release_rate, x=YEAR, col="lightgreen"))+
    scale_color_manual(values=c("lightblue", "lightblue4", "lightgreen"),  labels = c("URR", "MRR", "RR"))+
    theme(legend.position="bottom")+
    ylab("Proportion")+
    facet_wrap(~finescale_fishery)+
    theme_bw()+ geom_vline(xintercept = 2012)

  cnr_canada_sport_short<- cnr_canada_sport %>% dplyr::select(-finescale_fishery, -finescale_fishery_old) %>% unique()

  Sport_mark_rate_mrr_sum<-Sport_mark_rate_mrr %>% dplyr::select(-Kept, -Released, -Release_rate) %>% group_by(erafishery, YEAR) %>%   summarise_if(is.numeric, sum, na.rm = TRUE)
  Sport_mark_rate_mrr_sum<-Sport_mark_rate_mrr_sum  %>% left_join(cnr_canada_sport_short)

  rec_summary<-read.csv("rec_summary_2005-2023.csv") %>% as_tibble()
  Sport_mark_rate_mrr_sum<-Sport_mark_rate_mrr_sum  %>% left_join(rec_summary)


  ## Kept only
  ggplot() +
    geom_point(data=Sport_mark_rate_mrr_sum, aes(y=Kept_total, x=YEAR, col="lightblue4")) + geom_line(data=Sport_mark_rate_mrr_sum, aes(y=Kept_total, x=YEAR, col="lightblue4"))+
    geom_point(data=Sport_mark_rate_mrr_sum, aes(y=total_kept, x=YEAR, col="lightgreen")) + geom_line(data=Sport_mark_rate_mrr_sum, aes(y=total_kept, x=YEAR, col="lightgreen"))+
    geom_point(data=Sport_mark_rate_mrr_sum, aes(y=Kept, x=YEAR, col="darkgreen")) + geom_line(data=Sport_mark_rate_mrr_sum, aes(y=Kept, x=YEAR, col="darkgreen"))+

    scale_color_manual(values=c("darkgreen", "lightblue4", "lightgreen"),  labels = c("Kept CNR", "Kept new", "Kept C&E Report"))+
    theme(legend.position="bottom")+
    ylab("Kept Catch")+
    facet_wrap(~erafishery, scales = "free")+
    theme_bw()+ geom_vline(xintercept = 2012)


  ## Released only
  ggplot() +
    geom_point(data=Sport_mark_rate_mrr_sum, aes(y=Released_total, x=YEAR, col="lightblue4")) + geom_line(data=Sport_mark_rate_mrr_sum, aes(y=Released_total, x=YEAR, col="lightblue4"))+
    geom_point(data=Sport_mark_rate_mrr_sum, aes(y=legal_releases, x=YEAR, col="lightgreen")) + geom_line(data=Sport_mark_rate_mrr_sum, aes(y=legal_releases, x=YEAR, col="lightgreen"))+
    geom_point(data=Sport_mark_rate_mrr_sum, aes(y=Released, x=YEAR, col="darkgreen")) + geom_line(data=Sport_mark_rate_mrr_sum, aes(y=Released, x=YEAR, col="darkgreen"))+

   scale_color_manual(values=c("darkgreen", "lightblue4", "lightgreen"),  labels = c("Released CNR", "Released new", "Released C&E Report"))+
    theme(legend.position="bottom")+
    ylab("Released Catch")+
    facet_wrap(~erafishery, scales = "free")+
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




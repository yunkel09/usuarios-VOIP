#   ____________________________________________________________________________
#   Usuarios VOIP Marzo 2016                                                ####


##  ............................................................................
##  Step 0: Packages and options                                            ####

      list.of.packages <- c('data.table',  # funcion 'fread' lectura mas rapida que read_csv de readr
                            'tidyverse',   # dplyr
                            'magrittr',    # operators like (%>%, %<>%, %$%)
                            'scales',      # percent_format
                            'gdata',       # reorder.factor function
                            'stringr')     # function str_replace_all
      
      new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
      if(length(new.packages)) install.packages(new.packages)
      inst <- lapply(c(list.of.packages, new.packages), library, character.only = TRUE, quietly = TRUE)

      # definir opciones      
      options(scipen = 999)                # Evitar notacion cientifica
      

##  ............................................................................
##  Step 1: Custom theme                                                    ####

      mytheme <- theme(panel.background = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       axis.text.x = element_text(size = 8),
                       axis.title.y = element_blank(),
                       panel.border = element_rect(colour = "black", fill = NA))
      
##  ............................................................................
##  Step 2: Load Dataset                                                    ####

      myurl <- 'http://talent.com.gt/IntroR/UsersVoIP.csv'
      var_1 <- as.tibble(fread(input = myurl,
                               data.table = FALSE))
      
      # cargar rda una vez leido.
      # save(var_1, file = 'voip_users_raw_df.rda')
      # load('voip_users_raw_df.rda')
      
            
##  ............................................................................
##  Step 3: Data Carpentry                                                  ####

      colnames(var_1) %<>% tolower
      
##  ............................................................................
##  Step 3: Calculations                                                    ####

            
      # 3.1 caching grouping variables for speeding up the code
      grp.v1      <- var_1 %>% group_by(cst_type)
      
      
      # 3.2 Registros totales
      var_2       <-    nrow(var_1)
      
      # 3.3 Cantidad de usuarios por categoria
      var_3       <-    var_1 %>%
                        count(cst_type, sort = TRUE) %>%
                        rename(users = n) %>%
                        mutate_at('cst_type', as.factor)
      
                        # reordenar factores
                        type_1 <- var_3 %$% reorder.factor(cst_type, users, function(x) sum(x))
                  
                        # graficar
                        ggplot(var_3, aes(x = type_1, y = users)) +
                        geom_bar(stat = "identity", col = "black", fill = "steelblue") +
                        ggtitle("PARETO USUARIOS POR CATEGORIA") +
                        scale_y_continuous(limits = c(0, (max(var_3$users) * 1.1)), labels = comma) +
                        coord_flip() +
                        geom_text(aes(label = comma(users)), size = 3, hjust = -0.2) +
                        mytheme
                  
      # 3.4 Porcentaje de usuarios por categoria
      var_4       <-    grp.v1 %>%
                        summarise(users = n()) %>%
                        mutate(user_proportion = users / sum(users)) %>%
                        arrange(desc(user_proportion))
      
                        # reordenar factores
                        type_2 <- var_4 %$% reorder.factor(cst_type, user_proportion, function(x) sum(x))
                  
                        # graficar
                        ggplot(var_4, aes(x = type_2, y = user_proportion)) +
                        geom_bar(stat = "identity", col = "black", fill = "steelblue") +
                        ggtitle("PORCENTAJES") +
                        scale_y_continuous(limits = c(0, (max(var_4$user_proportion) * 1.1)), labels = percent) +
                        coord_flip() +
                        geom_text(aes(label = percent(user_proportion)), size = 3, hjust = -0.2) +
                        mytheme
                  
      
      # 5. Porcentaje de revenue por categoria de usuario
      var_5       <-    grp.v1 %>%
                        summarise(revenue = sum(revenue_total)) %>%
                        mutate(revenue_proportion = revenue / sum(revenue)) %>%
                        arrange(desc(revenue_proportion))

                        # reordenar factores
                        type_3 <- var_5 %$% reorder.factor(cst_type, revenue_proportion, function(x) sum(x))
                        
                        # graficar
                        ggplot(var_5, aes(x = type_3, y = revenue_proportion)) +
                        geom_bar(stat = "identity", col = "black", fill = "steelblue") +
                        ggtitle("PORCENTAJES REVENUE") +
                        scale_y_continuous(limits = c(0, (max(var_5$revenue_proportion) * 1.1)), labels = percent) +
                        coord_flip() +
                        geom_text(aes(label = percent(revenue_proportion)), size = 3, hjust = -0.2) +
                        mytheme

                              
      # 6. Porcentaje de revenue broadband que por categoria de usuario
      var_6       <-    grp.v1 %>%
                        summarise(revenue = sum(revenue_inf)) %>%
                        mutate(revenue_inf_proportion = revenue / sum(revenue))%>%
                        arrange(desc(revenue_inf_proportion))
      
                        # reordenar factores
                        type_4 <- var_6 %$% reorder.factor(cst_type, revenue_inf_proportion, function(x) sum(x))
                        
                        # graficar
                        ggplot(var_6, aes(x = type_4, y = revenue_inf_proportion)) +
                              geom_bar(stat = "identity", col = "black", fill = "steelblue") +
                              ggtitle("REVENUE BROADBAND") +
                              scale_y_continuous(limits = c(0, (max(var_6$revenue_inf_proportion) * 1.1)), labels = percent) +
                              coord_flip() +
                              geom_text(aes(label = percent(revenue_inf_proportion)), size = 3, hjust = -0.2) +
                              mytheme
      
      
      # 7. Construir un df con los puntos anteriores
      refcols     <-    c('cst_type',
                          'users',                    # cantidad de usuarios
                          'users_proportion',
                          'revenue_proportion',
                          'revenue_inf_proportion')
      
      var_7       <-    var_1 %>%
                        select(cst_type, revenue_total, revenue_inf) %>%
                        group_by(cst_type) %>%
                        summarise(users = n(),
                                  revenue = sum(revenue_total),
                                  revenue_broadband = sum(revenue_inf)) %>%
                        mutate(users_proportion = percent(users / sum(users)),
                               revenue_proportion = percent(revenue / sum(revenue)),
                               revenue_inf_proportion = percent(revenue_broadband  / sum(revenue_broadband ))) %>%
                        select(-revenue) %>%
                        arrange(desc(users)) %>%
                        do(.[refcols])
      
      
      # 8. Porcentaje del revenue por tipo de dispositivo
      var_8       <-    var_1 %>%
                        select(revenue_inf, dvc_tp_nm) %>%
                        group_by(dvc_tp_nm) %>%
                        summarise(revenue_broadband = sum(revenue_inf)) %>%
                        mutate(revenue_inf_proportion = percent(revenue_broadband / sum(revenue_broadband))) %>%
                        arrange(desc(as.numeric(sub("%", "", revenue_inf_proportion))))
            
      # 9. Agregar Revenue outgoing de cada usuario (REVENUE_TOTAL - REVENUE_INC_TOTAL)
      var_1       %<>%  mutate(revenue_outgoing = revenue_total - revenue_inc_total)
      

      # 10. ARPU outgoing total
      var_10      <-    round(sum(var_1$revenue_outgoing)/n_distinct(var_1$msisdn_dd),2)
      
      
      # 11. Determine el ARPU outgoing para los siguientes tipos de usuarios
      #' Para calcular el ARPU se utiliza la variable msisdn, sin embargo, esta tiene un duplicado
      #' por lo que utilizamos n_distinct.  La diferencia de ese registros son Q 180 en el revenue
      #' total. 
      
      # usuarios VOIP
      var_11a     <-    var_1 %>%
                        select(revenue_outgoing, msisdn_dd, uservoip_max) %>%
                        filter(uservoip_max == 1) %>%
                        summarise(arpu_uservoip = sum(revenue_outgoing) / n_distinct(msisdn_dd)) %>%
                        arrange(desc(arpu_uservoip)) %>%
                        mutate_at('arpu_uservoip', round, digits = 2)
                              
      # usuarios whatsapp      
      var_11b     <-    var_1 %>%
                        select(revenue_outgoing, msisdn_dd, whatsappvoip_max) %>%
                        filter(whatsappvoip_max == 1) %>%
                        summarise(arpu_whatsappvoip = sum(revenue_outgoing) / n_distinct(msisdn_dd)) %>%
                        arrange(desc(arpu_whatsappvoip)) %>%
                        mutate_at('arpu_whatsappvoip', round, digits = 2)
                  
      # usuarios con trafico de datos, pero sin trafico VOIP
      # datauser_novoip
            
      var_11c     <-    var_1 %>%
                        select(uservoip_max, msisdn_dd, mb_totales, voip_trffc_sum, revenue_outgoing) %>%
                        filter(mb_totales > 0,
                              voip_trffc_sum == 0) %>%
                        summarise(arpu_datauser_novoip = round(sum(revenue_outgoing) / n_distinct(msisdn_dd), 2))
                        
      # usuarios sin trafico de datos
      var_11d     <-    var_1 %>%
                        filter(mb_totales == 0) %>%
                        summarise(arpu_users_with_no_data_trffc = round(sum(revenue_outgoing) / n_distinct(msisdn_dd), 2))
                        
      # usuarios con trafico entrante internacional      
      var_11e     <-    var_1 %>%
                        filter(user_inc_intl > 0) %>%
                        summarise(arpu_users_with_intl_trffc = round(sum(revenue_outgoing) / n_distinct(msisdn_dd), 2))
      
      
      var_11      <-    as.data.frame(list(var_11a, var_11b, var_11c, var_11d, var_11e)) %>%
                        gather(key = 'tipo', value = 'arpu_qtz') %>%
                        mutate_at(.vars = 'tipo', .funs = str_replace_all, 'arpu_', "") %>%
                        arrange(desc(arpu_qtz))
                  
      

      # 12. ARPU outgoing por tipo de dispositivo
      var_12      <-    var_1 %>%
                        group_by(dvc_tp_nm) %>%
                        summarise(arpu_by_dvc_tp = sum(revenue_outgoing) / n_distinct(msisdn_dd)) %>%
                        arrange(desc(arpu_by_dvc_tp)) %>%
                        mutate_at('arpu_by_dvc_tp', round, digits = 2)
      

##  ............................................................................
##  Guardar workspace                                                       ####

      rm(var_1, pkgs, inst, new.packages, list.of.packages)
      save.image(file = 'workspace_ejercicio_1.RData')
            
            
            
            
            
            
            
        
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
               
      
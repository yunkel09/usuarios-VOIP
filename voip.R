#   ____________________________________________________________________________
#   Usuarios VOIP Marzo 2016                                                ####


##  ............................................................................
##  Step 0: Packages and options                                            ####

      pkgs <- c('dplyr',
                'data.table',     # fread 
                'scales',
                'magrittr')

      lapply(pkgs, library, character.only = TRUE)
      options(scipen = 999)
      
##  ............................................................................
##  Step 1: Load Dataset                                                   ####

      myurl <- 'http://talent.com.gt/IntroR/UsersVoIP.csv'
      var_1 <- as.tibble(fread(input = myurl,
                               data.table = FALSE))
      save('voip_users_raw_df.rda')
      load('voip_users_raw_df.rda')
      
##  ............................................................................
##  Step 2: Data Carpentry                                                  ####

      colnames(var_1) %<>% tolower
      
##  ............................................................................
##  Step 3: Calculations                                                    ####

            
      # 2. Registros totales
      var_2 <-    nrow(var_1)
      
      # 3. Cantidad de usuarios por categoria
      var_3 <-    var_1 %>%
                  count(cst_type, sort = TRUE) %>%
                  rename(users = 'n')
      
      # 4. Porcentaje de usuarios por categoria
      var_4 <-    var_1 %>%
                  group_by(cst_type) %>%
                  summarise(users = n()) %>%
                  mutate(user_proportion = percent(users / sum(users))) %>%
                  arrange(desc(as.numeric(sub("%", "", user_proportion))))
      
     
      # 5. Porcentaje de revenue por categoria de usuario
      var_5 <-    var_1 %>%
                  group_by(cst_type) %>%
                  summarise(revenue = sum(revenue_total)) %>%
                  mutate(porcentaje_revenue = percent(revenue / sum(revenue))) %>%
                  arrange(desc(as.numeric(sub("%", "", porcentaje_revenue))))
      
      # 6. Porcentaje de revenue broadband que por categoria de usuario
      var_6 <-    var_1 %>%
                  group_by(cst_type) %>%
                  summarise(revenue = sum(revenue_inf)) %>%
                  mutate(porcentaje_revenue_broadband = percent(revenue / sum(revenue)))%>%
                  arrange(desc(as.numeric(sub("%", "", porcentaje_revenue_broadband))))
      
      # 7. Construir un df con los puntos anteriores
      refcols <- c('cst_type',
                   'users',                    # cantidad de usuarios
                   'users_proportion',
                   'revenue_proportion',
                   'revenue_inf_proportion')
      
      var_7 <-    var_1 %>%
                  select(cst_type, revenue_total, revenue_inf) %>%
                  group_by(cst_type) %>%
                  summarise(users = n(),
                            revenue = sum(revenue_total),
                            revenue_broadband = sum(revenue_inf)) %>%
                  mutate(users_proportion = percent(users / sum(users)),
                         revenue_proportion = percent(revenue / sum(revenue)),
                         revenue_inf_proportion = percent(revenue_broadband  / sum(revenue_broadband ))) %>%
                  select(-revenue) %>%
                  arrange(desc(users))
      
      var_7 <-    var_7[refcols]                # reordenar columnas
                  
      
      # 8. Porcentaje del revenue por tipo de dispositivo
      var_8 <-    var_1 %>%
                  select(revenue_inf, dvc_tp_nm) %>%
                  group_by(dvc_tp_nm) %>%
                  summarise(revenue_broadband = sum(revenue_inf)) %>%
                  mutate(revenue_inf_proportion = percent(revenue_broadband / sum(revenue_broadband))) %>%
                  arrange(desc(as.numeric(sub("%", "", revenue_inf_proportion))))
            
      # 9. Agregar Revenue outgoing de cada usuario (REVENUE_TOTAL - REVENUE_INC_TOTAL)
      var_9 <-    var_1 %>%
                  mutate(revenue_outgoing = revenue_total - revenue_inc_total)
      

      # 10. ARPU outgoing total
      var_10 <-   dollar(sum(var_9$revenue_outgoing)/n_distinct(var_9$msisdn_dd))
      
      
      # 11. Determine el ARPU outgoing para los siguientes tipos de usuarios
      #' Para calcular el ARPU se utiliza la variable msisdn, sin embargo, esta tiene un duplicado
      #' por lo que utilizamos n_distinct.  La diferencia de ese registros son Q 180 en el revenue
      #' total. 
      
      # usuarios VOIP
      var_11a <-  var_9 %>%
                  select(revenue_outgoing, msisdn_dd, uservoip_max) %>%
                  group_by(uservoip_max) %>%
                  summarise(arpu_uservoip = sum(revenue_outgoing) / n_distinct(msisdn_dd)) %>%
                  arrange(desc(arpu_uservoip)) %>%
                  mutate_at('arpu_uservoip', round, digits = 2)
                              
      # usuarios whatsapp      
      var_11b <-  var_9 %>%
                  select(revenue_outgoing, msisdn_dd, whatsappvoip_max) %>%
                  group_by(whatsappvoip_max) %>%
                  summarise(arpu_whatsappvoip = sum(revenue_outgoing) / n_distinct(msisdn_dd)) %>%
                  arrange(desc(arpu_whatsappvoip)) %>%
                  mutate_at('arpu_whatsappvoip', round, digits = 2)
                  
      # usuarios con trafico de datos, pero sin trafico VOIP
      # datauser_novoip
            
      var_11c <-  var_9 %>%
                  select(uservoip_max, msisdn_dd, mb_totales, voip_trffc_sum, revenue_outgoing) %>%
                  filter(mb_totales > 0,
                        voip_trffc_sum == 0) %>%
                  summarise(arpu_datauser_novoip = round(sum(revenue_outgoing) / n_distinct(msisdn_dd), 2))
                        
            
            
            
      # usuarios sin trafico de datos
      var_11d <-  var_9 %>%
                  filter(mb_totales == 0) %>%
                  summarise(arpu_users_no_data_tffc = round(sum(revenue_outgoing) / n_distinct(msisdn_dd), 2))
                        
            
      # usuarios con trafico entrante internacional      
      var_11e <-  var_9 %>%
                  filter(user_inc_intl > 0)
                        

      # 12. ARPU outgoing por tipo de dispositivo
      var_12 <-   var_9 %>%
                  group_by(dvc_tp_nm) %>%
                  summarise(arpu_by_dvc_tp = sum(revenue_outgoing) / n_distinct(msisdn_dd)) %>%
                  arrange(desc(arpu_by_dvc_tp)) %>%
                  mutate_at('arpu_by_dvc_tp', round, digits = 2)
      

##  ............................................................................
##  Guardar workspace                                                       ####

      rm(var_1, var_9)
      save.image(file = 'workspace_ejercicio_1.RData')
            
            
            
            
            
            
            
        
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
               
      
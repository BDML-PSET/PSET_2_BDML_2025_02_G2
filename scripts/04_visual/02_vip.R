### setup
cat("\f")
rm(list = ls())
source("scripts/00_packages.R")

#--------------#
# 1. Load data #
#--------------#

train = import('stores/output/02_wrangle/02_train.rds',setclass = 'tibble')
test = import('stores/output/02_wrangle/02_test.rds',setclass = 'tibble')
model = import("stores/output/03_models/41_xgb_tree_weights_nrounds_500_max_depth_7_eta_0.04_gamma_0_col_sample_0.6_min_n_25_ss_0.8.rds")

#---------------#
# 2. Split data #
#---------------#

set.seed(1234)
df_split = initial_validation_split(data = train,prop = c(0.6,0.2),strata = pobre)
df_train = training(df_split)
df_validation = validation(df_split)
df_test = testing(df_split)

#--------#
# 3. VIP #
#--------#

pfun = function(object,newdata){
  stats::predict(object,newdata = newdata)
}

set.seed(20205)
p1 = vi(object = model,
        method = 'permute',
        train = df_train,
        target = 'pobre',
        metric = yardstick::f_meas_vec,
        pred_wrapper = pfun,
        smaller_is_better = FALSE )

#-----------------# 
# select variabless
#-----------------# 
ac = p1 %>%  
     summarise(sum(Importance)) %>%  pull()


top_vars_10 <- p1 %>%
            arrange(desc(Importance)) %>%
            slice_head(n = 10) 

top_vars_10 = top_vars_10 %>% 
             mutate(vip_ac = round((Importance/ac)*100,2))

top_vars_10 <- top_vars_10 %>%  
              mutate(
                Variable = case_when(
                  Variable == "oc_household_average" ~ "Número de ocupados promedio",
                  Variable == "regimen_salud_1_household_average" ~ "Número de personas afiliadas al régimen contributivo",
                  Variable == "numero_personas_unidad_gasto" ~ "Número de personas por unidad de gasto",
                  Variable == "ocupado_horas_trabajadas_normalmente_ho" ~ "Horas promedio trabajadas por ocupados",
                  Variable == "lp" ~ "Línea de pobreza",
                  Variable == "tipo_propiedad_vivienda_hogar" ~ "Tenencia de la vivienda",
                  Variable == "ocupado_tiempo_en_la_empresa_household_" ~ "Tiempo promedio de ocupación en la empresa",
                  Variable == "numero_personas_hogar" ~ "Número de personas por hogar",
                  Variable == "tipo_propiedad_vivienda_hogar_3" ~ "Vivienda tipo 3",
                  Variable == "ocupado_tiempo_en_la_empresa_household_working" ~ "tiempo promedio de ocupación en la empresa", 
                  Variable == "ocupado_horas_trabajadas_normalmente_household_working" ~ "horas promedio trabajadas por ocupados",
                  TRUE ~ Variable  # mantiene los nombres que no cambian
                )
              )

# gráfica


graph_10 = top_vars_10 %>%
          mutate(Variable = str_wrap(Variable, width = 25)) %>%  # ajusta el ancho
          arrange(desc(vip_ac)) %>%
          ggplot(aes(x = reorder(Variable, vip_ac), y = vip_ac, fill = vip_ac)) +
          geom_col(show.legend = FALSE) +
          geom_text(
            aes(label = round(vip_ac, 2)),
            hjust = -0.1, size = 5, color = "black", fontface = "bold"
          ) +
          scale_fill_gradientn(colors = c("#1591EA", "#4052D6", "#0000FF", "#00008A")) +
          coord_flip() +
          labs(
            title = NULL,
            x = NULL,
            y = "Importancia relativa"
          ) +
          theme_classic(base_size = 16) +
          theme(
            axis.title.x = element_text(size = 15),
            axis.text.x = element_text(size = 13),
            axis.text.y = element_text(size = 12, lineheight = 0.9)
          ) +
          expand_limits(y = max(top_vars_10$vip_ac) * 1.1)

ggsave(filename = "stores/output/04_visual/02_vip_10.png", plot = graph_10, width = 10, height = 6, dpi = 400,  bg = "white")

#------------------------#
# 11. vip all variable   #
#------------------------#

top_gen = p1 %>%  mutate(vip_ac = round((Importance/ac)*100,2))

top_gen <- top_gen %>%
            mutate(categoria = case_when(
                        str_detect(Variable, "household_average") ~ "Características de los miembros del hogar",
                        str_detect(Variable, "household_working") ~ "Ocupados del hogar",
                        str_detect(Variable, "hh") ~ "Características del jefe del hogar",
                        TRUE ~ "Caracteristicas del hogar"))

top_gen = top_gen %>% 
          group_by(categoria) %>%  
          summarise(importance = sum(vip_ac))

graph_gen = top_gen %>%
          arrange(desc(importance)) %>%
          ggplot(aes(x = reorder(categoria, importance), y = importance, fill = categoria)) + 
          geom_col(show.legend = FALSE) +
          geom_text(aes(label = round(importance, 2)),hjust = -0.1, size = 5, color = "black", fontface = "bold") +
          scale_fill_manual(values = c("#0000FF", "#4052D6", "#1591EA", "#00008A")) +
          coord_flip() +
          labs(title = NULL, x = NULL, y = "Importancia acumulada") +
          theme_classic(base_size = 16) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
                axis.title.x = element_text(size = 15),
                axis.text.x = element_text(size = 13),
                axis.text.y = element_text(size = 13)) +
          expand_limits(y = max(top_gen$importance) * 1.1)



ggsave(filename = "stores/output/04_visual/02_vip_gen.png", plot = graph_gen, width = 10, height = 6, dpi = 400,  bg = "white")

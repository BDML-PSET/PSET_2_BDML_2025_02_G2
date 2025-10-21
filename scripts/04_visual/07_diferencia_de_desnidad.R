### setup
cat("\f")
rm(list = ls())
source('scripts/00_packages.R')

#--------------#
# 1. Load data #
#--------------#
data = import('stores/output/01_import/01_train_hogares.rds')

#-----------------#
# 2. prepare data #
#-----------------#
data = data |> 
       mutate(prop = log(ingpcug/lp), 
              depto = ifelse(test = depto ==11, "Bogotá D.C.", "Colombia"))

plot = data |> 
       ggplot(aes(x = prop, fill = depto)) +
       geom_density(alpha = 0.6, color = "white", linewidth = 0.4) +
       scale_fill_manual(values = c("Bogotá D.C." = "#EF476F", "Colombia" = "#00008A")) +
       labs(x = "Log(Ingreso percápita de la unidad de gasto/ línea de pobreza)",y = "Densidad",) +
       geom_vline(linetype = "dashed", xintercept = 0, color = alpha("black", 0.6)) +
       geom_text(inherit.aes = FALSE, aes(x = -0.25, y = 0.42, label = "Línea de pobreza"), hjust = 1, size = 4, color = "gray20") + 
       theme_minimal() +
       theme(legend.position = "top",
             legend.title = element_blank(),
             panel.grid.minor = element_blank(),
             panel.grid.major.x = element_blank(),
             axis.line = element_line(color = "gray60"))

#-----------------#
# 3 export 
#-----------------#
ggsave(plot = plot, filename =  "stores/output/04_visual/07_plot_bogota_ingreso.png", width = 10, height = 6, dpi = 400,  bg = "white")

#drainage_X10302 = Muenchen
#drainage_X10303 = BadToelz
#drainage_X10304 = Mittenwald
#drainage_X10321 = Schlehdorf


setwd("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/kbe")

# load packages
library(zoo)
library(bnlearn)
library(Rgraphviz)
library(ggplot2)
source("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/functions.R")


# load data
data_merged_rol <- readRDS("data_merged_rol.rds")
data_merged_rol$YY <- as.numeric(data_merged_rol$YY)



# THIS IS ONLY FOR VIZUALIZATION
## application
# influence variables united
data_viz <- data_merged_rol
colnames(data_viz)[1] <- "year"
colnames(data_viz)[5:8] <- c("drainage_MU", "drainage_BT", "drainage_MW", "drainage_SD") 
colnames(data_viz)[46:81] <- c("X_MU", "X_BT", "X_MW", "X_SD",
                               "glorad_MU", "glorad_BT", "glorad_MW", "glorad_SD",
                               "grdepth_MU", "grdepth_BT", "grdepth_MW", "grdepth_SD",
                               "precip_MU", "precip_BT", "precip_MW", "precip_SD",
                               "qinfil_MU", "qinfil_BT", "qinfil_MW", "qinfil_SD",
                               "relhum_MU", "relhum_BT", "relhum_MW", "relhum_SD",
                               "snow_MU", "snow_BT", "snow_MW", "snow_SD",
                               "soilroot_MU", "soilroot_BT", "soilroot_MW", "soilroot_SD",
                               "soilunsat_MU", "soilunsat_BT", "soilunsat_MW", "soilunsat_SD")

variables_needed <- c("drainage_MU", "drainage_BT", "drainage_MW", "drainage_SD",
                      "X_MU", "X_BT", "X_MW", "X_SD",
                      "year")

# Building network
network <- empty.graph(variables_needed)

network <- set.arc(network, "X_MU", "drainage_MU")
network <- set.arc(network, "X_BT", "drainage_BT")
network <- set.arc(network, "X_MW", "drainage_MW")
network <- set.arc(network, "X_SD", "drainage_SD")
network <- set.arc(network, "year", "drainage_MU")
network <- set.arc(network, "year", "drainage_BT")
network <- set.arc(network, "year", "drainage_MW")
network <- set.arc(network, "year", "drainage_SD")
network <- set.arc(network, "drainage_MW", "drainage_BT")
network <- set.arc(network, "drainage_BT", "drainage_MU")
network <- set.arc(network, "drainage_SD", "drainage_MU")

# fitting bayesian network for whole data set (parameters are estimated via maximum likelihood (method = "mle-g"))
fitted_network <- bn.fit(network, data = data_viz[, variables_needed])

graphviz.plot(fitted_network, shape = "ellipse")
ggsave("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/BN_graph.pdf", width = 12, height = 9)


## theory
# parents
data_viz$pa_v <- rep(0, nrow(data_viz))
data_viz$v <- rep(0, nrow(data_viz))

variables_needed <- c("pa_v", "v")

network <- empty.graph(variables_needed)
network <- set.arc(network, "pa_v", "v")

fitted_network <- bn.fit(network, data = data_viz[, variables_needed])
pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/theory_BN_pa.pdf", width = 12, height = 9)
graphviz.plot(fitted_network, layout = "circo")
dev.off()


# undirected graph
library(igraph)
library(ggraph)
adjmatrix <- matrix(c(0, 0, 1,
                      0, 0, 1,
                      1, 1, 0), ncol = 3, nrow = 3)
network <- graph_from_adjacency_matrix(adjmatrix, mode = "directed")
layout <- create_layout(network, layout = "tree")
name <- c("C", "B", "A")
ggraph(network, layout) +
  geom_edge_link() +
  geom_node_circle(aes(r = 0.1), fill = "white") +
  geom_node_text(aes(label = name), color = "black", fontface = 2,
                 size = 20) +
  coord_equal() +
  theme_graph() +
  theme_bw() +
  theme(axis.ticks=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(), 
        panel.border=element_blank())
ggsave("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/theory_BN_undirected.pdf", width = 12, height = 9)


# directed graph
data_viz$A <- rep(0, nrow(data_viz))
data_viz$B <- rep(0, nrow(data_viz))
data_viz$C <- rep(0, nrow(data_viz))

variables_needed <- c("A", "B", "C")

network <- empty.graph(variables_needed)
network <- set.arc(network, "A", "C")
network <- set.arc(network, "A", "B")

fitted_network <- bn.fit(network, data = data_viz[, variables_needed])

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/theory_BN_directed.pdf", width = 12, height = 9)
graphviz.plot(fitted_network)
dev.off()


# cyclic graph
adjmatrix <- matrix(c(0, 1, 1,
                      1, 0, 1,
                      1, 1, 0), ncol = 3, nrow = 3)
network <- graph_from_adjacency_matrix(adjmatrix, mode = "undirected")

name <- c("C", "B", "A")
ggraph(network) +
  geom_edge_link() +
  geom_node_circle(aes(r = 0.1), fill = "white") +
  geom_node_text(aes(label = name), color = "black", fontface = 2,
                 size = 20) +
  coord_equal() +
  theme_graph() +
  theme_bw() +
  theme(axis.ticks=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(), 
        panel.border=element_blank())
ggsave("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/theory_BN_cyclic.pdf", width = 12, height = 9)

# acyclic graph
adjmatrix <- matrix(c(0, 0, 1,
                      0, 0, 1,
                      1, 1, 0), ncol = 3, nrow = 3)
network <- graph_from_adjacency_matrix(adjmatrix, mode = "directed")
layout <- create_layout(network, layout = "tree")
name <- c("C", "B", "A")
ggraph(network, layout) +
  geom_edge_link() +
  geom_node_circle(aes(r = 0.1), fill = "white") +
  geom_node_text(aes(label = name), color = "black", fontface = 2,
                 size = 20) +
  coord_equal() +
  theme_graph() +
  theme_bw() +
  theme(axis.ticks=element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(), 
        panel.border=element_blank())
ggsave("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/theory_BN_acyclic.pdf", width = 12, height = 9)


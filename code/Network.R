library(igraphdata)
library(intergraph)
library(network)
library(sna)
library(ergm)
library(texreg)
library(latentnet)
library(blockmodels)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


data("UKfaculty")
UKfaculty_net <- intergraph::asNetwork(UKfaculty)
UKfaculty_net <- ergm::ergm_symmetrize(UKfaculty_net, rule = "weak")

colours <- c("#008744", "#0057e7", "#d62d20", "#ffa700")
pdf("UK_faculty.pdf", width = 10, height = 10)
set.seed(123)
plot(UKfaculty_net,
  vertex.col = colours[UKfaculty_net %v% "Group"],
  vertex.cex = log(log(degree(UKfaculty_net))) -
    min(log(log(degree(UKfaculty_net)))) + 1
)
dev.off()

library(ggpubr)


degree_info <- degree(UKfaculty_net)
degree_info <- factor(degree_info, levels = (0:max(degree_info)))
degree_information <- data.frame(table(degree_info))
# degree_information$Degree <- as.numeric(as.character(degree_information$Degree))
# degree_information$Degree <- factor(degree_information$Degree, levels = (0:max(degree_information$Degree)))
names(degree_information) <- c("Degree", "Freq")

pdf("UK_faculty_degree.pdf", width = 10, height = 10)
ggplot(data = degree_information, aes(x = Degree, y = Freq)) +
  geom_bar(stat = "identity", fill = "black") +
  labs(x = "Degree", y = "Frequency") +
  scale_y_continuous(breaks = c(0, 3, 6, 9, 12)) +
  scale_x_discrete(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 82)) +
  theme_pubr(base_size = 20)
dev.off()


network.density(UKfaculty_net)
selected_nodes <- which(get.vertex.attribute(UKfaculty_net, "Group") == 1)
# Create the subnetwork
network.density(get.inducedSubgraph(UKfaculty_net,
  v = which(get.vertex.attribute(UKfaculty_net, "Group") == 1)
))
network.density(get.inducedSubgraph(UKfaculty_net,
  v = which(get.vertex.attribute(UKfaculty_net, "Group") == 2)
))
network.density(get.inducedSubgraph(UKfaculty_net,
  v = which(get.vertex.attribute(UKfaculty_net, "Group") == 3)
))
network.density(get.inducedSubgraph(UKfaculty_net,
  v = which(get.vertex.attribute(UKfaculty_net, "Group") == 4)
))
# Get Transitivity
gtrans(UKfaculty_net)

# ERGM fit
ergm_model <- ergm(UKfaculty_net ~ edges + nodematch("Group") +
  transitiveties)
indep_model <- ergm(UKfaculty_net ~ edges + nodematch("Group"))
texreg::texreg(list(ergm_model, indep_model))


latex_table <- texreg::texreg(list(ergm_model, indep_model),
  caption = "Results for the UK Faculty network of the ERGM modedl with and without dependence inducing terms,", caption.above = TRUE, stars = numeric(0),
  label = "tbl:ergm_results", leading.zero = FALSE,
  custom.coef.names = c("Edges", "Nodematch Faculty", "Transitive Ties")
)

latex_table


# LSM fit
fit <- ergmm(UKfaculty_net ~ euclidean(d = 2))

latex_table <- texreg::texreg(list(fit),
  caption = "Results for the UK Faculty network of the Latent Space Model.", caption.above = TRUE, stars = numeric(0),
  label = "tbl:lsm_results", leading.zero = FALSE,
  custom.coef.names = c("Edges", "Nodematch Faculty")
)

latex_table


sbm_fit <- BM_bernoulli(
  adj = as.matrix(UKfaculty_net), explore_min = 2,
  explore_max = 4, explore_min = 4,
  membership_type = "SBM_sym"
)
sbm_fit$estimate()

blocks <- apply(
  X = sbm_fit$memberships[[which.max(sbm_fit$ICL)]]$Z, MARGIN = 1,
  FUN = function(x) which.max(x)
)

pdf("UK_faculty_LSM_SBM.pdf", width = 10, height = 10)
plot(fit,
  plot.means = FALSE, main = "", sub = NULL, suppress.center = TRUE,
  plot.vars = FALSE, vertex.col = blocks, suppress.axes = TRUE,
  ylab = "", xlab = "",
  vertex.cex = log(log(degree(UKfaculty_net))) -
    min(log(log(degree(UKfaculty_net)))) + 1, print.formula = FALSE
)
dev.off()

pdf("UK_faculty_SBM.pdf", width = 10, height = 10)
set.seed(123)
plot(UKfaculty_net,
  vertex.col = blocks,
  vertex.cex = log(log(degree(UKfaculty_net))) -
    min(log(log(degree(UKfaculty_net)))) + 1
)
dev.off()

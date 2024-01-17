################################################################################
## Autor: Wellington Santos Souza                                             ##
## Data: 16/01/2024                                                           ##
## Análise de Redes Sociais                                                   ##
################################################################################

# Carregando os pacotes necessários
if(!require(tidyverse)){install.packages("tidyverse")};library(tidyverse) 
if(!require(igraph)){install.packages("igraph")};library(igraph)  
if(!require(tidygraph)){install.packages("tidygraph")};library(tidygraph) 
if(!require(visNetwork)){install.packages("visNetwork")};library(visNetwork)
if(!require(summarytools)){install.packages("summarytools")};library(summarytools)
if(!require(kableExtra)){install.packages("kableExtra")};library(kableExtra)

# Carregando os dados
# relações
arestas <- read_csv("data/arestas.csv") %>% 
  select(Source, Target,Weight) %>% 
  mutate_at(vars(Source, Target), as.character) %>% 
  rename(from=Source, to=Target)

# nos
nos <- read_csv("data/nos.csv") %>% 
  select(Id,Label,timeset) %>% 
  rename(group=timeset,id=Id)
nos$group <- 1

# removendo possiveis relações duplicadas
l <- unique(arestas$from) 
l2 <- unique(arestas$to) 
l3 <- unique(c(l, l2))

# mater somente os nós que estão nas arestas
nosTotal <- filter(nos, id %in% l3) %>% 
  mutate(Id = as.character(id))

nos <- nosTotal

rm(list = setdiff(ls(), c("nos", "arestas")))

# Criando o grafo
nt <- tbl_graph(nodes = nos, edges = arestas, node_key = "id", directed = FALSE)

# transformar relacoes multiplas em peso
nt <- as_tbl_graph(simplify(nt))

# identificando grupos
comm <- cluster_fast_greedy(nt, weights = E(nt)$weight)
nos$membership <- comm$membership

data.frame(Métrica = c("Número de nós",
                       "Número de relacões",
                       "Número de componentes",
                       "Densidade",
                       "Distancia geodésica média",
                       "Diâmetro",
                       "Transitividade global",
                       "Transitividade local",
                       "Número de comunidades",
                       "Modularidade",
                       "Assortatividade"
),
Valor = c(round(gorder(nt),2),
          round(gsize(nt),2),
          round(components(nt)$no,2),
          round(edge_density(nt),2),
          round(mean_distance(nt),2),
          round(diameter(nt),2),
          round(transitivity(nt, type = "global"),2),
          round(transitivity(nt, type = "localaverage"),2),
          round(length(comm),2),
          round(modularity(comm),2),
          round(assortativity_nominal(nt, comm$membership),2)
)
)%>%
  kbl(caption = "Métricas da Rede") %>%
  kable_classic(full_width = F, html_font = "Cambria") 

# Centralidade de grau
nos$grau <- degree(nt)

# grau de intermediação
nos$intermediacao <- betweenness(nt)

# grau de proximidade
nos$proximidade <- closeness(nt, normalized = TRUE)

# Excentricidade
nos$excentricidade <- eccentricity(nt)

# Centralidade de autovetor
nos$autovetor <- eigen_centrality(nt)$vector

# Criando tabela com métricas
(metricas_ind <- nos %>%
    select(grau, intermediacao, proximidade, excentricidade, autovetor, transitividade) %>%
    descr(stats = c("min", "q1", "med", "mean", "sd", "q3", "max"), transpose = TRUE) %>% 
    round(., digits = 2))

# Gerando a rede

nos_new <- nos
nos_new$value <- nos_new$grau 
nos_new$label <- 
  nos_new$group <- comm$membership 
nos_new$title <- paste0("Músico: ", nos_new$Label,
                        "</b><br>comunidade: ", nos_new$membership,
                        "</b><br>grau: ", nos_new$grau)

arestas_new <- arestas
arestas_new$width <- arestas_new$Weight
arestas_new$title <- paste0("peso: ", arestas_new$Weight)

visNetwork(nos_new, arestas_new, background = "white") %>%
  visIgraphLayout(layout = "layout_nicely", randomSeed = 1) %>% 
  visGroups() %>% 
  visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T),
             selectedBy = list(variable = "Label", style = c(150, 26)))







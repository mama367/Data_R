setwd("C:/Users/mama3/Downloads")

source("setup.R")	
#' 	
#' 	
#' **중요한 윤리적 고려사항:**	
#' - 항상 사이트의 `robots.txt`와 이용약관을 확인	
#' - 서버에 과부하를 주지 않도록 요청 속도 제한	
#' - 가능한 경우 API 사용을 우선 고려	
#' 	
#' ### 기본 설정	
#' 	
#' 	
# 필요한 패키지 설치 및 로드	
install.packages(c("rvest", "sf", "rnaturalearth", "rnaturalearthdata", "countrycode"))

library(rvest)        # 웹스크래핑	
library(dplyr)        # 데이터 조작	
library(stringr)      # 문자열 처리	
library(ggplot2)      # 시각화	
library(quanteda)     # 텍스트 분석	
library(stm)          # 토픽 모델링	
library(igraph)     # 네트워크 분석	

#' 	
#' # 1: 백악관 브리핑 데이터 수집 및 텍스트 분석	
#' 	
#' ### 2.1 단일 페이지 스크래핑	
#' ![White House Press Release](whitehouse.png)	
#' ![White House Press Release (Source)](whitehousesource.png)	
#' 	
# 백악관 브리핑 단일 페이지 스크래핑	
url <- "https://www.whitehouse.gov/briefings-statements/2025/06/presidential-message-on-national-mens-health-week-2025/"	

# HTML 읽기	
page <- read_html(url)	

# 제목, 날짜, 본문 추출	
title <- page %>% 	
  html_node("h1") %>% 	
  html_text(trim = TRUE)	

date <- page %>% 	
  html_node("time") %>% 	
  html_attr("datetime")	

# 본문 텍스트 추출 (단락별로)	
paragraphs <- page %>% 	
  html_nodes("p") %>% 	
  html_text(trim = TRUE)	

# 전체 텍스트로 결합	
full_text <- paste(paragraphs, collapse = "\n\n")	

# 결과 확인	
cat("제목:", title, "\n")	
cat("날짜:", date, "\n")	
cat("본문 길이:", nchar(full_text), "자\n")	
head(paragraphs)	
#' 	
#' 	
#' 	
#' ### 다중 페이지 스크래핑	
#' ![White House Press Release (multiple pages)](whitehousemulti.png)	
#' 	
# 백악관 브리핑 전체 페이지 수집	
base_url <- "https://www.whitehouse.gov/briefings-statements/"	

# Step 1: 마지막 페이지 번호 추출	
page0 <- read_html(base_url)	
last_page <- page0 %>%	
  html_nodes(".page-numbers") %>%	
  html_text(trim = TRUE) %>%	
  as.integer() %>%	
  max(na.rm = TRUE)	

cat("총 페이지 수:", last_page, "\n")	

# Step 2: 모든 페이지에서 링크 수집	
all_links <- list()	
for (i in 1:min(5, last_page)) {  # 예시로 첫 5페이지만	
  cat("페이지", i, "처리 중...\n")	
  page_url <- paste0(base_url, "page/", i, "/")	
  
  page <- tryCatch(read_html(page_url), error = function(e) NULL)	
  if (is.null(page)) next	
  
  links <- page %>%	
    html_nodes("h2 a") %>% 	
    html_attr("href")	
  
  all_links <- append(all_links, links)	
  
  # 서버 과부하 방지를 위한 지연	
  Sys.sleep(1)	
}	

all_links <- unique(all_links)	
head(all_links)	
cat("수집된 링크 수:", length(all_links), "\n")	
#' 	
#' 	
#' ### 개별 기사 내용 수집	
#' 	
#' 	
# Step 3: 각 기사에서 제목, 날짜, 본문 수집	
collect_article <- function(link) {	
  tryCatch({	
    article_page <- read_html(link)	
    
    title <- article_page %>% 	
      html_node("h1") %>% 	
      html_text(trim = TRUE)	
    
    date <- article_page %>% 	
      html_node("time") %>% 	
      html_attr("datetime")	
    
    text <- article_page %>% 	
      html_nodes("p") %>% 	
      html_text(trim = TRUE) %>% 	
      paste(collapse = "\n\n")	
    
    return(data.frame(	
      title = title %||% NA,	
      date = date %||% NA,	
      url = link,	
      text = text %||% NA,	
      stringsAsFactors = FALSE	
    ))	
  }, error = function(e) {	
    return(data.frame(	
      title = NA, date = NA, url = link, text = NA,	
      stringsAsFactors = FALSE	
    ))	
  })	
}	

# 샘플 데이터 수집 (처음 10개 링크만)	
sample_links <- head(all_links, 10)	
results <- lapply(sample_links, collect_article)	

# 데이터프레임으로 결합	
final_df <- do.call(rbind, results)	
final_df <- final_df[complete.cases(final_df), ]  # NA 제거	

cat("수집된 기사 수:", nrow(final_df), "\n")	
head(final_df)	
#' 	
#' 	
#' ### 텍스트 분석	
#' 	
#' 	
# 텍스트 전처리 및 기본 분석	
library(quanteda)	
library(quanteda.textstats)	
library(quanteda.textplots)	

# 코퍼스 생성	
corp <- corpus(final_df$text)	
docvars(corp, "title") <- final_df$title	
docvars(corp, "date") <- as.Date(final_df$date)	

# 토큰화 및 전처리	
toks <- tokens(corp, 	
               remove_punct = TRUE, 	
               remove_numbers = TRUE) %>%	
  tokens_tolower() %>%	
  tokens_remove(stopwords("en"))	

# DFM 생성	
dfmat <- dfm(toks)	

cat("문서 수:", ndoc(dfmat), "\n")	
cat("고유 단어 수:", nfeat(dfmat), "\n")	

# 최빈 단어 확인	
top_words <- topfeatures(dfmat, 20)	
print(top_words)	

# 워드클라우드	
textplot_wordcloud(dfmat, max_words = 50, 	
                   color = RColorBrewer::brewer.pal(8, "Dark2"))	
#' 	
#' 	
#' ### 토픽 모델링	
#' 	
#' 	
# STM을 이용한 토픽 모델링	
library(stm)	

# 텍스트 전처리 (STM 형식)	
processed <- textProcessor(documents = final_df$text, 	
                           metadata = final_df,	
                           lowercase = TRUE, 	
                           removestopwords = TRUE, 	
                           removenumbers = TRUE,	
                           removepunct = TRUE)	

out <- prepDocuments(processed$documents, 	
                     processed$vocab, 	
                     processed$meta)	

# 토픽 모델 적합	
set.seed(100)	
stm_model <- stm(documents = out$documents, 	
                 vocab = out$vocab, 	
                 K = 5,  # 5개 토픽	
                 data = out$meta, 	
                 seed = 100)	

# 토픽 라벨링	
topic_labels <- labelTopics(stm_model, n = 7)	
print(topic_labels)	

# 토픽 시각화	
plot(stm_model, type = "summary")	
#' 	
#' 	
#' 	
#' ## 실습 3: 소셜 미디어 데이터와 네트워크 분석	
#' 	
#' ### 가상의 소셜 미디어 네트워크 생성	
#' 	
#' 	
# 실제 소셜 미디어 스크래핑은 API 제한이 있으므로 	
# 시뮬레이션 데이터로 네트워크 분석 실습	

library(igraph)	

# 정치인-시민 관계 시뮬레이션 (이분 네트워크)	
set.seed(123)	

# 정치인 리스트 (백악관 브리핑에서 언급된 인물들 기반)	
politicians <- c("President", "Vice_President", "Secretary_State", 	
                 "Secretary_Defense", "Press_Secretary", "Chief_of_Staff")	

# 시민 그룹 (관심사별)	
citizens <- paste0("Citizen_", 1:50)	

# 관계 생성 (정치인-시민 팔로우 관계)	
create_bipartite_network <- function(politicians, citizens, prob = 0.3) {	
  edges <- expand.grid(citizens, politicians) %>%	
    filter(runif(n()) < prob) %>%	
    rename(from = Var1, to = Var2)	
  
  return(edges)	
}	

political_edges <- create_bipartite_network(politicians, citizens, 0.2)	

# 이분 그래프 생성	
g_bipartite <- graph_from_data_frame(political_edges, directed = FALSE)	
V(g_bipartite)$type <- V(g_bipartite)$name %in% politicians	

# 이분 네트워크 시각화	
plot(g_bipartite, 	
     layout = layout_as_bipartite,	
     vertex.color = c("lightblue", "pink")[V(g_bipartite)$type + 1],	
     vertex.size = c(3, 8)[V(g_bipartite)$type + 1],	
     vertex.label = ifelse(V(g_bipartite)$type, V(g_bipartite)$name, ""),	
     edge.color = "gray70",	
     main = "정치인-시민 이분 네트워크")	
#' 	
#' 	
#' ### 네트워크 투영 및 분석	
#' 	
#' 	
# 이분 네트워크 투영	
projections <- bipartite_projection(g_bipartite)	
politician_network <- projections[[2]]  # 정치인 간 네트워크	
citizen_network <- projections[[1]]     # 시민 간 네트워크	

# 정치인 네트워크 분석	
cat("정치인 네트워크 기본 정보:\n")	
cat("노드 수:", vcount(politician_network), "\n")	
cat("엣지 수:", ecount(politician_network), "\n")	
cat("밀도:", edge_density(politician_network), "\n")	

# 중심성 분석	
politician_centrality <- data.frame(	
  politician = V(politician_network)$name,	
  degree = degree(politician_network),	
  betweenness = betweenness(politician_network),	
  closeness = closeness(politician_network),	
  eigenvector = eigen_centrality(politician_network)$vector	
)	

print(politician_centrality)	

# 네트워크 시각화	
set.seed(42)	
plot(politician_network,	
     vertex.size = degree(politician_network) * 3,	
     vertex.label.cex = 0.8,	
     vertex.color = "lightcoral",	
     edge.width = 2,	
     layout = layout_with_fr,	
     main = "정치인 간 네트워크 (공통 팔로워 기반)")	
#' 	
#' 	
#' ### 텍스트-네트워크 결합 분석	
#' 	
#' 	
# 백악관 브리핑 텍스트에서 언급된 인물 간 공출현 네트워크	
library(quanteda)	

# 정치인/기관명 사전	
political_terms <- c("president", "vice president", "secretary", "congress", 	
                     "senate", "house", "administration", "cabinet", 	
                     "department", "agency", "director", "administrator")	

# 텍스트에서 정치 용어 추출	
extract_political_terms <- function(text) {	
  tokens <- tokens(text, remove_punct = TRUE) %>%	
    tokens_tolower()	
  
  # 정치 용어가 포함된 토큰 추출	
  political_tokens <- tokens_select(tokens, 	
                                    pattern = political_terms, 	
                                    valuetype = "glob")	
  return(political_tokens)	
}	

# 각 문서에서 정치 용어 추출	
doc_political_terms <- lapply(final_df$text, extract_political_terms)	

# 동시 출현 행렬 생성	
create_cooccurrence_network <- function(term_lists, min_cooccur = 2) {	
  # 모든 고유 용어 수집	
  all_terms <- unique(unlist(term_lists))	
  
  # 동시 출현 행렬 초기화	
  cooc_matrix <- matrix(0, 	
                        nrow = length(all_terms), 	
                        ncol = length(all_terms),	
                        dimnames = list(all_terms, all_terms))	
  
  # 각 문서에서 동시 출현 계산	
  for (terms in term_lists) {	
    if (length(terms) > 1) {	
      term_combinations <- combn(terms, 2)	# original code: "combn(unique(terms), 2)" -> Error in combn(unique(terms), 2) : n < m
      for (i in 1:ncol(term_combinations)) {	
        term1 <- term_combinations[1, i]	
        term2 <- term_combinations[2, i]	
        cooc_matrix[term1, term2] <- cooc_matrix[term1, term2] + 1	
        cooc_matrix[term2, term1] <- cooc_matrix[term2, term1] + 1	
      }	
    }	
  }	
  
  # 최소 동시 출현 빈도 필터링	
  cooc_matrix[cooc_matrix < min_cooccur] <- 0	
  
  return(cooc_matrix)	
}	

# 정치 용어 동시 출현 네트워크 생성	
term_lists <- lapply(doc_political_terms, function(x) as.character(x[[1]]))	
cooc_matrix <- create_cooccurrence_network(term_lists, min_cooccur = 1)	

# 네트워크 그래프 생성	
political_cooc_network <- graph_from_adjacency_matrix(cooc_matrix, 	
                                                      mode = "undirected", 	
                                                      weighted = TRUE,	
                                                      diag = FALSE)	

# 네트워크 시각화	
# 라벨을 명시적으로 설정	
plot(political_cooc_network,	
     vertex.size = 20,                           # 고정 크기로 테스트	
     vertex.label = V(political_cooc_network)$name,  # 명시적 라벨 설정	
     vertex.label.cex = 1,                       # 매우 크게	
     vertex.label.color = "red",                 # 빨간색으로 눈에 띄게	
     vertex.label.font = 2,                      # 굵게	
     vertex.label.dist = 1,                      # 노드 바깥쪽에	
     vertex.color = "lightblue",	
     edge.width = 2,	
     layout = layout_with_fr,	
     main = "정치 용어 공동출현 네트워크")	
#' 	
#' 	
#' 	
#' 	
#' ### 시계열 분석 (날짜 데이터 활용)	
#' 	
#' 	
# 백악관 브리핑 발행 패턴 분석	
if ("date" %in% names(final_df) && !all(is.na(final_df$date))) {	
  
  # 날짜별 브리핑 수	
  daily_briefings <- final_df %>%	
    mutate(date = as.Date(date)) %>%	
    filter(!is.na(date)) %>%	
    count(date) %>%	
    arrange(date)	
  
  # 시계열 플롯	
  ggplot(daily_briefings, aes(x = date, y = n)) +	
    geom_line(color = "blue", size = 1) +	
    geom_point(color = "red", size = 2) +	
    labs(title = "백악관 브리핑 발행 패턴",	
         x = "날짜",	
         y = "브리핑 수") +	
    theme_minimal() +	
    theme(axis.text.x = element_text(angle = 45, hjust = 1))	
}	
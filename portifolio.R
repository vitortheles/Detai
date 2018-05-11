otimizarCarteira<-function(ativosEscolhidos, dataInical, risco, retorno){
  
  #########################################
  #
  #     ETAPA 1 - Análise dos títulos
  #
  #########################################
  
  # Buscar sigla dos ativos
  Symbols = c()
  for(i in 1:length(ativosEscolhidos)){
   y <- which(ativosDisponiveis[]==ativosEscolhidos[i], arr.ind = TRUE)
   x <- ativosDisponiveis[y[1],1] 
   Symbols = as.vector(rbind(Symbols,x))
  }
  

  # Buscar histórico dos ativos na Yahoo Finance
  stock_prices  <- tq_get(Symbols, get = "stock.prices", from = dataInical)
  stock_prices=stock_prices%>%select(symbol,date,adjusted)%>%rename(Stock=symbol,Price=adjusted) 
  stock_prices=stock_prices[!anyDuplicated(stock_prices), ] #Remover linhas duplicadas
  
  # Trocando as linhas pelas colunas, ou seja, mostrando os valores dos ativos por dada em cada data e buscando os valores diários de cada dia
  stock_prices2=stock_prices%>%spread(Stock,Price) 
  stock_prices2 <- Symbols %>%
    tq_get(get  = "stock.prices",
           from = dataInical,
           to   = today()) %>%
    group_by(symbol) 
  
  # Calcular o retorno diário do ativo comparando seu valor dia a dia 
  stock_returns <- stock_prices2 %>%
    tq_transmute(select     = adjusted,
                 mutate_fun = periodReturn,
                 period     = "daily",
                 type       = "arithmetic",
                 col_rename = "returns") %>%
    spread(key = symbol, value = returns)

  # Agrupando os valores diáros por ativo
  stock_prices_daily <- stock_prices2 %>%
    group_by(symbol)
  
  # Converte os valores de retorno diário para um valor simplificado
  stock_returns=stock_returns%>%tk_xts()
  
  # Média do retorno e covariancia da matriz
  returns.portfolio<-na.omit(stock_returns)
  meanReturns <- colMeans(returns.portfolio)
  covMat <- cov(returns.portfolio)
  
  
  
  #########################################
  #
  #     ETAPA 2 - Análise de carteiras
  #
  #########################################
  
  
  # Criar portifólio vazio dos ativos inseridos
  port <- portfolio.spec(assets = colnames(stock_returns))
  port <- add.constraint(port, type = "box", min_sum=0.99, max_sum=1.01)
  port <- add.constraint(portfolio = port, type = "full_investment")
  
  # Gerar portifólio aletário dos ativos
  rportfolios <- random_portfolios(port, permutations = 5000, rp_method = "sample")
  
  # Inserir objetivo 'risco mínimo' e otimizar o portifólio aleatório
  minvar.port <- add.objective(port, type = "risk", name = "var")
  minvar.opt <- optimize.portfolio(stock_returns, minvar.port, optimize_method = "random", rp = rportfolios)
  
  # Inserir objetivo 'máximo retorno' e otimizar o portifólio aleatório
  maxret.port <- add.objective(port, type = "return", name = "mean")
  maxret.opt <- optimize.portfolio(stock_returns, maxret.port, optimize_method = "random", rp = rportfolios)
  

  #########################################
  #
  #     ETAPA 3 - Seleção de carteiras
  #
  #########################################
  
  # Criar novo portifólio dos ativos inseridos
  fund.names <- colnames(stock_returns)
  portilioOtimo <- portfolio.spec(assets=fund.names)
  
  
  # Adicionar objetivos à carteira
  # Minimizar o risco de acordo com o usuário
  portilioOtimo <- add.objective(portfolio=portilioOtimo,
                         type='risk',
                         name='ETL',
                         arguments=list(p=risco/100))
  
  # Adicionar o risco da carteira e risco máximo que cada ativo pode procionar sendo igual a metade do peso inical
  portilioOtimo <- add.objective(portfolio=portilioOtimo, type="risk_budget", name="ETL",
                         arguments=list(p=risco/100), max_prisk=length(ativosEscolhidos)/200) 
  
  portilioOtimo <- add.objective(portfolio=portilioOtimo, type="weight_concentration",
                         name="HHI", conc_aversion=0.1)
  
  portilioOtimo <- add.constraint(portfolio = portilioOtimo, type = "full_investment")
  
  # Soma dos ativos deve ser igual à 100%
  portilioOtimo <- add.constraint(portfolio=portilioOtimo, type="box", min=retorno[1]/100, max=retorno[2]/100)
  
  #############################################################################
  #
  #     ETAPA 4 - Utilizando GLPK para otimizar o resultado
  #
  #     # Pacotes GLPK ("DEoptim","ROI","ROI.plugin.glpk","ROI.plugin.quadprog")
  #
  #############################################################################

  
  # Inserir objetivo carteira com máximo retorno
  qu <- add.objective(portfolio=portilioOtimo, type="return", name="mean")
  
  # Inserir objetivo carteira com minimo risco
  qu <- add.objective(portfolio=qu, type="risk", name="var", risk_aversion=risco/100)

  # Obter carteira ótima entre os objetivos de menor risco e máximo retorno
  opt_qu <- optimize.portfolio(R=stock_returns, portfolio=qu,
                               optimize_method="ROI",
                               trace=TRUE)
  

  # Montar tabela com nome e valores dos ativos para mostra na tela
  carteiraOtimizada = c()
  for(i in 1:length(fund.names)){
    y <- which(ativosDisponiveis[]==fund.names[i], arr.ind = TRUE)
    x <- ativosDisponiveis[y[1],2] 
    carteiraOtimizada = as.vector(rbind(carteiraOtimizada,x))
  }
  
  # Criar um vetor com os nomes dos ativos e os valores de cada um
  carteira<-as.vector(rbind(opt_qu$weights))
  carteiraOtimizada[,2]<-round(carteira[] * 100, 3)
  colnames(carteiraOtimizada)<-c("Ativos", "Participação (%)")
  carteiraOtimizada<-carteiraOtimizada[order(carteiraOtimizada$`Participação (%)`, decreasing=TRUE), ]
  
  # Retornar resultado
  carteiraOtimizada
}

### Gerar lista de ativos para mostrar na tela
gerarAtivos<-function(){
  #amex<-tq_exchange("AMEX")
  #nasdaq<-tq_exchange("NASDAQ")
  #nyse<-tq_exchange("NYSE")
  ativosGerais<-as.vector(rbind(nyse[,1:2],amex[,1:2],nasdaq[,1:2]))
  ativosGerais=ativosGerais[!duplicated(ativosGerais), ]
  ativosGerais
}

### Mostrar a barra de carregando na tela enquanto executa a função
compute_data <- function(updateProgress = NULL) {
  dat <- data.frame(x = numeric(0), y = numeric(0))
  for (i in 1:10) {
    Sys.sleep(0.15)
    new_row <- data.frame(x = rnorm(1), y = rnorm(1))
    if (is.function(updateProgress)) {
      text <- paste0("x:", round(new_row$x, 2), " y:", round(new_row$y, 2))
      updateProgress(detail = text)
    }
    dat <- rbind(dat)
  }
  dat
}






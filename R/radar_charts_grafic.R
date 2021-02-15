
library(plotly)

fig <- plot_ly(
  type = 'scatterpolar',
  r = c(93,37,95,65,55,75,35,70,5,60,42),
  theta = c('Modelagem de \n Dados',
            'SQL',
            "Linguagem R",
            "Linguagem Python",
            'Análise Estatística',
            'Machine \n Learning', 
            'Web Scraping', 
            'Data Wranglig',
            'Segurança da \nInformação', 
            'Gestão de Projetos',
            "Engenharia de Software"
            ),
  fill = 'toself'
) 
fig <- fig %>%
  #layout(title = " Tecnologia e conhecimentos\n", showlegend = F, axis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,100)
      )
    ),
    showlegend = F
  )




fig

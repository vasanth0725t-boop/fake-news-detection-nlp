library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(tidytext)

data_path <- file.path("data", "news.csv")
if (!file.exists(data_path)) {
  stop("Dataset not found at ", data_path, ". Run generate_data.R from the project folder.")
}

news <- read.csv(data_path, stringsAsFactors = FALSE, check.names = FALSE)
required_columns <- c("News_ID", "Title", "Category", "Source", "Text", "Label")
if (!all(required_columns %in% names(news))) {
  stop("Dataset is missing required columns: ", paste(setdiff(required_columns, names(news)), collapse = ", "))
}

category_order <- c("Politics", "Technology", "Business", "Sports", "Health", "Science")
news$Label <- factor(news$Label, levels = c("REAL", "FAKE"))
news$Category <- factor(news$Category, levels = category_order)
news$Word_Count <- vapply(news$Text, function(text) length(strsplit(trimws(text), "\\s+")[[1]]), integer(1))

stop_words <- tidytext::stop_words$word
tokenize_text <- function(text) {
  words <- unlist(strsplit(tolower(gsub("[^[:alnum:]']", " ", text)), "\\s+"), use.names = FALSE)
  words[nzchar(words) & !words %in% stop_words]
}

train_classifier <- function(data) {
  tokens <- lapply(data$Text, tokenize_text)
  counts <- lapply(c("REAL", "FAKE"), function(label) {
    table(unlist(tokens[data$Label == label], use.names = FALSE))
  })
  names(counts) <- c("REAL", "FAKE")
  vocabulary <- union(names(counts$REAL), names(counts$FAKE))
  totals <- vapply(counts, sum, numeric(1))
  real_counts <- counts$REAL[match(vocabulary, names(counts$REAL))]
  fake_counts <- counts$FAKE[match(vocabulary, names(counts$FAKE))]
  real_counts[is.na(real_counts)] <- 0
  fake_counts[is.na(fake_counts)] <- 0
  signal <- log((real_counts + 1) / (totals[["REAL"]] + length(vocabulary))) -
    log((fake_counts + 1) / (totals[["FAKE"]] + length(vocabulary)))
  names(signal) <- vocabulary
  keywords <- names(sort(abs(signal), decreasing = TRUE))
  list(counts = counts, vocabulary = vocabulary, totals = totals, signal = signal, keywords = keywords)
}

classifier <- train_classifier(news)

classify_text <- function(text) {
  if (!nzchar(trimws(text))) {
    return(list(empty = TRUE, tokens = character(), prediction = NA_character_, confidence = NA_real_,
                keywords = character(), word_count = 0L, character_count = nchar(text),
                sentence_count = 0L))
  }
  tokens <- tokenize_text(text)
  known <- tokens[tokens %in% classifier$vocabulary]
  vocabulary_size <- length(classifier$vocabulary)
  scores <- vapply(c("REAL", "FAKE"), function(label) {
    label_counts <- classifier$counts[[label]]
    observed <- label_counts[match(known, names(label_counts))]
    observed[is.na(observed)] <- 0
    probabilities <- (observed + 1) / (classifier$totals[[label]] + vocabulary_size)
    sum(log(probabilities))
  }, numeric(1))
  prediction <- names(which.max(scores))
  margin <- abs(scores[["REAL"]] - scores[["FAKE"]])
  confidence <- round(50 + 50 * (1 - exp(-margin / 8)))
  signals <- unique(tokens[tokens %in% classifier$keywords])
  if (length(signals)) {
    signals <- signals[order(abs(classifier$signal[signals]), decreasing = TRUE)]
  }
  sentences <- strsplit(trimws(text), "(?<=[.!?])\\s+", perl = TRUE)[[1]]
  list(
    empty = FALSE,
    tokens = tokens,
    prediction = prediction,
    confidence = min(99, confidence),
    keywords = head(signals, 8),
    word_count = length(unlist(strsplit(trimws(text), "\\s+"), use.names = FALSE)),
    character_count = nchar(text),
    sentence_count = if (nzchar(trimws(text))) length(sentences) else 0L
  )
}

category_counts <- news %>% count(Category, .drop = FALSE)
label_counts <- news %>% count(Label, .drop = FALSE)
distribution <- news %>% count(Category, Label, .drop = FALSE)
top_words <- news %>%
  select(News_ID, Text) %>%
  tidytext::unnest_tokens(word, Text) %>%
  anti_join(tidytext::stop_words, by = "word") %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 15)

ui <- page_sidebar(
  title = "Signal / News Intelligence",
  window_title = "Fake News Detection using NLP",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#176b78",
    secondary = "#526579",
    base_font = "Inter, system-ui, sans-serif",
    heading_font = "Manrope, system-ui, sans-serif"
  ),
  sidebar = sidebar(
    width = 285,
    open = "always",
    div(class = "brand-mark", span(class = "brand-icon", "N"), div(
      strong("NEWS / SIGNAL"),
      tags$small("NLP intelligence workspace")
    )),
    tags$hr(),
    p(class = "nav-caption", "WORKSPACE"),
    radioButtons(
      "page", label = NULL,
      choices = c(
        "🏠  Dashboard" = "Dashboard",
        "🔍  News Analyzer" = "News Analyzer",
        "📊  NLP Analytics" = "NLP Analytics",
        "📰  News Dataset" = "News Dataset",
        "ℹ️  About" = "About"
      ),
      selected = "Dashboard"
    ),
    div(class = "sidebar-note",
        span(class = "status-dot"),
        span("DEMO MODE  ·  SYNTHETIC DATA"))
  ),
  tags$head(tags$style(HTML("
    :root { --ink: #172b3a; --muted: #6a7b89; --teal: #176b78; --line: #e5ebef; }
    body { background: #f4f7f9; color: var(--ink); font-family: Inter, sans-serif; }
    .bslib-page-sidebar { min-height: 100vh; }
    .sidebar { background: #fff; border-right: 1px solid var(--line); }
    .brand-mark { display:flex; align-items:center; gap:12px; padding:12px 5px 18px; }
    .brand-mark strong, .brand-mark small { display:block; }
    .brand-mark strong { font:800 13px Manrope,sans-serif; letter-spacing:.08em; }
    .brand-mark small { color:var(--muted); font-size:11px; margin-top:3px; }
    .brand-icon { display:grid; place-items:center; width:42px; height:42px; border-radius:13px;
      color:white; background:linear-gradient(145deg,#176b78,#279ca1); font:800 20px Manrope,sans-serif; }
    .nav-caption { color:#94a1aa; font-size:10px; font-weight:700; letter-spacing:.15em; margin:20px 0 8px; }
    .shiny-options-group { display:flex; flex-direction:column; gap:5px; }
    .shiny-options-group .radio { margin:0; }
    .shiny-options-group label { width:100%; border-radius:10px; padding:10px 12px; color:#546674;
      font-size:13px; font-weight:600; cursor:pointer; }
    .shiny-options-group label:has(input:checked) { background:#eaf4f4; color:#145f6b; }
    .shiny-options-group input { display:none; }
    .sidebar-note { position:sticky; top:calc(100vh - 70px); padding:12px 6px; color:#81909b;
      font-size:9px; font-weight:700; letter-spacing:.09em; }
    .status-dot { display:inline-block; width:7px; height:7px; margin-right:7px; border-radius:50%; background:#41b98d; }
    .main .container-fluid { max-width:1500px; padding-top:28px; padding-bottom:40px; }
    .page-heading { margin:0 0 22px; }
    .eyebrow { color:var(--teal); font-size:10px; font-weight:700; letter-spacing:.16em; text-transform:uppercase; }
    h1,h2,h3,h4 { font-family:Manrope,Inter,sans-serif; color:var(--ink); }
    .page-heading h1 { font-size:clamp(25px,3vw,35px); font-weight:800; margin:6px 0 5px; }
    .page-heading p { color:var(--muted); margin:0; }
    .card { border:1px solid var(--line); border-radius:15px; box-shadow:0 5px 20px rgba(22,45,59,.035); }
    .card-header { background:#fff; border:0; padding:18px 20px 0; font:700 14px Manrope,sans-serif; }
    .card-body { padding:17px 20px 20px; }
    .metric-card { min-height:136px; }
    .metric-label { color:#71818e; font-size:11px; font-weight:700; letter-spacing:.08em; text-transform:uppercase; }
    .metric-value { color:var(--ink); font:800 31px Manrope,sans-serif; margin:12px 0 3px; }
    .metric-foot { color:#97a3ac; font-size:11px; }
    .metric-top { display:flex; justify-content:space-between; align-items:center; }
    .metric-icon { display:grid; place-items:center; width:32px; height:32px; border-radius:9px;
      background:#eaf4f4; color:var(--teal); font-size:14px; font-weight:700; }
    .disclaimer { padding:13px 16px; border:1px solid #e9d9ad; border-radius:11px; background:#fff9e9;
      color:#735f32; font-size:12px; line-height:1.6; margin-top:18px; }
    .analyzer-area { border-radius:12px; }
    .analyzer-area textarea { min-height:220px; border-radius:12px; }
    .analyzer-layout { display:grid; grid-template-columns:minmax(0,1.25fr) minmax(280px,.75fr); gap:20px; }
    .result-card { min-height:260px; display:flex; flex-direction:column; align-items:center; justify-content:center;
      border-radius:16px; padding:28px; text-align:center; background:#f4f7f9; border:1px solid var(--line); }
    .result-card.real { background:#edf8f3; border-color:#c9e9d8; }
    .result-card.fake { background:#fff3ef; border-color:#f2d1c5; }
    .result-kicker { font-size:10px; letter-spacing:.18em; font-weight:800; color:#83919c; }
    .result-word { font:800 32px Manrope,sans-serif; margin:14px 0 5px; }
    .result-card.real .result-word { color:#16835d; }
    .result-card.fake .result-word { color:#c34e3d; }
    .result-confidence { color:#5e707c; font-size:13px; }
    .stat-card { padding:14px; border:1px solid var(--line); border-radius:11px; background:#fff; }
    .stat-card span { display:block; color:#82909b; font-size:10px; font-weight:700; text-transform:uppercase; }
    .stat-card strong { display:block; color:var(--ink); font:700 21px Manrope,sans-serif; margin-top:7px; }
    .keyword-chip { display:inline-block; padding:6px 10px; margin:4px 4px 0 0; border-radius:20px;
      color:#176b78; background:#eaf4f4; font-size:11px; font-weight:600; }
    .about-block { padding:20px; border-radius:14px; border:1px solid var(--line); background:white; }
    .feature-list li { margin:8px 0; color:#596c79; }
    @media(max-width:900px) { .analyzer-layout { grid-template-columns:1fr; } }
  "))),
  uiOutput("page_content")
)

server <- function(input, output, session) {
  output$page_content <- renderUI({
    switch(input$page,
      "Dashboard" = tagList(
        div(class = "page-heading",
            span(class = "eyebrow", "AI + NLP  /  News intelligence"),
            h1("Fake News Detection using NLP"),
            p("AI-Powered News Text Analysis · A transparent, educational text-classification demo")),
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          card(class = "metric-card", div(class="metric-top",span(class="metric-label","Total Articles"),span(class="metric-icon","N")),
               div(class="metric-value",format(nrow(news), big.mark=",")),div(class="metric-foot","Synthetic news records")),
          card(class = "metric-card", div(class="metric-top",span(class="metric-label","Real News"),span(class="metric-icon","✓")),
               div(class="metric-value",format(sum(news$Label=="REAL"),big.mark=",")),div(class="metric-foot","Dataset classification")),
          card(class = "metric-card", div(class="metric-top",span(class="metric-label","Fake News"),span(class="metric-icon","!")),
               div(class="metric-value",format(sum(news$Label=="FAKE"),big.mark=",")),div(class="metric-foot","Dataset classification")),
          card(class = "metric-card", div(class="metric-top",span(class="metric-label","Fake News %"),span(class="metric-icon","%")),
               div(class="metric-value",paste0(round(mean(news$Label=="FAKE")*100,1),"%")),div(class="metric-foot","Share of demo dataset"))
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("News Classification"), plotlyOutput("donut_chart", height="300px")),
          card(card_header("News by Category"), plotlyOutput("category_chart", height="300px"))
        ),
        card(card_header("News Distribution"), plotlyOutput("distribution_chart", height="330px")),
        div(class="disclaimer", tags$strong("Disclaimer: "),
            "This is an educational NLP demonstration. The classifier identifies patterns learned from the dataset and cannot determine whether real-world information is factually true or false.")
      ),
      "News Analyzer" = tagList(
        div(class="page-heading",span(class="eyebrow","Interactive NLP workspace"),h1("News Analyzer"),
            p("Explore the language patterns in a headline or short article.")),
        tags$div(class="visually-hidden",textOutput("prediction_result")),
        div(class="analyzer-layout",
          card(card_header("Enter News Article"),
            div(class="analyzer-area",
              textAreaInput("article_text", NULL, value="", rows=9, width="100%", height="220px",
                            placeholder="Enter a news headline or short article here...")
            ),
            actionButton("analyze", "Analyze News", class="btn-primary"),
            div(class="disclaimer",tags$strong("Educational use only. "),
                "Predictions reflect patterns in synthetic training examples, not verified facts.")
          ),
          card(card_header("Prediction"), uiOutput("prediction_card"))
        ),
        card(card_header("Text Statistics"), uiOutput("text_stats")),
        card(card_header("Detected Keywords"), uiOutput("detected_keywords"))
      ),
      "NLP Analytics" = tagList(
        div(class="page-heading",span(class="eyebrow","Corpus exploration"),h1("NLP Analytics"),
            p("Article length, category mix, and the vocabulary found across the synthetic corpus.")),
        layout_columns(
          col_widths=c(6,6),
          card(card_header("Article Length"),plotlyOutput("length_histogram",height="300px")),
          card(card_header("Article Length by Label"),plotlyOutput("length_boxplot",height="300px")),
          card(card_header("News by Category"),plotlyOutput("analytics_category",height="300px")),
          card(card_header("Most Common Words"),plotlyOutput("common_words",height="350px"))
        )
      ),
      "News Dataset" = tagList(
        div(class="page-heading",span(class="eyebrow","Synthetic corpus"),h1("News Dataset"),
            p("Filter, search, and inspect the short generated examples used in this demonstration.")),
        card(card_header("Dataset Filters"),
          layout_columns(
            col_widths=c(4,4,4),
            selectInput("filter_category","Category",choices=c("All categories",category_order)),
            selectInput("filter_source","Source",choices=c("All sources",sort(unique(news$Source)))),
            selectInput("filter_label","Label",choices=c("All labels","REAL","FAKE"))
          )
        ),
        card(card_header("Articles"),DTOutput("news_table")),
        card(card_header("Selected Article"),uiOutput("article_detail"))
      ),
      "About" = tagList(
        div(class="page-heading",span(class="eyebrow","Project information"),h1("Fake News Detection using NLP")),
        div(class="about-block",
          h3("Project Objective"),
          p("To demonstrate how Natural Language Processing can be used to analyze textual patterns in news articles and perform basic text classification."),
          h4("Technologies"),
          p("R · Shiny · tidytext · ggplot2 · Plotly · dplyr · DT"),
          h4("Features"),
          tags$ul(class="feature-list",
            tags$li("NLP-based text analysis and simple news classification"),
            tags$li("Interactive dashboard and visual analytics"),
            tags$li("Text statistics, dataset exploration, and keyword analysis"),
            tags$li("Fully synthetic example dataset with reproducible generation")),
          h4("Academic AI & Data Science Project"),
          div(class="disclaimer",tags$strong("Disclaimer: "),
              "This is an educational NLP demonstration. The classifier identifies patterns learned from the dataset and cannot determine whether real-world information is factually true or false.")
        )
      )
    )
  })

  analysis <- eventReactive(input$analyze, {
    classify_text(if (is.null(input$article_text)) "" else input$article_text)
  })

  output$prediction_result <- renderText({
    req(analysis())
    if (analysis()$empty) "Enter some text to analyze" else analysis()$prediction
  })
  output$prediction_card <- renderUI({
    result <- analysis()
    if (is.null(result)) {
      return(div(class="result-card",span(class="result-kicker","READY TO ANALYZE"),
                 div(class="result-word","Your result"),p(class="result-confidence","Submit text to see a prediction.")))
    }
    if (result$empty) {
      return(div(class="result-card",span(class="result-kicker","TEXT REQUIRED"),
                 div(class="result-word","Add an article"),p(class="result-confidence","Enter a headline or article to continue.")))
    }
    is_fake <- result$prediction == "FAKE"
    div(class=paste("result-card",if(is_fake)"fake" else "real"),
      span(class="result-kicker","NEWS ANALYSIS"),
      div(class="result-word",paste(if(is_fake)"⚠" else "✓",result$prediction,"NEWS")),
      p(class="result-confidence",paste0("Confidence: ",result$confidence,"%")),
      p(class="result-confidence","Pattern match from the synthetic training dataset")
    )
  })
  output$text_stats <- renderUI({
    result <- analysis()
    if (is.null(result) || result$empty) return(p(class="text-muted","Text statistics will appear after you analyze an article."))
    layout_columns(col_widths=c(3,3,3,3),
      div(class="stat-card",span("Word Count"),strong(result$word_count)),
      div(class="stat-card",span("Character Count"),strong(result$character_count)),
      div(class="stat-card",span("Sentence Count"),strong(result$sentence_count)),
      div(class="stat-card",span("Detected Keywords"),strong(length(result$keywords)))
    )
  })
  output$detected_keywords <- renderUI({
    result <- analysis()
    if (is.null(result) || result$empty) return(p(class="text-muted","Relevant words found in the submitted text will appear here."))
    if (!length(result$keywords)) return(p(class="text-muted","No high-signal training keywords found."))
    tagList(lapply(result$keywords,function(word)span(class="keyword-chip",word)))
  })

  output$donut_chart <- renderPlotly({
    plot_ly(label_counts,labels=~Label,values=~n,type="pie",hole=.66,
            textinfo="label+percent",sort=FALSE,
            marker=list(colors=c("#209978","#e58c50"),line=list(color="white",width=3))) %>%
      layout(showlegend=FALSE,margin=list(l=5,r=5,t=8,b=8),
             annotations=list(list(text=paste0(nrow(news),"<br><span style='font-size:10px'>ARTICLES</span>"),
                                   x=.5,y=.5,showarrow=FALSE,font=list(size=17,color="#172b3a"))))
  })
  output$category_chart <- renderPlotly({
    p <- ggplot(category_counts,aes(x=Category,y=n,fill=Category,
                                    text=paste0(Category,"<br>Articles: ",n))) +
      geom_col(width=.68,show.legend=FALSE) +
      scale_fill_manual(values=c("#176b78","#279ca1","#69b6ae","#90c8b4","#c7d89a","#e4bd70")) +
      labs(x=NULL,y="Articles") + theme_minimal(base_size=11) +
      theme(panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),
            axis.text.x=element_text(angle=20,hjust=1,color="#637481"),
            axis.text.y=element_text(color="#637481"))
    ggplotly(p,tooltip="text") %>% layout(margin=list(l=10,r=10,t=10,b=35))
  })
  output$distribution_chart <- renderPlotly({
    p <- ggplot(distribution,aes(x=Category,y=n,fill=Label,
                                  text=paste0(Category," · ",Label,"<br>Articles: ",n))) +
      geom_col(position="dodge",width=.72) +
      scale_fill_manual(values=c("REAL"="#209978","FAKE"="#e58c50")) +
      labs(x=NULL,y="Articles",fill=NULL) + theme_minimal(base_size=11) +
      theme(panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),
            axis.text.x=element_text(angle=20,hjust=1,color="#637481"),
            axis.text.y=element_text(color="#637481"),legend.position="top")
    ggplotly(p,tooltip="text") %>% layout(margin=list(l=10,r=10,t=10,b=35))
  })
  output$length_histogram <- renderPlotly({
    p <- ggplot(news,aes(x=Word_Count,fill=Label)) +
      geom_histogram(binwidth=5,alpha=.85,position="identity",color="white") +
      scale_fill_manual(values=c("REAL"="#209978","FAKE"="#e58c50")) +
      labs(x="Words per article",y="Articles",fill=NULL) + theme_minimal(base_size=11) +
      theme(panel.grid.minor=element_blank(),legend.position="top")
    ggplotly(p)
  })
  output$length_boxplot <- renderPlotly({
    p <- ggplot(news,aes(x=Label,y=Word_Count,fill=Label)) +
      geom_boxplot(width=.52,alpha=.9,outlier.alpha=.4) +
      scale_fill_manual(values=c("REAL"="#209978","FAKE"="#e58c50")) +
      labs(x=NULL,y="Words per article") + theme_minimal(base_size=11) +
      theme(panel.grid.minor=element_blank(),legend.position="none")
    ggplotly(p)
  })
  output$analytics_category <- renderPlotly({
    p <- ggplot(category_counts,aes(x=reorder(Category,n),y=n,fill=Category,
                                    text=paste0(Category,"<br>Articles: ",n))) +
      geom_col(show.legend=FALSE,width=.68) + coord_flip() +
      scale_fill_manual(values=c("#176b78","#279ca1","#69b6ae","#90c8b4","#c7d89a","#e4bd70")) +
      labs(x=NULL,y="Articles") + theme_minimal(base_size=11) +
      theme(panel.grid.minor=element_blank(),panel.grid.major.y=element_blank())
    ggplotly(p,tooltip="text")
  })
  output$common_words <- renderPlotly({
    p <- ggplot(top_words,aes(x=reorder(word,n),y=n,
                              text=paste0(word,"<br>Occurrences: ",n))) +
      geom_col(fill="#279ca1",width=.72) + coord_flip() +
      labs(x=NULL,y="Occurrences") + theme_minimal(base_size=11) +
      theme(panel.grid.minor=element_blank(),panel.grid.major.y=element_blank())
    ggplotly(p,tooltip="text")
  })

  filtered_news <- reactive({
    result <- news
    if (!is.null(input$filter_category) && input$filter_category != "All categories")
      result <- result %>% filter(Category == input$filter_category)
    if (!is.null(input$filter_source) && input$filter_source != "All sources")
      result <- result %>% filter(Source == input$filter_source)
    if (!is.null(input$filter_label) && input$filter_label != "All labels")
      result <- result %>% filter(Label == input$filter_label)
    result
  })
  output$news_table <- renderDT({
    datatable(
      filtered_news() %>% select(`News ID`=News_ID,Title,Category,Source,Label),
      rownames=FALSE,filter="top",selection="single",
      options=list(pageLength=10,scrollX=TRUE,autoWidth=TRUE,
                   language=list(search="Search records:"))
    )
  })
  output$article_detail <- renderUI({
    selected <- input$news_table_rows_selected
    rows <- filtered_news()
    if (is.null(selected) || !length(selected) || selected > nrow(rows))
      return(p(class="text-muted","Select a row in the table to inspect its full article text."))
    article <- rows[selected,,drop=FALSE]
    div(class="about-block",
      span(class="eyebrow",paste(article$News_ID,"·",as.character(article$Category),"·",article$Source)),
      h4(article$Title),
      p(article$Text),
      tags$strong(paste("Dataset label:",as.character(article$Label)))
    )
  })
}

shinyApp(ui, server)

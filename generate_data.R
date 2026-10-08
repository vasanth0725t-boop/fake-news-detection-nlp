set.seed(20261008)

categories <- c("Politics", "Technology", "Business", "Sports", "Health", "Science")
sources <- c("Civic Ledger", "Tech Daily", "Market Journal", "Field Report", "Wellness Review", "Science Post")
subjects <- c(
  Politics = "a regional public policy",
  Technology = "a community technology",
  Business = "a local business",
  Sports = "a youth sports",
  Health = "a public health",
  Science = "a regional science"
)

real_titles <- c(
  "New report outlines measured progress in %s",
  "Local team shares findings from %s review",
  "Researchers publish data on %s project",
  "Community update describes next steps for %s",
  "Independent review examines %s program"
)
fake_titles <- c(
  "Shocking secret about %s leaves everyone stunned",
  "Hidden discovery promises to change %s overnight",
  "Unbelievable claim about %s spreads across the region",
  "They do not want you to know this about %s",
  "Miracle breakthrough in %s is being kept quiet"
)
real_text <- c(
  "A public research team released a measured update on %s. The report documents reviewed data, explains the study methods, and notes that the findings require further evaluation. Officials said the results are preliminary and will be published for independent review.",
  "A community group shared a routine progress report about %s. Researchers compared available records and confirmed the figures with participating organizations. The update describes the evidence, acknowledges limitations, and outlines the next scheduled review.",
  "An independent review of %s found a modest change in the latest observations. The analysis uses verified records collected over several months. Specialists said the report supports continued monitoring and does not establish a final conclusion.",
  "Officials published an evidence-based summary concerning %s. The document includes measured results, explains how information was collected, and identifies areas that need more study. A follow-up assessment is planned after additional data is available.",
  "Researchers presented findings from a small study of %s. The team shared its methods and source data for review, while noting that the sample has limitations. The report recommends further testing before drawing broader conclusions."
)
fake_text <- c(
  "A shocking secret about %s is being hidden from the public. An unbelievable miracle discovery will completely change everything overnight, according to an unnamed insider. The impossible results are guaranteed, so share this sensational claim immediately before it disappears.",
  "An insider says a secret %s breakthrough has stunned everyone. This unbelievable miracle is supposedly guaranteed to work instantly, but no report or source is provided. They do not want you to know the shocking truth, so share this incredible claim now.",
  "A sensational post claims %s has produced an impossible result. The hidden discovery will supposedly transform everything overnight, with a miracle solution that experts cannot explain. The dramatic claim cites no evidence and urges readers to share it immediately.",
  "An unbelievable story says a secret plan involving %s will change the world today. An unnamed source promises guaranteed results and calls it a miracle, while offering no data or independent review. The shocking claim asks everyone to spread it before it is deleted.",
  "A viral message claims a hidden %s discovery has uncovered a stunning secret. The impossible breakthrough is described as a miracle that works instantly, but the post provides no verified evidence. Readers are urged to share the sensational news right away."
)

n <- 500L
labels <- rep(c("REAL", "FAKE"), each = n / 2)
category <- sample(rep(categories, length.out = n))
label <- sample(labels)

article_text <- vapply(seq_len(n), function(i) {
  category_name <- category[[i]]
  template <- if (label[[i]] == "REAL") sample(real_text, 1) else sample(fake_text, 1)
  sprintf(template, subjects[[category_name]])
}, character(1))

headline <- vapply(seq_len(n), function(i) {
  category_name <- category[[i]]
  template <- if (label[[i]] == "REAL") sample(real_titles, 1) else sample(fake_titles, 1)
  sprintf(template, tolower(category_name))
}, character(1))

news <- data.frame(
  News_ID = sprintf("N%03d", seq_len(n)),
  Title = headline,
  Category = category,
  Source = sample(sources, n, replace = TRUE),
  Text = article_text,
  Label = label,
  stringsAsFactors = FALSE
)

dir.create("data", showWarnings = FALSE)
write.csv(news, file.path("data", "news.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat(sprintf("Generated %d synthetic articles at %s\n", n, file.path("data", "news.csv")))

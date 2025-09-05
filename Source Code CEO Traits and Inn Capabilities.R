#Calling libraries
library("psych"); library("GPArotation"); library("lavaan"); library("corrplot")
library("semTools"); library("DiagrammeR")

#Load Questionnaire's responses
X_raw <- read.csv("Data CEO Dimensions and Innovation.csv")
X <- X_raw[, 4:length(X_raw)]

#Inverting negative items
NP <- read.csv("Preguntas negativas.csv", header = FALSE)
NP <- (NP != 0)
X[,NP] <- 6 - X[,NP]

#Grouping items by category
Leadership <- X[, grepl("LID",colnames(X))]
Innovative.Behavior <- X[, grepl("INN",colnames(X))]
Demographics <- X[, grepl("DEM",colnames(X))]
Hexaco <- X[, !(grepl("LID",colnames(X)) | grepl("INN",colnames(X)) | grepl("DEM",colnames(X))) ]

#Creating dimensions for each category
#For the personality (HEXACO) survey
Hexaco.Dimensions <- as.data.frame(matrix(rep(0,106*6), nrow = 106, ncol = 6))
colnames(Hexaco.Dimensions)<-c("Honesty.Humility","Emotionality","Extraversion",
                               "Agreeableness","Conscientiousness",
                               "Openness.to.Experience")
Hexaco.Honesty.Humility.items <- Hexaco[,(grepl("HON",colnames(Hexaco)) | grepl("VAL",colnames(Hexaco)))]
Hexaco.Honesty.Humility.items <- Hexaco.Honesty.Humility.items[,-1]
Hexaco.Emotionality.items <- Hexaco[,(grepl("EST",colnames(Hexaco)) | grepl("NEU",colnames(Hexaco)))]
Hexaco.Extraversion.items <- Hexaco[,grepl("EXT",colnames(Hexaco))]
Hexaco.Extraversion.items <- Hexaco.Extraversion.items[,-2]
Hexaco.Agreeableness.items <- Hexaco[,grepl("AGR",colnames(Hexaco))]
Hexaco.Agreeableness.items <- Hexaco.Agreeableness.items[, -c(7,8)]
Hexaco.Conscientiousness.items <- Hexaco[,grepl("CSN",colnames(Hexaco))]
Hexaco.Openness.to.Experience.items <- Hexaco[,grepl("OPN",colnames(Hexaco))]

Hexaco.Dimensions$Honesty.Humility <- rowMeans(Hexaco.Honesty.Humility.items)
Hexaco.Dimensions$Emotionality <- rowMeans(Hexaco.Emotionality.items)
Hexaco.Dimensions$Extraversion <- rowMeans(Hexaco.Extraversion.items)
Hexaco.Dimensions$Agreeableness <- rowMeans(Hexaco.Agreeableness.items)
Hexaco.Dimensions$Conscientiousness <- rowMeans(Hexaco.Conscientiousness.items)
Hexaco.Dimensions$Openness.to.Experience <- rowMeans(Hexaco.Openness.to.Experience.items)

#Computing Cronbach's Alpha values
alpha(Hexaco.Honesty.Humility.items, check.keys = TRUE)$total$raw_alpha
alpha(Hexaco.Emotionality.items, check.keys = TRUE)$total$raw_alpha
alpha(Hexaco.Extraversion.items, check.keys = TRUE)$total$raw_alpha
alpha(Hexaco.Agreeableness.items, check.keys = TRUE)$total$raw_alpha
alpha(Hexaco.Conscientiousness.items, check.keys = TRUE)$total$raw_alpha
alpha(Hexaco.Openness.to.Experience.items, check.keys = TRUE)$total$raw_alpha

#For leadership styles
Leadership.Dimensions <- as.data.frame(matrix(rep(0,106*3), nrow=106, ncol=3))
colnames(Leadership.Dimensions) <- c("Transformational.Leadership",
                                     "Transactional.Leadership","Passive.Leadership")

Transformational.Leadership.items <- Leadership[,c("LID06", "LID14","LID23",
                                                   "LID34","LID09","LID13","LID26","LID36","LID02","LID10","LID18","LID21","LID25",
                                                   "LID08","LID30","LID32","LID15","LID19","LID29","LID31")]
Leadership.Dimensions$Transformational.Leadership <- rowMeans(Transformational.Leadership.items)

Transactional.Leadership.items <- Leadership[,c("LID01","LID11","LID16","LID35",
                                                "LID04","LID22","LID24","LID27")]
Leadership.Dimensions$Transactional.Leadership <- rowMeans(Transactional.Leadership.items)

Passive.Leadership.items <- Leadership[,c("LID05","LID07","LID22","LID33",
                                          "LID03","LID12","LID17","LID20")]
Leadership.Dimensions$Passive.Leadership <- rowMeans(Passive.Leadership.items)

#Computing Cronbach's Alpha values
alpha(Transformational.Leadership.items, check.keys = TRUE)$total$raw_alpha
alpha(Transactional.Leadership.items, check.keys = TRUE)$total$raw_alpha
alpha(Passive.Leadership.items, check.keys = TRUE)$total$raw_alpha

#For innovative behavior
Innovative.Behavior.Dimensions <- as.data.frame(matrix(rep(0,106*7), nrow=106, ncol=7))
colnames(Innovative.Behavior.Dimensions) <- c("Questioning", 
                                              "Exploration.Experimentation","Achievement.Motivation","Need.for.Conformity", 
                                              "Observation","Idea.Networks","Tolerance.for.Ambiguity")

Questioning.items <- Innovative.Behavior[,c("INN12","INN13","INN14","INN15",
                                            "INN16","INN17")]
Innovative.Behavior.Dimensions$Questioning <- rowMeans(Questioning.items)

Exploration.Experimentation.items <- Innovative.Behavior[,c("INN21","INN22",
                                                            "INN23","INN24","INN25")]
Innovative.Behavior.Dimensions$Exploration.Experimentation <- rowMeans(Exploration.Experimentation.items)

Achievement.Motivation.items <- Innovative.Behavior[,c("INN01","INN02","INN03",
                                                       "INN04","INN05")]
Innovative.Behavior.Dimensions$Achievement.Motivation <- rowMeans(Achievement.Motivation.items)

Need.for.Conformity.items <- Innovative.Behavior[,c("INN06","INN07","INN08")]
Innovative.Behavior.Dimensions$Need.for.Conformity <- rowMeans(Need.for.Conformity.items)

Observation.items <- Innovative.Behavior[,c("INN18","INN19","INN20")]
Innovative.Behavior.Dimensions$Observation <- rowMeans(Observation.items)

Idea.Networks.items <- Innovative.Behavior[,c("INN26","INN27","INN28","INN29")]
Innovative.Behavior.Dimensions$Idea.Networks <- rowMeans(Idea.Networks.items)

Tolerance.for.Ambiguity.items <- Innovative.Behavior[,c("INN09","INN10","INN11")]
Innovative.Behavior.Dimensions$Tolerance.for.Ambiguity <- rowMeans(Tolerance.for.Ambiguity.items)

#Computing Cronbach's Alpha values
alpha(Questioning.items, check.keys = TRUE)$total$raw_alpha
alpha(Exploration.Experimentation.items, check.keys = TRUE)$total$raw_alpha
alpha(Achievement.Motivation.items, check.keys = TRUE)$total$raw_alpha
alpha(Need.for.Conformity.items, check.keys = TRUE)$total$raw_alpha
alpha(Observation.items, check.keys = TRUE)$total$raw_alpha
alpha(Idea.Networks.items, check.keys = TRUE)$total$raw_alpha
alpha(Tolerance.for.Ambiguity.items, check.keys = TRUE)$total$raw_alpha

#Factorability checks: KMO & Bartlett per block
kmo_hexaco <- KMO(cor(Hexaco, use = "pairwise.complete.obs"))
bart_hexaco <- cortest.bartlett(cor(Hexaco, use = "pairwise.complete.obs"), n = nrow(Hexaco))

kmo_lead <- KMO(cor(Leadership, use = "pairwise.complete.obs"))
bart_lead <- cortest.bartlett(cor(Leadership, use = "pairwise.complete.obs"), n = nrow(Leadership))

kmo_inn <- KMO(cor(Innovative.Behavior, use = "pairwise.complete.obs"))
bart_inn <- cortest.bartlett(cor(Innovative.Behavior, use = "pairwise.complete.obs"), n = nrow(Innovative.Behavior))

kmo_bartlett_summary <- data.frame(
  Block = c("HEXACO", "Leadership", "Innovative Behavior"),
  KMO_Overall = c(kmo_hexaco$MSA, kmo_lead$MSA, kmo_inn$MSA),
  Bartlett_ChiSq = c(bart_hexaco$chisq, bart_lead$chisq, bart_inn$chisq),
  Bartlett_df    = c(bart_hexaco$df,    bart_lead$df,    bart_inn$df),
  Bartlett_p     = c(bart_hexaco$p.value, bart_lead$p.value, bart_inn$p.value)
)
print(kmo_bartlett_summary, digits = 3, row.names = FALSE)

subscale_table <- function(df_items, subscales_named_list, english_names, spanish_names) {
  stopifnot(length(subscales_named_list) == length(english_names),
            length(english_names) == length(spanish_names))
  out <- lapply(seq_along(subscales_named_list), function(i) {
    nm <- names(subscales_named_list)[i]
    cols <- intersect(subscales_named_list[[i]], colnames(df_items))
    if (length(cols) < 2) {
      return(data.frame(
        Subscale_English = english_names[i],
        Subscale_Spanish = spanish_names[i],
        n_items = length(cols),
        Alpha = NA_real_,
        Mean = rowMeans(df_items[, cols, drop = FALSE], na.rm = TRUE) |> mean(na.rm = TRUE),
        SD   = rowMeans(df_items[, cols, drop = FALSE], na.rm = TRUE) |> sd(na.rm = TRUE)
      ))
    }
    a <- psych::alpha(df_items[, cols, drop = FALSE], check.keys = TRUE)$total$raw_alpha
    sc <- rowMeans(df_items[, cols, drop = FALSE], na.rm = TRUE)
    data.frame(
      Subscale_English = english_names[i],
      Subscale_Spanish = spanish_names[i],
      n_items = length(cols),
      Alpha = a,
      Mean = mean(sc, na.rm = TRUE),
      SD   = sd(sc, na.rm = TRUE)
    )
  })
  do.call(rbind, out)
}

Innovation.Capabilites <- read.csv("Innovation Capabilities.csv")
Innovation.Capabilites <- Innovation.Capabilites[,-28]

Product.Innovation <- Innovation.Capabilites[,c(1,3,4,5)]
Process.Innovation <- Innovation.Capabilites[,c(6,7)]
Organizational.Innovation <- Innovation.Capabilites[,c(10,11,12)]
Marketing.Innovation <- Innovation.Capabilites[,c(13,15,16,17)]
Innovation.Culture <- Innovation.Capabilites[,c(2,13,21,22,23)]
Resources.for.Innovation <- Innovation.Capabilites[,c(6,9,18:27)]

kmo_Inn.Capab <- KMO(cor(Innovation.Capabilites, use = "pairwise.complete.obs"))
bart_Inn.Capab <- cortest.bartlett(cor(Innovation.Capabilites, use = "pairwise.complete.obs"), n = nrow(Innovation.Capabilites))

# Cronbach's alpha
alpha(Product.Innovation, check.keys = TRUE)$total$raw_alpha
alpha(Process.Innovation, check.keys = TRUE)$total$raw_alpha
alpha(Organizational.Innovation, check.keys = TRUE)$total$raw_alpha
alpha(Marketing.Innovation, check.keys = TRUE)$total$raw_alpha
alpha(Innovation.Culture, check.keys = TRUE)$total$raw_alpha
alpha(Resources.for.Innovation, check.keys = TRUE)$total$raw_alpha

Innovation.Capabilites.Dimensions <- cbind(rowMeans(Product.Innovation), 
                                           rowMeans(Process.Innovation), rowMeans(Organizational.Innovation), 
                                           rowMeans(Marketing.Innovation), rowMeans(Innovation.Culture), 
                                           rowMeans(Resources.for.Innovation))

colnames(Innovation.Capabilites.Dimensions) <- c("Product.Innovation",
                                                 "Process.Innovation","Organizational.Innovation","Marketing.Innovation",
                                                 "Innovation.Culture","Resources.for.Innovation")

All.Dimensions <- cbind(Hexaco.Dimensions, Leadership.Dimensions, 
                        Innovative.Behavior.Dimensions, Innovation.Capabilites.Dimensions)

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], method = "pearson", ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
corMat=cor(All.Dimensions, method="pearson")
temp <- is.na(corMat)
corMat[temp] <- as.numeric(0)
p.mat <- cor.mtest(All.Dimensions)
corrplot(corMat, type = "upper", order = "original", p.mat = p.mat, 
         sig.level = 0.05, pch.cex = 1.1, cl.cex = 0.7, tl.cex = 0.7)

################################################################################
#SEM model
# --- Reverse Conformity subscale (you already created Conformity_R earlier) ---
Innovative.Behavior.Dimensions$Conformity_R <- 6 - Innovative.Behavior.Dimensions$Need.for.Conformity
Innovation.Capabilites.Dimensions <- as.data.frame(Innovation.Capabilites.Dimensions)

dat_sem <- data.frame(
  O_comp = Hexaco.Dimensions$Openness.to.Experience,
  C_comp = Hexaco.Dimensions$Conscientiousness,
  TL_comp = Leadership.Dimensions$Transformational.Leadership,
  Questioning = Innovative.Behavior.Dimensions$Questioning,
  Exploration_Experimentation = Innovative.Behavior.Dimensions$Exploration.Experimentation,
  Achievement_Motivation = Innovative.Behavior.Dimensions$Achievement.Motivation,
  Observation = Innovative.Behavior.Dimensions$Observation,
  Idea_Networks = Innovative.Behavior.Dimensions$Idea.Networks,
  Tolerance_Ambiguity = Innovative.Behavior.Dimensions$Tolerance.for.Ambiguity,
  Conformity_R = 6 - Innovative.Behavior.Dimensions$Need.for.Conformity,
  Product_Innovation        = Innovation.Capabilites.Dimensions$Product.Innovation,
  Process_Innovation        = Innovation.Capabilites.Dimensions$Process.Innovation,
  Organizational_Innovation = Innovation.Capabilites.Dimensions$Organizational.Innovation,
  Marketing_Innovation      = Innovation.Capabilites.Dimensions$Marketing.Innovation,
  Innovation_Culture        = Innovation.Capabilites.Dimensions$Innovation.Culture,
  Resources_for_Innovation  = Innovation.Capabilites.Dimensions$Resources.for.Innovation
)

model_TLobs <- "
  # Measurement
  IB =~ Questioning + Exploration_Experimentation + Achievement_Motivation +
        Observation + Idea_Networks + Tolerance_Ambiguity + Conformity_R

  ICAP =~ Product_Innovation + Process_Innovation + Organizational_Innovation +
          Marketing_Innovation + Innovation_Culture + Resources_for_Innovation

  # Structural (H1–H3)
  TL_comp ~ a1*O_comp + a2*C_comp      # H1
  IB      ~ b1*TL_comp                 # H2
  ICAP    ~ d1*IB                      # H3

  # Indirects (H4–H5)
  ind_O_to_IB      := a1*b1
  ind_C_to_IB      := a2*b1
  ind_TL_to_ICAP   := b1*d1
  serial_O_to_ICAP := a1*b1*d1
  serial_C_to_ICAP := a2*b1*d1
"

fit_TLobs <- lavaan::sem(model_TLobs, data = dat_sem,
                         estimator = "MLR", std.lv = TRUE, missing = "fiml")
summary(fit_TLobs, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
fitMeasures(fit_TLobs, c("cfi","tli","rmsea","srmr"))

g <- lavaanPlot::lavaanPlot(
  model = fit_TLobs,
  stand = TRUE, coefs = TRUE,
  covs = FALSE,          # keep it uncluttered
  sig  = 0.05,           # show only significant paths (optional)
  labels = list(         # shorten long names
    "O_comp" = "Openness to Experience",
    "TL_comp" = "Transformational Leadership",
    "C_comp" =  "Conscientiousness ",
    "Conformity_R"                 = "Need for Conformity",
    "IB" = "Innovative Behavior",
    "ICAP" = "Innovation Capabilities"
  ),
  graph_options = list(rankdir = "LR")  # left→right layout
)

g

########################################### LOAD LIBRARIES ###################################################
library(data.table);
library(caret);

########################################## GLOBAL VARIABLES #################################################
target <- "delitos_personas"
remove_ceuta_melilla <- F;

########################################## READ DATA ########################################################
dat <- fread("/Users/Falendor/Dropbox/Planes/proyectos/contradatos/contradata/inmigracion_y_delincuencia/inmigracion_y_delincuencia.csv");

########################################## PRE-PROCESS ####################################################
# Remove no targets
dat <- dat[, -setdiff(c("tasa_crim_1000", "delitos_personas", "delitos_sexuales"), target), with = F]

# Remove ceuta and melilla
if (remove_ceuta_melilla){
  dat <- dat[! (comunidad %in% c("ceuta", "melilla"))];
}

# Set comunidad as row name
rownames(dat) <- dat$comunidad;
dat <- dat[, -"comunidad", with = F];

# Convert to numeric
dat <- data.table(sapply(dat, function(x){as.numeric(gsub(",", ".", x))}));

# Copy for linear regression
dat_lm <- dat;

# Group categories
dat_lm$no_primarios <- dat$Analfabetos + dat$Estudios_primarios_incompletos;
dat_lm$primarios <- dat$Educación_primaria;
dat_lm$secundarios <- dat$Primera_etapa_de_Educación_Secundaria + dat$Segunda_etapa_de_educación_secundaria + dat$Segunda_etapa_de_educación_secundaria_con_orientación_profesional;
dat_lm$under19 <- dat$under_15 + dat$`15-19`;
dat_lm$`20-34` <- dat$`20-24` + dat$`25-34`;
dat_lm$over34 <- dat$`35-49` + dat$`50-69` + dat$`>70`;


# Unuseful variables
dat <- dat[, -"anio", with = F];
remove_variables <- c("anio", "delitos_conocidos", "delitos_esclarecidos", 
                      "paro_under25", "paro_over25", "Comunitarios", "No_Comunitarios",
                      "Analfabetos", "Estudios_primarios_incompletos", "Educación_primaria",
                      "Primera_etapa_de_Educación_Secundaria", "Segunda_etapa_de_educación_secundaria",
                      "Segunda_etapa_de_educación_secundaria_con_orientación_profesional",
                      "under_15", "15-19", "20-24", "25-34", "35-49", "50-69", ">70",
                      "poblacion", "renta", "no_primarios");
dat_lm <- dat_lm[, -remove_variables, with = F];



# Tipify
mean_vector_lm <- sapply(dat_lm, mean); 
sd_vector_lm <- sapply(dat_lm, sd);
mean_vector <- sapply(dat, mean); 
sd_vector <- sapply(dat, sd); 

dat <- as.matrix(dat);
for (i in 1:ncol(dat)){
  dat[,i] <-  (dat[,i]-mean_vector[i])/sd_vector[i];
}
dat <- data.table(dat)

dat_lm <- as.matrix(dat_lm);
for (i in 1:ncol(dat_lm)){
  dat_lm[,i] <-  (dat_lm[,i]-mean_vector_lm[i])/sd_vector_lm[i];
}
dat_lm <- data.table(dat_lm)

######################################### [1] LINEAR MODEL ################################################
if (remove_ceuta_melilla){
  model <-lm(formula = sprintf("%s ~ .", target), data = dat_lm);
  summary(model);
}





######################################## [2] VARIABLE IMPORTANCE ##########################################
if (!remove_ceuta_melilla $ target == "delitos_personas"){
  var_imp <- filterVarImp(dat[, -target, with = F], as.numeric(t(dat[, target, with = F])));
  var_imp <- data.table(variable = rownames(var_imp), imp=var_imp$Overall);
  var_imp <- var_imp[order(-imp)];
  print(var_imp);
}



####################################### [3] TEST ERROR ######################################################
# Train/test split
set.seed(140);
test_index <- sample(1:nrow(dat_lm), 4);
test <- dat_lm[test_index];
train <- dat_lm[!test_index];

model_pred <- lm(formula = sprintf("%s ~ .", target), data = train);

# Predict
pred_train <- predict(model_pred, train);
pred_test <- predict(model_pred, test);

# Error
original_mae_train <- mean(abs(pred_train - as.numeric(t(train[, target, with = F]))));
original_mae_test <- mean(abs(pred_test - as.numeric(t(test[, target, with = F]))));

results <- data.table();
for (var in setdiff(colnames(train), target)){
  # Train model
  model_pred <- lm(formula = sprintf("%s ~ .", target), data = train[, -var, with = F]);
  
  # Predict
  pred_train <- predict(model_pred, train);
  pred_test <- predict(model_pred, test);
  
  # Error
  mae_train <- mean(abs(pred_train - as.numeric(t(train[, target, with = F]))));
  mae_test <- mean(abs(pred_test - as.numeric(t(test[, target, with = F]))));
  
  results <- rbind(results, data.table(var = var, mae_train = mae_train, mae_test = mae_test,
                                       diff_train = abs(original_mae_train - mae_train),
                                       diff_test = abs(original_mae_test - mae_test)));
}

results <- results[order(-diff_test)];
print(results);


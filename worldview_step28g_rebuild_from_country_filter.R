# WorldView Online - Step 28g
# Rebuilds the reduced browser rows from WVS7_Individual.rds using the exact
# set of countries already present in the browser dataset, then verifies all
# existing shared variables before importing Q18-Q26 and Q238.

app_dir <- "worldview_static_app"
data_path <- file.path(app_dir,"data","worldview-browser-data-v1.0.0.json")
codebook_path <- file.path(app_dir,"data","worldview-codebook-v1.0.0.json")
index_path <- file.path(app_dir,"index.html")
if(!requireNamespace("jsonlite",quietly=TRUE)) stop("Install jsonlite first.")

source_paths <- c(file.path("WVS_Dataset","WVS7_Individual.rds"),"WVS7_Individual.rds")
source_path <- source_paths[file.exists(source_paths)][1]
if(length(source_path)==0||is.na(source_path)) stop("WVS7_Individual.rds was not found.")

browser <- jsonlite::fromJSON(data_path,simplifyVector=TRUE,simplifyDataFrame=FALSE,simplifyMatrix=FALSE)
browser_df <- as.data.frame(browser,stringsAsFactors=FALSE,optional=TRUE)
codebook <- jsonlite::fromJSON(codebook_path,simplifyVector=FALSE)
source <- readRDS(source_path)
if(!is.data.frame(source)){
 frames <- Filter(is.data.frame,source)
 if(!length(frames)) stop("No data frame found in WVS7_Individual.rds.")
 scores <- vapply(frames,function(x)sum(c(paste0("Q",18:26),"Q238")%in%names(x)),integer(1))
 source <- frames[[which.max(scores)]]
}
requested <- c(paste0("Q",18:26),"Q238")
if(!all(c("B_COUNTRY_ALPHA",requested)%in%names(source))) stop("The source lacks required country or requested columns.")

norm <- function(x){
 if(inherits(x,"haven_labelled")) x<-unclass(x)
 if(is.factor(x)) x<-as.character(x)
 y<-suppressWarnings(as.numeric(x))
 if(sum(!is.na(y))>=.8*sum(!is.na(x))){y[y<0]<-NA_real_;return(y)}
 x<-as.character(x);x[x%in%c("","-1","-2","-3","-4","-5")]<-NA_character_;x
}

countries <- unique(as.character(browser_df$B_COUNTRY_ALPHA))
reduced <- source[as.character(source$B_COUNTRY_ALPHA)%in%countries,,drop=FALSE]

# Test both retained source order and country-block order against the browser.
candidates <- list(source_country_filter=reduced)
if("B_COUNTRY"%in%names(reduced)) candidates$country_name_order <- reduced[order(match(as.character(reduced$B_COUNTRY_ALPHA),countries)),,drop=FALSE]

shared <- intersect(names(browser_df),names(reduced))
shared <- setdiff(shared,c("B_COUNTRY"))
compare_candidate <- function(df){
 if(nrow(df)!=nrow(browser_df)) return(list(rate=0,exact=FALSE))
 rates <- vapply(shared,function(id){
   a<-norm(browser_df[[id]]);b<-norm(df[[id]])
   mean((is.na(a)&is.na(b))|(!is.na(a)&!is.na(b)&as.character(a)==as.character(b)))
 },numeric(1))
 list(rate=mean(rates),exact=all(rates==1),rates=rates)
}
results <- lapply(candidates,compare_candidate)
best <- which.max(vapply(results,function(x)x$rate,numeric(1)))
aligned <- candidates[[best]]; result <- results[[best]]

report <- c(
 paste("Source:",source_path),paste("Browser rows:",nrow(browser_df)),
 paste("Source rows after country filter:",nrow(reduced)),
 paste("Browser countries:",paste(countries,collapse=", ")),
 paste("Best ordering:",names(candidates)[best]),
 paste("Mean shared-variable row agreement:",sprintf("%.8f",result$rate))
)
if(!is.null(result$rates)) report<-c(report,paste(names(result$rates),sprintf("%.6f",result$rates),sep=": "))
writeLines(report,file.path(app_dir,"step28g_alignment_report.txt"))

if(nrow(aligned)!=nrow(browser_df)) stop("Filtering the full WVS source to the browser countries produced ",nrow(aligned)," rows rather than ",nrow(browser_df),". See step28g_alignment_report.txt.")
if(!isTRUE(result$exact)) stop("Country filtering produced the correct row count but shared variables did not match exactly. No data were changed. See step28g_alignment_report.txt.")

labels <- c(Q18="One of my main goals in life has been to make my parents proud",Q19="Parents have a duty to do their best for their children",Q20="Respect and love for parents regardless of their qualities and faults",Q21="Men make better political leaders than women do",Q22="University is more important for a boy than for a girl",Q23="Men make better business executives than women do",Q24="Being a housewife is just as fulfilling as working for pay",Q25="When jobs are scarce, men should have more right to a job than women",Q26="It is a problem if women have more income than their husbands",Q238="Having a strong leader who does not have to bother with parliament and elections")
for(id in requested) browser[[id]]<-unname(norm(aligned[[id]]))
for(id in requested){
 rec<-list(id=id,displayName=unname(labels[id]),analysisType="ordinal",correlationEligible=TRUE,correlationRepresentation="Original WVS response coding; negative missing-value codes excluded",topic=if(id=="Q238")"Political culture and political regimes" else "Social values, norms and stereotypes")
 ix<-which(vapply(codebook$variables,function(v)identical(v$id,id),logical(1)))
 if(length(ix))codebook$variables[[ix[1]]]<-modifyList(codebook$variables[[ix[1]]],rec) else codebook$variables[[length(codebook$variables)+1L]]<-rec
}
jsonlite::write_json(browser,data_path,pretty=FALSE,auto_unbox=TRUE,na="null",null="null",digits=NA)
jsonlite::write_json(codebook,codebook_path,pretty=TRUE,auto_unbox=TRUE,na="null",null="null",digits=NA)

checks<-data.frame(check=c("country_filter_row_count_exact","all_existing_shared_values_exact","requested_variables_added","codebook_updated"),passed=c(nrow(aligned)==nrow(browser_df),result$exact,all(requested%in%names(browser)),all(requested%in%vapply(codebook$variables,function(v)v$id,character(1)))))
write.csv(checks,file.path(app_dir,"step28g_validation_checks.csv"),row.names=FALSE)
if(!all(checks$passed))stop("Step 28g post-import validation failed.")
cat("\nStep 28g completed successfully.\nThe full WVS source was reduced using the existing browser-country set, and every shared value matched before import.\nRestart and force-refresh:\n  servr::httd(\"worldview_static_app\", browser = TRUE)\n")

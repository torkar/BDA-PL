## Pull TOPLAS replication package in directory `dirname` if it doesn't already exist
##
## The replication package is a tarball, which is kept in compressed form with only the needed files extracted locally.
## The tarball is then used to determine if the data is available. Thus, delete the tarball to pull the data anew.
setup.data  <- function(dirname="Data")
{
    options(timeout=100)
    ## Create directory if it doesn't already exist
    dir.create(dirname, showWarnings=FALSE)
    zipfile  <- file.path(dirname, "toplas.zip")
    toplas  <- file.path(dirname, "toplas.tar.gz")
    toplas.dir  <- file.path(dirname, "TOPLAS_Artifact")
    ## yes it's a tarball inside a zip (the latter created by Dropbox)
    if (!file.exists(toplas)) {
        download.file("https://www.dropbox.com/sh/gqcvfzs5awep573/AABfGYQHjmGiExIcXfhb-GPqa?dl=1&preview=toplas.tar.gz",
                      zipfile)
        unzip(zipfile, list=FALSE, exdir=dirname)
        originals  <- file.path("original-artifact", c("_newSha.csv", "_everything.csv"))
        revised  <- file.path("repetition", "Data", c("newSha.csv", "checkSha.csv"))
        untar(toplas, list=FALSE, exdir=dirname)
        for (f in file.path(toplas.dir, c(originals, revised)))
            file.copy(f, dirname)
    }
    unlink(zipfile)
    unlink(file.path(toplas.dir), recursive=TRUE)
}

## Check whether Boolean expression `e` holds and display the result
check  <- function(e)
{
    cat(paste("Assertion: ", deparse(substitute(e)), ": ", if (e) "pass" else "FAIL", "\n"))
}

## Read the overall CSV data of the FSE14 study
## If `cleanup` then also drop all unused columns and setup factors as needed.
load.FSE  <- function(cleanup=FALSE)
{
    data  <- read.csv("Data/_newSha.csv")
    check(nrow(data)==1578165)
    if (cleanup)
        data <- cleanup.data(data)
    data
}

## Regenerate the overall CSV data used for the re-analysis of the TOPLAS study
## Implementation (including comments) copied from `re-analysis.Rmd` in TOPLAS artifact
## I do this because I could not find a dump of this processed data in the TOPLAS artifact
load.TOPLAS  <- function(cleanup=FALSE)
{
    library(dplyr)
    data <- read.csv("Data/newSha.csv")
    ## REMOVE_DUPLICATES
    sha_proj = data %>% dplyr::select(sha, project) %>% group_by(sha, project) %>% dplyr::summarize(n = n())
    duplicates_sha = sha_proj %>% group_by(sha) %>% dplyr::summarize(n = n()) %>% filter(n > 1)
    # this is the number of commits that are present in multiple commits
    num_duplicates_sha = nrow(duplicates_sha)
    # how many projects are affected? 
    dup_repos = data %>% filter(sha %in% duplicates_sha$sha)
    dup_repos = unique(dup_repos$project)
    # determine how many commits are there in the affected projects in total
    commits_by_dup_repos = data %>% filter(project %in% dup_repos) %>% group_by(sha)
    num_commits_by_dup_repos = nrow(commits_by_dup_repos)
    # since we can't do better, exclude all duplicate commits from the dataset
    data = data %>% filter(! sha %in% duplicates_sha$sha)
    not_keep = data %>% group_by(project, language) %>% dplyr::summarize(n = n()) %>% filter(n < 20)
    keep = data %>% group_by(project, language) %>% dplyr::summarize(n = n()) %>% filter(n >= 20)
    data = keep %>% inner_join(data, by=c("project", "language"))
    ## REMOVE_TYPESCRIPT
    data = data %>% filter(language != "Typescript")
    ## REMOVE_V8
    data = data %>% filter(project != "v8")
    ## relevel factors
    data$language <- factor(data$language,
                            levels=sort(unique(as.character(data$language))))
    data$project <- factor(data$project,
                            levels=unique(as.character(data$project)))
    check(nrow(data)==1481307)
    if (cleanup)
        data <- cleanup.data(data)
    check(nrow(data)==1481307)
    data
}

cleanup.data  <- function(data)
{
    ## Remove unused columns
    data  <- data[,
                  c(
                      "language"
                     ,"typeclass"
                     ,"langclass"
                     ,"memoryclass"
                     ,"compileclass"
                     ,"project"
                     ,"sha"
                     ,"files"
                     ,"committer"
                      ##                         ,"author"   ## author is derived by TOPLAS19 from `everything.csv`
                     ,"commit_age"
                     ,"commit_date"
                     ,"insertion"
                     ,"deletion"
                     ,"isbug"
                     ,"domain"
                     ,"btype1"
                     ,"btype2"
                  )]

    ## Keep as factors only columns that are used as such
    data$sha  <- as.character(data$sha)
    data$committer  <- as.character(data$committer)
    data$commit_date  <- as.Date(as.character(data$commit_date))
    data$btype1  <- as.character(data$btype1)
    data$btype2  <- as.character(data$btype2)
    
    # For R v4.0.0 compatibility
    data$language <- as.factor(data$language)
    data$project <- as.factor(data$project)
    ## Sort languages by alphabetical order
    data$language  <- factor(data$language, levels=sort(levels(data$language)))
        
    ## Use `devs` as a synonym of `committer`
    data$devs  <- data$committer
    data
}

## Summarize data `what` by project, language.
##
## This function is based on `summarizeByLanguage` in TOPLAS19's `implementation.R`
by.project.language  <- function(what)
{
    aggr  <- what %>% 
        group_by(project, language) %>%
        dplyr::summarize(
                   commits = n_distinct(sha),
                   insertions = sum(insertion),  ## total insertions (`tins` in TOPLAS19)
                   max_commit_age = max(commit_age),
                   n_bugs = sum(isbug),  ## number of buggy commits (`bcommits` in TOPLAS19)
##                   combined = unique(combined), ## combined refers to the original classification of language, which doesn't matter for RQ1
                   domain = unique(domain),
                   devs = n_distinct(devs)
               )
    ## Add a numeric language id
    aggr$language_id  <- as.numeric(aggr$language)
    aggr
}

## Transform data `what` using function `lofg`.
##
## This function is based on `logTransform` in TOPLAS19's `implementation.R`, 
## but it uses only one kind of log function since I cannot find a justification for mixing log functions.
## In any case, TOPLAS19's replication and CACM17 too use only one log function.
log.transform  <- function(what, logf = log)
{
    data.frame(
        language = what$language,
        language_id = what$language_id,
        log_devs = logf(what$devs),
        log_commits = logf(what$commits),
        log_insertions = logf(what$insertions),
        log_max_commit_age = logf(what$max_commit_age),
        log_n_bugs = logf(what$n_bugs + 0.5*(what$n_bugs == 0)),  ## Set bug counts of 0 to 0.5, so that we can take the log
        n_bugs = what$n_bugs,
##        combined=factor(what$combined),
        domain = factor(what$domain),
        domain_revlev = relevel(what$domain, rev(levels(what$domain))[1]),
        language_revlev = relevel(what$language, rev(levels(what$language))[1]),
        commits = what$commits
##        combined_r = relevel(what$combined, rev(levels(what$combined))[1])
    )
}

## If an object with `filename` exists, deserialize and return it.
## Otherwise, evaluate `fit.model`, serialize, and return it.
eval.or.load  <- function(fit.model, filename, savedir=OBJECT_DIR)
{
    fpath  <- file.path(savedir, paste(filename))
    if (!file.exists(fpath)) {
        m  <- fit.model
        saveRDS(m, file=fpath)
        
    } else {
        m  <- readRDS(fpath)
    }
    m
}

install.packages("git2r")
library(git2r)

## add the pull requester's fork as a named remote
##remote_add(name = "abcde", url = "git@github.com:abcde/usethis.git")

## fetch
##fetch(name = "abcde")

## list remote branches and isolate the one I want
b <- branches(flags = "remote")
b <- b[["upstream/main"]]

## get the SHA of HEAD on this branch
sha <- branch_target(b)

## create local branch
branch_create(commit = lookup(sha = sha), name = "att")

## check it out
checkout(object = ".", branch = "att")

## set upstream tracking branch
branch_set_upstream(repository_head(), name = "upstream/main")

## confirm upstream tracking branch
branch_get_upstream(repository_head())

## make one or more commits here

## push to the branch in the fork and, therefore, into the PR
push()

GitCommitPushLFS <- function(file.path) {
  # Commit and Push large files through GitHub Large File Storage (LFS)
  #
  # Args
  #   file.path : Path to file path/file.suffix
  #
  # Retursn
  #   Message : Indicate status of commit and push
  
  
  git.add.attributes <- "git add .gitattributes"
  git.add            <- paste("git add", file.path)
  git.commit         <- "git commit -m 'commit large file'"
  git.push           <- "git push origin master"
  
  system(git.add.attributes)
  system(git.add)
  system(git.commit)
  system(git.push)
  
  # return(c(git.add.attributes, git.add, git.commit, git.push))
  
}



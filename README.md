# Project description
Graduate final project for Data Science, analysing student evasion in a Private University

# Integrants
 - Diego Luis Pires  - [Linkedin](https://www.linkedin.com/in/diegoluispires)
 - Júlio César Ramos - [Linkedin](https://www.linkedin.com/in/julio-cesar-ramos)
 - Diego Henrique Negretto - Advisor

# Versioning
 - How to version?
   - Version up uses MASTER.MINOR.PATCH to specify each modification using the following the pattern x.x.x
 - How versions are controled?
   - MASTER: updated when you added features like new libs or more code to process the model in other ML algorithms
   - MINOR: updated when you improved some process in the code such as improvements in processing and new pre-processing code or updates in the dataset (for any reason).
   - PATCH: Small modifications in code, like fixes, performance improvements and code standardization.

# Commiting
  - How to commit?
   - After updating the files, follow de steps
  ```
    git status
    git add .
    git commit
    git push --set-upstream origin branch_name
  ```
  - If you made some mistake, please, amend your updates or create a new commit with the fixes.
  ```
    git add .
    git commit --amend --no-edit   // if you don't want to edit the commit message
    git push origin branch_name -f // -f is used to force override your commit
  ```
 
# Merging Rules
  - Merges from pre-release into master can be done every friday night.
  - Always remenber to create a pull request to update the pre-relase.

# What shouldn't I do?
  - Do not update the release branch, it should always contain a stable version of the code. 
  New features must be commited in pre-release branch to be tested and then merged into release.
  - Do not forget to raise the version after a update.
  - Don't ever merge a commit in release or pre-release without a approved.

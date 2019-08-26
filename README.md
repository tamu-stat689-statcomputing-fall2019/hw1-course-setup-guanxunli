# Assignment 1 - Getting started

Student: *Li*, *Guanxun*

Potentially helpful resources:

  * [Happy Git with R](https://happygitwithr.com)
  * [H.Wickham intro to Git and Github with Rstudio](http://r-pkgs.had.co.nz/git.html#git-rstudio)

## Part 1: R and git installation

* Install [R from CRAN](https://cran.r-project.org). If you already have R installed, please check that you have a recent version (at least 3.5.0).

* Install [Rstudio](https://www.rstudio.com/products/rstudio/#Desktop). If you already have Rstudio installed, please update to the most recent version.

* Install [git](https://git-scm.com). If you use Windows, use [git for Windows](https://gitforwindows.org). In your terminal, configure Git to recognize you. Please use your TAMU email if possible.

`git config user.name “First Last"`

`git config --global user.email "email@example.com"`

* Take a screen shot of your terminal window that shows you executing the above commands (you will need to upload it later).


## Part 2: Integration of Git, Github and R

* Clone this repository to local machine using
`git clone https://github.com/StatisticalComputingClass/CourseSetup.git` (make sure you are already within the folder you want it to clone to)

* Open Rstudio and make sure you have the version control enabled, see [Rstudio version control](https://support.rstudio.com/hc/en-us/articles/200532077?version=1.1.463&mode=desktop) for further help.

* Use Rstudio to open the project associated with the CourseSetup directory. You should see the Git tab appear next to Environment and History consoles which will enable you to do staging, commits, pulls and pushes from within Rstudio. You will make all the changes locally, and only then push them to Github.

* Take a screen shot of your Rstudio window with Git panel opened and the Files displaying CourseSetup folder content (you will need to upload it later).

## Part 3: Basics of git

To practice using git, create a separate commit for the following changes:

* Commit 1: Change the *last name*, *first name* at the top of this document to your last name/first name.

* Commit 2: Create a new folder called Screenshots. Upload a screen shot of your git setup from Part 1. Upload a screen shot of your R project with git console open from Part 2. (*all these changes should be in one commit*)

* Push your changes to Github.

## Part 4: Simple program

To practice using git while programming, open the **FunctionsLM.R** and write the corresponding functions for finding beta in linear model. Afterwards, open **SimpleProgram.R** and fill in the missing parts of the code (indicated by [ToDo], **do not use any external libraries or functions such as lm** and do not erase any of the comments). Make sure to make consistent commits throughout and push your changes to GitHub in the end.



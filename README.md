### Description

Will the weather in Peoria in October be suitable for road trippin g?  This
project creates an interactive tool for assessing likely weather conditions for
a specific location in a specific time of the year.  The tool provides useful
climate knowledge that can inform road trip planning.  It currently only covers
the contiguous United States.

### Repository Layout

This repository contains 3-4 directories.  For convenience, each directory name
has a unique letter prefix, such as "X_".

+ **A_Inputs** - Holds all source data files.  For coding purposes, I treat this
directory as read only.
+ **B_Intermediates** – Holds data files created as byproducts of the
computations performed.  This can include cached API pulls, temporary data caches for large computations, and data objects to be passed from one script to the next.
+ **C_Outputs** – Holds the final products for the project.
+ **D_Misc** – If needed, this directory will contain any other useful materials
for the project, such as source data documentation.

The scripts containing executable code are in the top level of the repository.
Each script has a unique numeric prefix, such as "2_", which indicates the order
in which to execute the scripts.

### Scripts (Actions, Inputs, and Outputs)

**1_get_data.R** -

**2_prepare_data.R** -

**3_interact_with_data.R** -

### Project Status and To-Dos

I am actively building the code for this project.  So far, the data pull code
(mostly API pulls) is complete but I have only downloaded a small segment of the
data.

- [ ] Complete README Scripts description for script #1

- [ ] Complete script #2 and associated part of the README Scripts section

- [ ] Complete script #3 and associated part of the README Scripts section

- [ ] Pull the rest of the data down from NOAA's server

- [ ] Complete the description section once the scripts are drafted

- [ ] Make sure gallery, README, and tool text are aligned

- [ ] Polish writing and grammar in all three sets of text

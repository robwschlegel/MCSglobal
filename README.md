# MCSglobal

This is the code repository for the [global marine cold-spell (MCS) analysis](https://www.youtube.com/watch?v=dQw4w9WgXcQ). It is broken down into several folders for ease of access as outlined below.

- analysis: Vignettes for supplementary analyses not part of the main manuscript. Currently it only contains the ice category vignette.
- code/: The entire code base of the project may be found here.
  - functions.R: A central location for customs functions used in other scripts.
  - workflow.R: The full workflow for the analysis may be found in this one script.
  - figures.R: The code for the creation of the figures and tables may be found here.
  - visuals.R: Other visual anlayses etc. that are not intended for publication.
- data/: Results created from `code/workflow.R`. Note that results greater than ~5 MB are not pushed to GitHub. The global SST and MCS results are also too large to host on GitHub. Contact the corresponding author on the publication for data access.
- figures/: All of the figures and tables for publications.
- metadata/: Meta-data created by scripts in this project, with a few files having been imported from external projects. Note that on GitHub this currently only contains colour palette options as the other meta-data files are too large. Contact the corresponding author on the publication for these larger meta-data files.
- output/: Mostly figures created by `code/visuals.R` that are useful for interogating the data, but have no intention of being published.
- summary/: Annual or total summary figures of the state of MCSs globally. These are interesting as an appendix for the manuscript.

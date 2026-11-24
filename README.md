# BSR 1715 Final Project

Code + Figures + Explanations for our final project for the course BSR 1715: Modern Statistics for Modern Biolody at Mt. Sinai

[Full Project Walkthrough](https://djsiddharthvader.github.io/BSR.1715.Project/)

## Reproducibility

We use [rv](https://a2-ai.github.io/rv-docs/intro/installation/) to manage R dependencies, including the following files in the repo
```{bash}
./
├── rproject.toml
├── rv.lock
└── rv/
    └── scripts/
        ├── activate.R
        └── rvr.R

```
which can be used to replicate the R environment used for this analysis as follows, just open R inside this directory and run
```{bash}
curl -sSL https://raw.githubusercontent.com/A2-ai/rv/refs/heads/main/scripts/install.sh | bash
rv init
rv sync  # should isntall packages listed in the toml file
```
You can then render the whole website with `quarto render`

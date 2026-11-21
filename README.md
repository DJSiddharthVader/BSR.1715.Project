# BSR.1715.Project

## Reproducibility

We are using [rv](https://a2-ai.github.io/rv-docs/intro/installation/) to manage R dependencies, including the following files in the repo
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

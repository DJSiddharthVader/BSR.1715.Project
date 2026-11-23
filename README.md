# BSR.1715.Project

## Results

The walkthrough of our approach to cleaning, featurizing, modeling and interpreting the patient data is detailed in the following Quarto reports

1. [Demograhics and Outcomes](https://html-preview.github.io/?url=https://github.com/DJSiddharthVader/BSR.1715.Project/tree/main/notebooks/01_Patient.Outcomes.html)
1. [Featurizing Medical History](https://html-preview.github.io/?url=https://github.com/DJSiddharthVader/BSR.1715.Project/tree/main/notebooks/02_Featurizing.Medical.History.html)
1. [Linear Modelling Approaches](https://html-preview.github.io/?url=https://github.com/DJSiddharthVader/BSR.1715.Project/tree/main/notebooks/03_Linear.Modelling.html)
1. [Bootstrapping](https://html-preview.github.io/?url=https://github.com/DJSiddharthVader/BSR.1715.Project/tree/main/notebooks/04_Bootstrapping.html)


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

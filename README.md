# Relating the Unread

Code and data for my dissertation "Relating the Unread: Modelling Literary History" (publication in preparation).

## Repository structure

```
RelatingTheUnread/
├── corpora/          # Full-text novels (ENG, GER), one .txt per title
├── code/             # Analysis pipeline, organised by stage (see below)
├── data/             # Intermediate and derived data (scores, entities, metadata)
├── analyses/         # Model outputs (embeddings, networks, comparisons)
├── LICENSE
└── README.md
```

### `code/` — pipeline corresponding to each chapter

| Chapter | Folder | What it does |
|---|---|---|
| 1 | `01_canonCorpusBeyond` | Scrapes and cleans primary texts (ECCO, Gutenberg, Zeno, Textgrid) and runs named-entity recognition on literary histories to identify works and persons mentioned beyond the established canon. |
| 2 | `02_operationalisingLitConcepts` | Operationalises "canonisation" (logistic regression over syllabi/anthology presence) and "reception" (reviews and circulating-library records) as quantitative scores per text. |
| 3 | `03_embeddingLitHist` | Trains and evaluates word embeddings (skip-gram word2vec) over the literary history corpus. |
| 4 | `04_modellingLitHist` | Builds network and embedding-based models of literary history (similarity networks, rolling centroids, modularity vs. canonisation/reception) for English and German. |
| 5 | `05_comparingSystems` | Compares the resulting "detected" structures against "described" literary historical accounts, including network centrality and clustering analyses. |

### `data/` and `analyses/`

- `data/literary_histories/` — entities (persons, works) extracted from literary historical texts, clustered and disambiguated, split by language (`ENG`, `GER`).
- `data/scores/` — canonisation scores, reception scores (reviews, circulating libraries), and the metadata/syllabi tables they're derived from.
- `analyses/embedding_lithist/` — trained word embedding models and preprocessed tokens.
- `analyses/modelling_lithist/` (referenced by scripts; generated on re-run) and `analyses/comparing_systems/` — network outputs, centrality measures, and comparison figures.

## Requirements

--- See requirements.txt ---

**R** (≥ 4.x recommended): `ggplot2`, `dplyr`, `tidyr`, `reshape2`, `broom`, `ggpubr`, `ggExtra`, `ggridges`, `gridExtra`, `ggthemes`, `cowplot`, `factoextra`, `vcd`, `e1071`, `MASS`, `car`, `ResourceSelection`, `pscl`, `xml2`, `xslt`.

**Python** (≥ 3.9 recommended): `pandas`, `numpy`, `gensim`, `spacy` (+ `spacy-transformers`), `scikit-learn`, `networkx`, `nltk`, `seaborn`, `matplotlib`, `mantel`.


## Usage

Run the stages in numerical order (`01` → `05`); later stages read the CSV/JSON outputs written by earlier ones. Several scripts contain hard-coded, machine-specific `setwd()` paths (e.g. `C:\Users\Brottrager\Documents\Diss\...`) — these will need to be updated to relative paths or your local clone location before running.


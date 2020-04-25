This repository contains all code and data necessary to replicate all results in the main article and the appendix of our paper *When do Word Embeddings Accurately Reflect Surveys on our Beliefs about People?*.  If you use the new data we collected, please cite our paper as follows:

```

@inproceedings{joseph_when_2020,
  title = {{W}hen do {W}ord {E}mbeddings {A}ccurately {R}eflect {S}urveys on our {Beliefs} about {P}eople?},
  booktitle = {Proceedings of the 58th {{Annual Meeting}} of the {{Association}} for {{Computational Linguistics}} ({{ACL}}'2020)},
  author = {Joseph, Kenneth and Morgan, Jonathan M.},
  year = {2020}
}

```

**Note: We also use data from several other papers! If you use their data or ideas, please cite them as referenced below!**


# Code

- ```generate_embedding_measures.ipynb``` - This python jupyter notebook generates all embedding-based measures of beliefs used in the paper
- ```paper_results.R``` - All results presented in the main portion of the paper, as well as Figures 5-9 in the appendix.
- ```survey_data_statistics_for_appendix.R``` - This file generates plots that summarize the data in the appendix (Figures 1-4).  Probably the best place to start if you're just interested in our survey data.

# Survey Data

We use several different survey datasets, including two we collected ourselves and three that were collected by others. We describe our data in detail here, and then point to the requisite references for the data we used that was collected by others.

## Our survey data

### New beliefs dataset

The file ```our_belief_data_clean.csv``` contains one row per annotator/dimension/identity combination. See the paper for full details on the questions, annotator demographics, etc.

- ```qname``` - A question identifier. You likely will not need this
- ```qtype``` - The type of dimension; either ```affective```, ```association```, or ```trait```
- ```dimension``` - Which of the 17 different dimensions of stereotype was rated by this individual
- ```ecode``` - An anonymous indicator for specific annotators
- ```value``` - The raw value returned on the original scale
- ```rescaled_value``` - The raw value rescaled to [0,1] according to the minimum/maximum of the original scale 
- ```identity``` - The social identity being rated.

### Identity labeling dataset

The file ```our_labeling_data.csv``` contains each question and response posed to annotators for our identity labeling task.  See the paper for more details!

- ```Responseid``` - Unique (anonymized) ID of the respondent
- ```Questionid``` - Unique ID for the question being asked 
- ```Questiontype``` - Type of question - "IsA" or "SeenWith"
- ```Query``` - The identity presented in the question text 
- ```Answer1``` - The first answer presented for this question
- ```Answer2``` - The second answer presented for this question
- ```Answer3``` - The third answer presented for this question
- ```Answer4``` - The fourth answer presented for this question
- ```Answer5``` - The fifth answer presented for this question
- ```Answer``` - The answer selected by the survey respondent
- ```Embeddeddata``` - Irrelevant
- ```Startdate``` - Time the respondent started the survey
- ```Enddate``` - Time the respondent ended the survey
- ```Gender``` - Gender of the respondent
- ```Age``` - Age of the respondnet
- ```Hispanic``` - Is this respondent of Hispanic descent?
- ```Race1``` - Race/Ethnicity of respondent 
- ```Race2``` - Optional answer for additional Race/Ethnicity of respondent
- ```Borninus``` - Was the respondent born in the U.S.?
- ```Percentinus``` - What percentage of the respondent's life has been lived in the U.S.?
- ```Wherelivedlongest``` - Where has the respondent lived the longest?
- ```Wherelivedrecently``` - Where has the respondent lived the most recently?
- ```Education``` - Level of education of the respondent
- ```Political``` - Political leaning of the respondent

## Other people's survey data

### Garg et al. (2018)

The file ```garg_mturk_stereotypes.csv``` is a direct copy of the [MTurk data from Garg et al. (2018)](https://raw.githubusercontent.com/nikhgarg/EmbeddingDynamicStereotypes/master/data/mturk_stereotypes.csv). If you use this data, please cite their paper as follows:

```
@article{garg_word_2018,
  title = {Word Embeddings Quantify 100 Years of Gender and Ethnic Stereotypes},
  author = {Garg, Nikhil and Schiebinger, Londa and Jurafsky, Dan and Zou, James},
  year = {2018},
  month = apr,
  volume = {115},
  pages = {E3635-E3644},
  journal = {Proceedings of the National Academy of Sciences},
  language = {en},
  number = {16},
  pmid = {29615513}
}
```

### Smith-Lovin and Robinson (2015)

The file ```FullCleanUGAData.dta``` is a Stata file that contains results from an EPA study conducted by Smith-Lovin and Robinson (2015). If you use this data, please cite their work as follows:
```
@article{smith-lovin_interpreting_2015,
  title = {Interpreting and {{Responding}} to {{Events}} in {{Arabic Culture}}},
  journal = {Final Report to Office of Naval Research, Grant N00014-09-1-0556},
  author = {{Smith-Lovin}, L. and Robinson, Dawn T.},
  year = {2015}
}
```

### Agarwal et al. (2018)

The directory ```personality-bias_survey``` is a direct copy of [the raw data directory from the personality-based surveys of Agarwal et al. (2018)](https://github.com/oagarwal/personality-bias/tree/master/data/raw). If you use this data, please cite their paper as follows:

```

@inproceedings{agarwal_word_2019,
  title = {Word {{Embeddings}} ({{Also}}) {{Encode Human Personality Stereotypes}}},
  booktitle = {Proceedings of the {{Eighth Joint Conference}} on {{Lexical}} and {{Computational Semantics}} (*{{SEM}} 2019)},
  author = {Agarwal, Oshin and Durup\i{}nar, Funda and Badler, Norman I. and Nenkova, Ani},
  year = {2019},
  month = jun,
  pages = {205--211},
  language = {en-us}
}
```

# Embedding Data

We use embeddings from the following four sources:

- [GloVe](https://nlp.stanford.edu/projects/glove/)
- [FastText](https://fasttext.cc/docs/en/english-vectors.html)
- [Word2Vec](https://code.google.com/archive/p/word2vec/) 
- [Number Batch](https://github.com/commonsense/conceptnet-numberbatch)

We convert them to more efficient formats using the [save_embeds.py utility function from Hila Gonen's github repository](https://github.com/gonenhila/gender_bias_lipstick/blob/master/source/save_embeds.py).

The embedding we use can be downloaded from [here (note, the tar is ~16GB)](https://drive.google.com/open?id=1o8eAA2OD0j3nH40fPBG1v0jcD7ukasBj). Pull this tar file down and run the following at the command line to extract the files (or just run the relevant code to download and untarr in ```generate_embedding_measures.ipynb```).



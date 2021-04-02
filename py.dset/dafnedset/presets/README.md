# Presets for the Dafne Dataset

Each .py file in this folder is a preset of the dataset.
It must define a `data` object with labeled training data.

The data comes from the dafne-db:dset-latest database, see [../datasets.py](../datasets.py)

Standard dataset is builded from the following:
 - neg10.py
 - pos10.py (posivives labeled 1,0)
 - pos08.py (posivives labeled 0.8,0.2)
 - total.py (augmented set)
 - total_pos.py (augmented set, positive samples only)
 - total_pos_n.py (augmented positive set relabeled as negative)

Partition strategy is start splitting the dataset:

| Training | Testing | Validation | Visaulization |
| -------- | ------- | ---------- | ------------- |
| 0.7      | 0.15    | 0.145      | 0.005         |

`__init__.py` takes Testing, Validation and Visualization sets and assigns
them in `test_(pos/neg/tot)`, `val_(...)`, `show(...)` objects

# [neg10](./neg10.py): A preset for negative samples

- Query samples
- splits in train test val and show

- For training, drops epochs according to nan_distriburion

The distribution of nans is hardcoded, the goal is for negative samples
to follow the nans distribution of positive samples.

# [pos10](./pos10.py): A pre-set for positive samples

 - Query samples
 - split in Train, Test, Val, Show

# [pos08](./pos08.py): A pre-set for positive samples, labeled 0.8,0.2

This showld probably be gone, relabeling should be made explicit.

 - Take pos10 samples
 - Relabel

# [total.py](total.py): A pre-set for fully augmented dataset

 - Takes positive and negative samples
 - Creates 4x positive and negative sets
 - For each set of ( all_positive, all_negative, all )
   - mix channels and relabel.
   - rotate east-north axis.

 - Return all mixed data along unmodified data (positives 0.8,0.2).

# [total_pos.py](total_pos.py): A pre-set for augmented positive Samples

Operates only on the training set.

 - Take positive data
 - Create 8x length set of data with mixed channels, relabeled.
 - Return mixed data along original data. (labeled 0.8,0.2)

# [total_pos_n.py](total_pos_n.py): A pre-set for augmented positive data relabeled as negative.

This is for usage in deceiver training, which targets an all-negative response
of the classifier.

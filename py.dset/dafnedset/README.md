# Dataset Creation API

This module defines:

1- a Dataset creation API
2- presets for datasets used in the project.

# API:

The api defines some classes of iterators, and defines transformations over those classes.

Once an iterator is initializated with data (primitive itrators takes data form a database,
and can be imported from [[datastets.py]]). dot-notation is used in order to apply transformations.

Available transformations are:
  - `.split`
  - `.preprocess`
  - `.scale`
  - `.label`
  - `.cache`

Preprocessing is a generic transformation, used in combination with the functions defined in `transformations.py`

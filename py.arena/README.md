# Training Arena for Dafne (tag: jupyter)

This app is for experimentation.

We expect a cached dataset in dafne_home/cache,

Those files are read by a module name dafneloader, then,
datasets can be imported from python with `from dafneloader import dsets`

Convension is to save trained models into dafne_home/saved_models

bind-mounting anything into /arena allows it to be imported as `from arena import ...`

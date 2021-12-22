"""Dataset Creation for seismic GNSS dialy timeseries dataset,

Data is queryed form a pre-filled database.

Raw data is filtered trough pre-configured data_augmentation and labelling
operations.

The transformation configuration is managed by the presets submodule.

Any object of the presets submodeule which has a .data property which iterates
trough pyArrow batches can be served by the app.

We usually use Batch Feeders from base.py, transformed by the operations on
transformations.py. See presets for examples.
"""

if __name__ == '__main__':
    from . import presets_simple as ps
    print("Presets Imported")

    for i in ps.pos10.data:
        print("pos10 has batches of length {}".format(len(i)))
        break

    print("---- end ----")

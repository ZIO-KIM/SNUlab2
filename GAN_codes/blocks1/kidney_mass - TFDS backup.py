"""kidney_mass dataset."""

import tensorflow_datasets as tfds
import os

# TODO(kidney_mass): Markdown description  that will appear on the catalog page.
_DESCRIPTION = """
Description is **formatted** as markdown.

It should also contain any processing which has been applied (if any),
(e.g. corrupted example skipped, images cropped,...):
"""

# TODO(kidney_mass): BibTeX citation
_CITATION = """
"""


class KidneyMass(tfds.core.GeneratorBasedBuilder):
  """DatasetBuilder for kidney_mass dataset."""

  VERSION = tfds.core.Version('1.0.0')
  RELEASE_NOTES = {
      '1.0.0': 'Initial release.',
  }

  def _info(self) -> tfds.core.DatasetInfo:
    """Returns the dataset metadata."""
    # TODO(kidney_mass): Specifies the tfds.core.DatasetInfo object
    return tfds.core.DatasetInfo(
        builder=self,
        description=_DESCRIPTION,
        features=tfds.features.FeaturesDict({
            # These are the features of your dataset like images, labels ...
            'image': tfds.features.Image(shape=(512, 512, 3)),
            'label': tfds.features.ClassLabel(names=['ART', 'PRE']),
        }),
        # If there's a common (input, target) tuple from the
        # features, specify them here. They'll be used if
        # `as_supervised=True` in `builder.as_dataset`.
        supervised_keys=('image', 'label'),  # Set to `None` to disable
        homepage='https://dataset-homepage/',
        citation=_CITATION,
    )

  def _split_generators(self, dl_manager: tfds.download.DownloadManager):
    """Returns SplitGenerators."""
    # TODO(kidney_mass): Downloads the data and defines the splits
    # path = dl_manager.download_and_extract('https://todo-data-url')
    archive_path = '/home/ncp/workspace/blocks1/kidneyData_windowing_mass.zip' # train, test 나눠져 있는 데이터셋으로
    extracted_path = dl_manager.extract(archive_path)
    
    train_art_path = os.path.join(extracted_path, "TRAIN/ART")
    train_pre_path = os.path.join(extracted_path, "TRAIN/PRE")
    test_art_path = os.path.join(extracted_path, "TEST/ART")
    test_pre_path = os.path.join(extracted_path, "TEST/PRE")
    

    # TODO(kidney_mass): Returns the Dict[split names, Iterator[Key, Example]]
    return [
        tfds.core.SplitGenerator(
            name="TRAIN/ART",
            gen_kwargs={
                "path":train_art_path, 
                "label":"ART",
            },
        ),
        tfds.core.SplitGenerator(
            name="TRAIN/PRE",
            gen_kwargs={
                "path":train_pre_path, 
                "label":"PRE",
            },
        ),
        tfds.core.SplitGenerator(
            name="TEST/ART",
            gen_kwargs={
                "path":test_art_path, 
                "label":"ART",
            },
        ),
        tfds.core.SplitGenerator(
            name="TEST/PRE",
            gen_kwargs={
                "path":test_pre_path, 
                "label":"PRE",
            },
        ),
    ]

  def _generate_examples(self, path, label):
        images=tf.io.gfile.listdir(path)
        
        for image in images: 
            record={
                "image":os.path.join(path,image), 
                "label":label,
            }
            yield image, record

import matplotlib.pyplot as plt
import tensorflow as tf
from tensorflow import keras
import numpy as np
from natsort import natsorted
import rasterio
from rasterio.merge import merge
import glob
import os


def load_tiled_data(folder_name):
  tiles = []
  for file in natsorted(glob.glob(folder_name + '/*.tif')):
    image = rasterio.open(file)
    patch = image.read()
    tiles.append(patch)
  return(np.array(tiles).transpose(0,2,3,1)).astype('float32')

def find_NaN (im_array):
  Naan = np.argwhere(np.isnan(im_array))
  return print (Naan, 'found in', im_array, 'array')

def write_outputs(input_folder_name, prediction_tiles, output_path):
  for i in range(prediction_tiles.shape[0]):
    initial_image_path = natsorted(glob.glob(input_folder_name + '/*.tif'))[i]
    initial_image = rasterio.open(initial_image_path)
    profile = initial_image.profile
    profile.update(count=1)
    pred = prediction_tiles[i]
    new_tile = rasterio.open(output_path + '/Labeled_output_' + str(i+1) + '.tif', 'w', **profile)
    new_tile.write(pred, 1)
    new_tile.close()

def mosaic_tiles(input_folder_name, output_path):
  mosaic_list = []
  for file in natsorted(glob.glob(input_folder_name + '/*.tif')):
    image = rasterio.open(file)
    mosaic_list.append(image)
  mosaic, transform_data = merge(mosaic_list)
  output_meta = image.meta.copy()
  output_meta.update(
      {"driver": "GTiff",
          "height": mosaic.shape[1],
          "width": mosaic.shape[2],
          "transform": transform_data,
      }
  )
  return mosaic

  rasterio.open(output_path + '/mosaic.tif', 'w', **mosaic.profile).write(mosaic)
  return mosaic

def preprocess_images(x, y, N_class):
  find_NaN(x)
  find_NaN(y)
  x_norm = keras.utils.normalize(x, axis = -1)
  y_hot = keras.utils.to_categorical(y, num_classes = N_class)
  return(x_norm, y_hot)















def load_tiled_tif(file_path):
  for file in file_list:
    image = rasterio.open(file)
    patch = image.read()
    tiles.append(patch)
  return(np.array(tiles).transpose(0,2,3,1)).astype('float32')

def internal_read(image_list, mask_list):
    images = load_tiled_tif(image_list)
    masks = load_tiled_tif(mask_list)
    return(images, masks)

def data_generator(image_list, mask_list, batch_size=16):
    dataset = tf.data.Dataset.from_tensor_slices((image_list, mask_list))
    dataset = dataset.map(internal_read(image_list, mask_list))
    dataset = dataset.batch(BATCH_SIZE, drop_remainder=True)
    return dataset

def write_outputs(input_folder_name, prediction_tiles, output_path):
  for i in range(prediction_tiles.shape[0]):
    initial_image_path = natsorted(glob.glob(input_folder_name + '/*.tif'))[i]
    initial_image = rasterio.open(initial_image_path)
    profile = initial_image.profile
    profile.update(count=1)
    pred = prediction_tiles[i]
    new_tile = rasterio.open(output_path + '/Labeled_output_' + str(i) + '.tif', 'w', **profile)
    new_tile.write(pred, 1)
    new_tile.close()

def load_tiled_data(folder_name):
  tiles = []
  for file in natsorted(glob.glob(folder_name + '/*.tif')):
    image = rasterio.open(file)
    patch = image.read()
    tiles.append(patch)
  return(np.array(tiles).transpose(0,2,3,1)).astype('float32')

def find_NaN (array):
  Naan = np.argwhere(np.isnan(array))
  return print (Naan)


#Multi-plot
def plot_samples_matplot(display_list,im_num, figsize=(15,15)):
    _, axes = plt.subplots(1, len(display_list), figsize=figsize)
    for i in range(len(display_list)):
      if display_list[i].shape[-1] == 3:
        axes[i].imshow(display_list[im_num,:,:,1])
        axes[i].axis('off')
    plt.show()

def infer(image, model):
    prediction = model.predict(image)
    arg_pred = np.argmax(prediction, axis=-1)

def plot_predictions(image_list):
    for file in image_list:
        image = load_tiled_tif(image_list)
        soft_pred = model.predict(image)
        arg_pred = np.argmax(prediction, axis=-1)
        plot_samples_matplot([image, arg_pred])
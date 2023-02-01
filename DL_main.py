import tensorflow as tf
from tensorflow import keras
import rasterio
import sklearn
import os
import glob
from matplotlib import pyplot as plt
from tensorflow.keras import layers
from natsort import natsorted
from keras.callbacks import TensorBoard
from sklearn.model_selection import train_test_split

from helpers import *
from Models import *

device_name = tf.test.gpu_device_name()
if device_name != '/device:GPU:0':
  raise SystemError('GPU device not found')
  print('Found GPU at: {}'.format(device_name))

train_image_folder = '/home/bjyberg/Peatland/test_Run/training/patched_images'
train_mask_folder = '/home/bjyberg/Peatland/test_Run/training/patched_labels'
val_image_folder = '/home/bjyberg/Peatland/test_Run/validation/patched_images'
val_mask_folder = '/home/bjyberg/Peatland/test_Run/validation/patched_labels'
sizeX = 64
sizeY = 64
N_class = 5
channels = 5
batch_size = 16

train_x = load_tiled_data(train_image_folder)
train_y = load_tiled_data(train_mask_folder)
val_x = load_tiled_data(val_image_folder)
val_y = load_tiled_data(val_mask_folder)

train_x, train_y = preprocess_images(train_x, train_y, N_class)
val_x, val_y = preprocess_images(val_x, val_y, N_class)

train_dataset = tf.data.Dataset.from_tensor_slices((x_train, y_train))
val_dataset = tf.data.Dataset.from_tensor_slices((x_val, y_val))

test_mod = get_keras_model(N_class, sizeX, sizeY, channels)

plot_samples_matplot([x_train, train_y], im_num=77)



print(train_image_folder)








im_paths = natsorted(glob.glob(image_folder + '/*.tif'))
mask_paths = natsorted(glob.glob(mask_folder + '/*.tif'))
x_train, x_val, y_train, y_val = train_test_split(im_paths, mask_paths, test_size=0.2, random_state=42)

train = data_generator(x_train, y_train)
val = data_generator(x_val, y_val)

rm -rf ./logs/
tensorboard --logdir logs/fit

out = load_tiled_tif(im_paths[1])

file = tf.io.read_file(im_paths[1])

dataset = tf.data.Dataset.from_tensor_slices((x_train, y_train))

print(dataset)
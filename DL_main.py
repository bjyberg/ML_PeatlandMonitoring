import tensorflow as tf
from tensorflow import keras
import rasterio
import sklearn
import os
import glob
from matplotlib import pyplot as plt
from tensorflow.keras import layers
from natsort import natsorted



device_name = tf.test.gpu_device_name()
if device_name != '/device:GPU:0':
  raise SystemError('GPU device not found')
  print('Found GPU at: {}'.format(device_name))


import tensorflow as tf
from tensorflow import keras
import rasterio
from rasterio import plot
import sklearn
import os
import glob
from matplotlib import pyplot as plt
from tensorflow.keras import layers
from natsort import natsorted
from keras.callbacks import TensorBoard
from sklearn.model_selection import train_test_split
from keras.callbacks import EarlyStopping, ModelCheckpoint, ReduceLROnPlateau

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
full_image = '/home/bjyberg/Peatland/test_Run/patched_image'

sizeX = 64
sizeY = 64
N_class = 4
channels = 5
batch_size = 16

train_x = load_tiled_data(train_image_folder)
train_y = load_tiled_data(train_mask_folder)
val_x = load_tiled_data(val_image_folder)
val_y = load_tiled_data(val_mask_folder)

train_x, train_y = preprocess_images(train_x, train_y, N_class)
val_x, val_y = preprocess_images(val_x, val_y, N_class)

train_dataset = tf.data.Dataset.from_tensor_slices((train_x, train_y)).shuffle(150)
train_dataset = train_dataset.batch(batch_size)

val_dataset = tf.data.Dataset.from_tensor_slices((val_x, val_y)).shuffle(150)
val_dataset = val_dataset.batch(batch_size)

test_mod = multi_unet_model(N_class, sizeX, sizeY, channels)
#opt = keras.optimizers.Nadam(learning_rate=1e-5)
test_mod.compile(optimizer = 'adam', loss = 'categorical_crossentropy', metrics = ['accuracy'])
test_mod.summary()

early_stop = EarlyStopping(monitor='val_loss', patience=20, mode= 'min')
model_save = ModelCheckpoint('/home/bjyberg/Peatland/test_Run/peatNET.hdf5', save_best_only=True, monitor='val_loss', mode='min')
reduce_lr_loss = ReduceLROnPlateau(monitor='val_loss', factor=0.1, patience=20, verbose=1, mode='min')
from tensorflow.keras import backend as K
def dice_coef(y_true, y_pred, smooth):
    y_true_f = K.flatten(y_true)
    y_pred_f = K.flatten(y_pred)
    intersection = K.sum(y_true_f * y_pred_f)
    dice = (2. * intersection + smooth) / (K.sum(y_true_f) + K.sum(y_pred_f) + smooth)
    return dice
def dice_coef_multilabel(y_true, y_pred, M, smooth):
    dice = 0
    for index in range(M):
        dice += dice_coef(y_true[:,:,:,index], y_pred[:,:,:,index], smooth)
    return dice
def dice_coef_m_loss(y_true, y_pred, M = N_class, smooth = 100):
    return 1 - dice_coef_multilabel(y_true, y_pred, M, smooth)


test_history = test_mod.fit(train_dataset, 
                    batch_size = batch_size, 
                    verbose=1, 
                    epochs=150,
                    callbacks=[model_save, early_stop, reduce_lr_loss], 
                    #steps_per_epoch=200 / batch_size,
                    validation_data=(val_dataset))

val_preds = test_mod.predict(val_x)
val_preds = np.argmax(val_preds, axis=-1)
np.unique(val_preds)
plt.imshow(val_preds[50])
plt.show()

full_patches = load_tiled_data(full_image)
full_patches_norm = keras.utils.normalize(full_patches, axis=-1)

predictions = test_mod.predict(full_patches)
predictions_norm = test_mod.predict(full_patches_norm)
predictions = np.argmax(predictions, axis=-1)
predictions_norm = np.argmax(predictions_norm, axis=-1)
np.unique(predictions)
np.unique(predictions_norm)

test_mod = keras.models.load_model('/home/bjyberg/Peatland/test_Run/peatNET.hdf5')

plot_samples_matplot([x_train, train_y], im_num=77)

write_outputs(full_image, predictions, '/home/bjyberg/Peatland/test_Run/outputs')
write_outputs(full_image, predictions_norm, '/home/bjyberg/Peatland/test_Run/outputs_norm')

mosaic = mosaic_tiles('/home/bjyberg/Peatland/test_Run/outputs_norm', '/home/bjyberg/Peatland/test_Run/outputs_norm')
plot.show(mosaic)

print(train_image_folder)





full_patches.shape


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
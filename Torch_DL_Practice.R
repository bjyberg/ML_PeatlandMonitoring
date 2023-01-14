library(torch)
library(luz)
library(torchvision)
library(torchdatasets)
?nn_sequential()

train_indices <- 1:1000
val_indices <- 1001:1500
test_indices <- 1501:2000

add_channel_dim <- function(img) img$unsqueeze(1) #adds a single channel in the first position
#above 
#below crops out the axis and legend from the plots

crop_axes <- function(img) transform_crop(img, top = 0, left = 21, height = 131, width = 130)

root <- file.path(tempdir(), "correlation")

train_ds <- guess_the_correlation_dataset(
  # where to unpack
  root = root,
  # additional preprocessing 
  transform = function(img) crop_axes(img) %>% add_channel_dim(),
  # don't take all data, but just the indices we pass in
  indexes = train_indices,
  download = TRUE
)

#Add validation data - the transform functions were made above

valid_ds <- guess_the_correlation_dataset(
  root = root,
  transform = function(img) crop_axes(img) %>% add_channel_dim(),
  indexes = val_indices,
  download = FALSE
)

test_ds <- guess_the_correlation_dataset(
  root = root,
  transform = function(img) crop_axes(img) %>% add_channel_dim(),
  indexes = test_indices,
  download = FALSE
)

#test that they are right
length(train_ds)
length(valid_ds)
length(test_ds)

#create a data loader to feed into the algorithm 

train_dl <- dataloader(train_ds, batch_size = 16, shuffle = TRUE)


length(train_dl) #Number of batches#

###Plot the things to make sure correct
batch <- dataloader_make_iter(train_dl) %>% dataloader_next()

dim(batch$x)
dim(batch$y)

par(mfrow = c(8,8), mar = rep(0, 4)) #just for the plot

images <- as.array(batch$x$squeeze(2)) #this or below code removes the first channel

images %>%
  purrr::array_tree(1) %>%
  purrr::map(as.raster) %>%
  purrr::iwalk(~{plot(.x)})

#list the lables
batch$y %>% as.numeric() %>% round(digits = 2)

#load in the other datasets
valid_dl <- dataloader(valid_ds, batch_size = 64)
length(valid_dl)
test_dl <- dataloader(test_ds, batch_size = 64)
length(test_dl)

#create model

torch_manual_seed(6255)

net <- nn_module(
  
  "corr-cnn",
  
  initialize = function() {
    
    self$conv1 <- nn_conv2d(in_channels = 1, out_channels = 32, kernel_size = 3)
    self$conv2 <- nn_conv2d(in_channels = 32, out_channels = 64, kernel_size = 3)
    self$conv3 <- nn_conv2d(in_channels = 64, out_channels = 128, kernel_size = 3)
    
    self$fc1 <- nn_linear(in_features = 14 * 14 * 128, out_features = 128)
    self$fc2 <- nn_linear(in_features = 128, out_features = 1)
    
  },
  
  forward = function(x) {
    
    x %>% 
      self$conv1() %>%
      nnf_relu() %>%
      nnf_avg_pool2d(2) %>%
      
      self$conv2() %>%
      nnf_relu() %>%
      nnf_avg_pool2d(2) %>%
      
      self$conv3() %>%
      nnf_relu() %>%
      nnf_avg_pool2d(2) %>%
      
      torch_flatten(start_dim = 2) %>%
      self$fc1() %>%
      nnf_relu() %>%
      
      self$fc2()
  }
)

model <- net()
model(batch$x) #test to make sure the shapes line up

#prep the model in luz with optimizers and the loss functions (adam, mean sq error)

fitted <- net %>%
  setup(
    loss = function(y_hat, y_true) nnf_mse_loss(y_hat, y_true$unsqueeze(2)),
    optimizer = optim_adam
  )

#fitting/training model

fitted <- net %>%
  setup(
    loss = function(y_hat, y_true) nnf_mse_loss(y_hat, y_true$unsqueeze(2)),
    optimizer = optim_adam
  ) %>%
  fit(train_dl, epochs = 10, valid_data = test_dl)

preds <- predict(fitted, valid_dl) #predict the validation data as we used the test for validating training lol

#plot
preds <- preds$to(device = "cpu")$squeeze() %>% as.numeric()
test_dl <- dataloader(test_ds, batch_size = 5000)
targets <- (test_dl %>% dataloader_make_iter() %>% dataloader_next())$y %>% as.numeric()

df <- data.frame(preds = preds, targets = targets)

library(ggplot2)

ggplot(df, aes(x = targets, y = preds)) +
  geom_point(size = 0.1) +
  theme_classic() +
  xlab("true correlations") +
  ylab("model predictions")

plot(as.array(fitted$records$metrics$train))

cuda_is_available()

??tensorboard

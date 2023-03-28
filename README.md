# kNN OCR
MNIST Optical Character Recognition

## Installation
Download the project, and in the folder `src`, run

```bash
touch .depend
make depend
make
make cleanup
```

## Usage
```
$./main -h
Usage : ./main [-h] [-v] [-t] [-p INDEX] [-b] [-d DIST] [-kd] TRAIN_NB TEST_NB K

Recognize images from the MNIST data base.

Positional arguments :
    TRAIN_NB                  The number of train images
    TEST_NB                   The number of test images
    K                         The kNN parameter

Optional arguments :
    -h, --help                Print this help message and exit
    -v, --verbose             Show confusion matrix and elapsed time
    -t, --test                Run tests and exit (ignore positional arguments)
    -kd, --kd-tree            Use a kd tree
    -b, --binarize            Preprocess the image by keeping only two colors
    -d DIST, --distance DIST  Give the used distance. Possible values are :
                                  - 0 : the square of the euclidean distance (default) ;
                                  - 1 : same, but only in the 20 x 20 pixels
                                        center of the image ;
                                  - 2 : binarize image before applying the distance.
    -p INDEX, --print INDEX   Print the image at position INDEX and exit (ignore
                              positional arguments)
```

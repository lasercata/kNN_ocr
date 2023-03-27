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
Usage : ./main [-h] [-v] [-t] [-p INDEX] [-kd] TRAIN_NB TEST_NB K

Recognize images from the MNIST data base.

Positional arguments :
    TRAIN_NB                 The number of train images
    TEST_NB                  The number of test images
    K                        The kNN parameter

Optional arguments :
    -h, --help               Print this help message and exit
    -v, --verbose            Be more verbose
    -t, --test               Run tests and exit (ignore positional arguments)
    -kd, --kd-tree           Use a kd tree
    -p INDEX, --print INDEX  Print the image at position INDEX and exit (ignore
                             positional arguments)
```

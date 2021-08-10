# ASKE-E Jupyter Kernel

## Installation

0. Uninstall any old aske-e kernels

``` sh
$ jupyter kernelspec list
Available kernels:
  askee_kernel    /Users/abakst/Library/Jupyter/kernels/askee_kernel
  askee-kernel    /Users/abakst/Library/Jupyter/kernels/askee-kernel
  askeekernel    /Users/abakst/Library/Jupyter/kernels/askeekernel
$ jupyter kernelspec uninstall askee-kernel
$ jupyter kernelspec uninstall askee_kernel
$ jupyter kernelspec uninstall askeekernel
```

1. Install aske-e kernel

``` sh
$ jupyter kernelspec install src/askee_kernel --user
```

2. Setup pipenv

``` sh
$ pipenv install
```

3. Run Jupyter lab (assumes there is a running `donu` instance on `localhost:8000`

``` sh
$ pipenv run jupyter lab
```


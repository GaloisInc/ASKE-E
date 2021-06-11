The LOE tools are experimental; they attempt to quantify a Level of Effort
necessary to manually transform one representation of a model to another.

loe-metrics.py FILE1 FILE2
	Prints information regarding the LOE to transform FILE1 to FILE2.
loe-driver.sh FILE_OR_DIRECTORY FILE_OR_DIRECTORY...
	Prints information regarding the LOE for a sequence of derivations
	of the same model.

Notes and Caveats
------------------

1) It'd be nice if the scalar LOE metric satisfies the triangle inequality.
2) When a directory is specified as input to loe-driver.sh, all files found
   in that directory and its children are catenated before passing to the
   metrics calculation.
3) All input files must contain UTF-8 text.

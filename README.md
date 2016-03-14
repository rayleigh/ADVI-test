This repository holds code to test different methods of RStan's stan against NUTS on the Stan example models on Github. For a discussion of the code, click [here] (https://github.com/stan-dev/example-models/wiki/test_NUTS-test_ADVI-discussion-page).

To use it for the first time, source run\_test\_NUTS\_and\_test\_ADVI.R after setting the file paths. This will generate a file with the NUTS model information, a report of the max abs z-score, and a file with the NUTS and method mean and sds and NUTS Rhat and n\_eff size. For subsequent runs, you can source run\_test\_ADVI.R 

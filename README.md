Public website [https://qitools.ocpu.io/positivedeviance/www/](https://qitools.ocpu.io/positivedeviance/www/)


---------------------

Simple OpenCPU Application. 

If you want to run this on your computer, first install Rr:

    library(devtools)
    install_github("opencpu", "jeroenooms")
    install_github("positivedeviance", "qitools")

    library(opencpu)
    opencpu$browse("library/positivedeviance/www")

Use the same function using a local webserver on your computer:

    library(positivedeviance)
    positivedeviance()
    ?positivedeviance

For more information about OpenCPU apps, see [opencpu.js](https://github.com/jeroenooms/opencpu.js#readme)

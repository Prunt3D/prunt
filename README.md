Prunt currently requires a minimum GCC version of 15.3 from commit 1fdbcef462b5ffde1d03eda79c2db41829b84a46 or later. Using an older version may lead to silent data corruption and unpredictable behaviour due to compiler bugs.

GCC builds from Alire should be avoided for any boards using a serial port due to this bug: https://github.com/alire-project/GNAT-FSF-builds/issues/102

Prunt currently requires a custom GCC version with some backported and WIP patches. Using an upstream version may lead to silent data corruption and unpredictable behaviour due to compiler bugs.

Our GCC version may be found here: https://github.com/Prunt3D/gcc

GCC builds from Alire should be avoided for any boards using a serial port due to this bug: https://github.com/alire-project/GNAT-FSF-builds/issues/102

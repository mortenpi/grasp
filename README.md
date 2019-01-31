# General Relativistic Atomic Structure Package

[![Doxygen Documentation](https://img.shields.io/badge/documentation-master-blue.svg)](http://mortenpi.eu/grasp/)
[![Travis Status](https://travis-ci.com/mortenpi/grasp.svg?token=J2TJDmxGV6c9f8C3LXps&branch=master)](https://travis-ci.com/mortenpi/grasp)

## Quickstart

**Prerequisites.**
You need to have CMake, a Fortran compiler and the BLAS and LAPACK libraries installed. To compile the MPI programs you also need to have the MPI libraries installed. CMake should generally find the libraries automatically -- if not, please open an issue.

**Compilation.**
To start the build, you first need to set up the CMake build directory. You can use the `configure.sh` helper script to do that quickly by calling

```bash
./configure.sh
```

This will create a directory called `build/` which is where the compilation occurs. To compile GRASP, call

```bash
cd build/
make
```

You can also run the build in parallel on multiple cores with `make -jN`, where `N` is the number of cores you would like to use.

Finally, to place the GRASP binaries in the `bin/` directory, you can to install them with

```bash
make install
```

**Running GRASP.**
An easy way to run the different GRASP programs is to source the `envset.sh` script

```bash
source envset.sh
```

This will set up, in you current shell session:

1. The `$GRASP` environment variable that points to the root directory.
2. The `grasp` shell command with tab-completion for command names. You can then call a particular GRASP program with e.g. `grasp rmcdhf`.

**Debug builds.**
If you need binaries with debug symbols, you can easily set up a separate debug build.

```bash
./configure.sh --debug
```

You can also install the debug binaries to `$GRASP/bin`, but that is not recommended. Instead, you should call them with `$GRASP/build-debug/bin/<program name>`.

 
## Overview

This version of GRASP is a major revision of the previous GRASP2K package by
P. Jonsson, G. Gaigalas, J. Bieron, C. Froese Fischer, and I.P. Grant Computer
Physics Communication, 184, 2197 - 2203 (2013) written in FORTRAN 77 style with
COMMON and using Cray pointers for memory management.  The present version is a
FORTRAN95 translation using standard FORTRAN for memory management.  In
addition, COMMONS have been replaced with MODULES, with some COMMONS merged.
Some algorithms have been changed to improve performance for large cases and
efficiently.

The previous package, was an extension and modification of GRASP92 by Farid
Parpia, Charlotte Froese Fischer, and Ian Grant. Computer Physics Communication,
94, 249-271 (1996)

Development of this package was performed largely by:

* Charlotte Froese Fischer  email: cff@cs.ubc.ca
* Gediminas Gaigalas        email: Gediminas.Gaigalas@tfai.vu.lt
* Per Jönsson               email: per.jonsson@mau.se
* Jacek Bieron              email: jacek.bieron@uj.edu.pl

Please contact one of these authors if you have questions

Supporters include:

* Jörgen Ekman              email: jorgen.ekman@mah.se
* Ian Grant                 email: iangrant15@btinternet.com

Computer Physics Communications publication on this version of GRASP:

> C. Froese Fischer, G. Gaigalas, P. Jönsson, J. Bieroń,
> "GRASP2018 - a Fortran 95 version of the General Relativistic Atomic Structure Package",
> Computer Physics Communications, 2018,
> https://doi.org/10.1016/j.cpc.2018.10.032


## Structure of the Package

The package has the structure shown below where executables, after successful
compilation, reside in the `bin` directory. Compiled libraries are in the `lib`
directory. Scripts for example runs and case studies are in folders under
`grasptest`. Source code is in the `src` directory and divided into applications
in the `appl` directory, libraries in the `lib` directory and tools in the
`tool` directory.

```
   |-bin
   |-grasptest
   |---case1
   |-----script
   |---case1_mpi
   |-----script
   |-----tmp_mpi
   |---case2
   |-----script
   |---case2_mpi
   |-----script
   |-----tmp_mpi
   |---case3
   |-----script
   |---example1
   |-----script
   |---example2
   |-----script
   |---example3
   |-----script
   |---example4
   |-----script
   |-------tmp_mpi
   |---example5
   |-----script
   |-lib
   |-src
   |---appl
   |-----HF
   |-----jj2lsj90
   |-----jjgen90
   |-----rangular90
   |-----rangular90_mpi
   |-----rbiotransform90
   |-----rbiotransform90_mpi
   |-----rci90
   |-----rci90_mpi
   |-----rcsfgenerate90
   |-----rcsfinteract90
   |-----rcsfzerofirst90
   |-----rhfs90
   |-----rmcdhf90
   |-----rmcdhf90_mpi
   |-----rnucleus90
   |-----rtransition90
   |-----rtransition90_mpi
   |-----rwfnestimate90
   |-----sms90
   |---lib
   |-----lib9290
   |-----libdvd90
   |-----libmcp90
   |-----libmod
   |-----librang90
   |-----mpi90
   |---tool
```


## Program Guide and Compilation

The software is distributed with a practical guide to GRASP2018 in PDF-format.
The guide, which is under Creative Commons Attribution 4.0 International (CC BY
4.0) license, contains full information on how to compile and install the
package.


## Acknowledgements

This work was supported by the Chemical Sciences, Geosciences and Biosciences
Division, Office of Basic Energy Sciences, Office of Science, U.S. Department of
Energy who made the Pacific Sierra translator available and the National
Institute of Standards and Technology. Computer resources were made available by
Compute Canada.  CFF had research support from the Canadian NSERC Discovery
Grant 2017-03851.  JB acknowledges financial support of the European Regional
Development Fund in the framework of the Polish Innovation Economy Operational
Program (Contract No. POIG.02.01.00-12-023/08).


## License

The code in this repository is distributed under the [MIT license](LICENSE.md).
The associated manual "A practical guide to GRASP2018" is distributed under
[the Creative Commons Attribution 4.0 International (CC BY 4.0) license](https://creativecommons.org/licenses/by/4.0/legalcode).

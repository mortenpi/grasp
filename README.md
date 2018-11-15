# GRASP: General Relativistic Atomic Structure Package

This package is a major revision of the previous GRASP2K package by P. Jonsson,
G. Gaigalas, J. Bieron, C. Froese Fischer, and I.P. Grant Computer Physics
Communication, 184, 2197 - 2203 (2013) written in FORTRAN 77 style with COMMON
and using Cray pointers for memory management.  The present version is a
FORTRAN95 translation using standard FORTRAN for memory management.  In
addition, COMMONS have been replaced with MODULES, with some COMMONS merged.
Some algorithms have been changed to improve performance for large cases and
efficienty.

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

The package has the structure shown below where executables, after sucessful
compilation, reside in the bin directory. Compiled libraries are in the lib
directory. Scripts for example runs and case studies are in folders under
grasptest. Source code is in the src directory and divided into applications in
the appl directory, libraries in the lib directory and tools in the tool
directory.

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

The software is distributed with a practical  guide to GRASP2018 in PDF-format.
The guide, which is under Creative Commons Attribution 4.0 International — CC BY
4.0 - licence, contains full information on how to compile and install the
package.


## Acknowledgements

This work was supported by the Chemical Sciences, Geosciences and Biosciences
Division, Office of Basic Energy Sciences, Office of Science, U.S. Department of
Energy who made the Pacific Sierra translater available and the National
Institute of Standards and Technology. Computer resources were made available by
Compute Canada.  CFF had research support from the Canadian NSERC Discovery
Grant 2017-03851.  JB acknowledges financial support of the European Regional
Development Fund in the framework of the Polish Innovation Economy Operational
Program (Contract No. POIG.02.01.00-12-023/08).


## License

The code in this repository is distributed under the [MIT license](LICENSE.md).
The associated manual "A practical guide to GRASP2018" is distributed under
[the Creative Commons Attribution 4.0 International (CC BY 4.0) license](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

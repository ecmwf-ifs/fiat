FIAT
****

The Fortran IFS and Arpege Toolkit

Introduction
============

FIAT is a collection of selected Fortran utility libraries, extracted from the IFS/Arpege model.
 
 - drhook    : tracing
 - gstats    : timing
 - parkind   : choose precision
 - mpl       : MPI communication
 - mpi_serial: MPI dummy symbols compiled into static library
 - other various routines

License
=======

FIAT is distributed under the Apache License Version 2.0.
See `LICENSE` file for details.

Installing FIAT 
===============

Supported Platforms
-------------------

- Linux
- Apple MacOS

Other UNIX-like operating systems may work too out of the box.

Requirements
------------
- Fortran and C compiler, and optionally C++ compiler
- CMake (see https://cmake.org)
- ecbuild (see https://github.com/ecmwf/ecbuild)

Further optional dependencies:
- MPI Fortran libraries, preferably with MPI F08 support
- fckit compiled with eckit support (see https://github.com/ecmwf/fckit)

Building FIAT
-------------

Environment variables 

    $ export ecbuild_ROOT=<path-to-ecbuild>
    $ export MPI_HOME=<path-to-MPI>
    $ export fckit_ROOT=<path-to-fckit>
    $ export CC=<path-to-C-compiler>
    $ export FC=<path-to-Fortran-compiler>
    $ export CXX=<path-to-C++-compiler> 

You must compile FIAT out-of-source, so create a build-directory

    $ mkdir build && cd build
 
Configuration of the build happens through standard CMake

    $ cmake ..

Extra options can be added to the `cmake` command to control the build:

 - `-DCMAKE_BUILD_TYPE=<Debug|RelWithDebInfo|Release|Bit>` default=RelWithDebInfo (typically `-O2 -g`)
 - `-DENABLE_TESTS=<ON|OFF>` 
 - `-DENABLE_SINGLE_PRECISION=<ON|OFF>` default=ON
 - `-DENABLE_DOUBLE_PRECISION=<ON|OFF>` default=ON
 - `-DENABLE_MPI=<ON|OFF>`                               # if OFF, MPL links against dummy mpi_serial library
 - `-DENABLE_OMP=<ON|OFF>`
 - `-DENABLE_MPL_F77_DEPRECATED=<ON|OFF>` default=OFF    # build F77-based MPL instead of MPI_F08-based MPL
 - `-DENABLE_MPL_CHECK_CONTIG=<ON|OFF>` default=OFF      # enable run-time checks of contiguous status of arrays passed to MPL
 - `-DENABLE_DUMMY_MPI_HEADER=<ON|OFF>` default=ON
 - `-DCMAKE_INSTALL_PREFIX=<install-prefix>`

More options to control compilation flags, only when defaults are not sufficient

 - `-DOpenMP_Fortran_FLAGS=<flags>`
 - `-DCMAKE_Fortran_FLAGS=<fortran-flags>`
 - `-DCMAKE_C_FLAGS=<c-flags>`

Once this has finished successfully, run ``make`` and ``make install``.

Optionally, tests can be run to check succesful compilation, when the feature TESTS is enabled (`-DENABLE_TESTS=ON`, default ON)

    $ ctest


Contributing
============

Contributions to fiat are welcome. 
In order to do so, please open an issue where a feature request or bug can be discussed. 
Then create a pull request with your contribution and sign the [contributors license agreement (CLA)](https://bol-claassistant.ecmwf.int/ecmwf-ifs/fiat).


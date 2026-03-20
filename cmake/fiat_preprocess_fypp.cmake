# (C) Copyright 2026- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

macro(fiat_find_fypp)
    if (fckit_FOUND)
        ecbuild_info("fiat FOUND fypp via fckit: ${FYPP}}")
    endif()
    include(FetchContent)
    if (NOT FYPP)
        find_program( FYPP NAMES fypp PATHS ${FETCHCONTENT_BASE_DIR}/fypp/bin NO_DEFAULT_PATH )
        if (FYPP)
            ecbuild_info("fiat FOUND fypp in build-dir: ${FYPP}")
        endif()
    endif()
    if (NOT FYPP)
    find_program( FYPP NAMES fypp )
        if (FYPP)
            ecbuild_info("fiat FOUND fypp: ${FYPP}")
        endif()
    endif()
    if (NOT FYPP)
        set( fypp_SOURCE_DIR ${FETCHCONTENT_BASE_DIR}/fypp )
        set( FYPP ${fypp_SOURCE_DIR}/bin/fypp )
        ecbuild_info("fypp not found, downloading fypp in build-dir: ${FYPP}")
        cmake_policy_push()
        if (POLICY CMP0174)
            cmake_policy(SET CMP0174 OLD)
        endif()
        FetchContent_Declare(
            fypp
            GIT_REPOSITORY https://github.com/aradi/fypp
            GIT_TAG fc255be56ee2b0ad5833ecb567a477db2cc61167 # 2026-03-03
            SOURCE_DIR ${fypp_SOURCE_DIR}
            CONFIGURE_COMMAND ""  # Disables the configure step
            BUILD_COMMAND     ""  # Disables the build step
            INSTALL_COMMAND   ""  # Disables the install step
        )
        FetchContent_MakeAvailable(fypp)
        cmake_policy_pop()

        # PATCH: Adapt minimum required version of Python to 3.6, as 3.7 may not be default available, and 3.6 seems to work fine.
        file(READ ${FYPP} FILE_CONTENTS)
        string(REPLACE "MIN_PYTHON_VERSION = (3, 7)" "MIN_PYTHON_VERSION = (3, 6)" OUT_CONTENTS "${FILE_CONTENTS}")
        file(WRITE ${FYPP} "${OUT_CONTENTS}")
    endif()
    if (NOT FYPP)
        ecbuild_info("Could NOT find fypp")
    endif()
endmacro()

function( fiat_preprocess_fypp_sources output )
    set( options )
    set( single_value_args "" )
    set( multi_value_args SOURCES FYPP_ARGS FYPP_ARGS_EXCLUDE DEPENDS )
    cmake_parse_arguments(PARSE_ARGV 1 _PAR "${options}" "${single_value_args}" "${multi_value_args}")

    if (NOT FYPP)
        message(FATAL_ERROR "fypp not found, cannot preprocess fypp sources. Please install fypp or ensure it is on the PATH.")
    endif()

    unset( outfiles )
    foreach( filename ${_PAR_SOURCES} )
        get_filename_component( dir ${filename} DIRECTORY )
        get_filename_component( base ${filename} NAME_WE )
        if( dir )
            set( outfile "${CMAKE_CURRENT_BINARY_DIR}/${dir}/${base}.F90" )
            set( short_outfile "${dir}/${base}.F90" )
        else()
            set( outfile "${CMAKE_CURRENT_BINARY_DIR}/${base}.F90" )
            set( short_outfile "${base}.F90")
        endif()

        unset(args)
        list( APPEND args -l 132 ) # Line length
        list( APPEND args -p )     # Create parent folder
        if( _PAR_FYPP_ARGS )
            set( args ${args} ${_PAR_FYPP_ARGS} )
        endif()
        if (_PAR_FYPP_ARGS_EXCLUDE )
            list(REMOVE_ITEM args ${_PAR_FYPP_ARGS_EXCLUDE})
        endif()

        add_custom_command(
            OUTPUT ${outfile}
            COMMAND ${FYPP} ${args} ${CMAKE_CURRENT_SOURCE_DIR}/${filename} ${outfile}
            DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${filename} ${_PAR_DEPENDS}
            COMMENT "[fypp] Preprocessor generating ${short_outfile}" )
        set_source_files_properties(${outfile} PROPERTIES GENERATED TRUE)
        list( APPEND outfiles ${outfile} )
    endforeach()
    set(${output} ${${output}} ${outfiles} PARENT_SCOPE)
endfunction()

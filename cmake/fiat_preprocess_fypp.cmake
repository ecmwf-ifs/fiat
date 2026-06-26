# (C) Copyright 2026- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

#[=======================================================================[.rst:
fiat_preprocess_fypp_sources
----------------------------

Preprocess ``fypp`` input files into generated ``.F90`` sources.

.. command:: fiat_preprocess_fypp_sources(output
              SOURCES <files>...
              [FYPP_ARGS <args>...]
              [FYPP_ARGS_EXCLUDE <args>...]
              [DEPENDS <files>...])

    This function requires ``FYPP`` to be set to a usable ``fypp`` executable.
    It expands each file listed in ``SOURCES`` from
    ``${CMAKE_CURRENT_SOURCE_DIR}`` into a generated ``.F90`` file under the
    matching path in ``${CMAKE_CURRENT_BINARY_DIR}``.

    The generated command always passes ``-l 132`` and ``-p`` to ``fypp``.
    Additional options can be appended with ``FYPP_ARGS`` and selectively
    removed again with ``FYPP_ARGS_EXCLUDE``.

    For each input source, a custom command is created with dependencies on the
    original source file and any extra files listed in ``DEPENDS``. The
    generated files are marked with the ``GENERATED`` source property and their
    paths are appended to the variable named by ``output`` in the parent scope.
#]=======================================================================]

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

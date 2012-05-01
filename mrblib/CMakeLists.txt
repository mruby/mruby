# build mrblib
# need custom commands 
# Compile C source from merged mruby source

file(GLOB MRBLIB_SRC_RB "*.rb")

# generate the a single rubu file from all the existing ones.
add_custom_command(OUTPUT mrblib.rbtmp
COMMAND cat ${MRBLIB_SRC_RB} > mrblib.rbtmp
DEPENDS ${MRBLIB_SRC_RB}
)

# generate the intermediate representation in C
add_custom_command(OUTPUT mrblib_irep.c 
COMMAND echo -B mrblib_irep -o mrblib_irep.c mrblib.rbtmp
COMMAND mrbc -Bmrblib_irep -omrblib_irep.c mrblib.rbtmp
DEPENDS mrblib.rbtmp
)

# finally generate the library's c file
add_custom_command(OUTPUT mrblib.c 
COMMAND cat init_mrblib.c mrblib_irep.c > mrblib.c
DEPENDS init_mrblib.c mrblib_irep.c
)

# only use this C file to generate mrblib.
set(MRBLIB_SRC_C mrblib.c)
add_library(mrblib_static STATIC ${MRBLIB_SRC_C})
add_library(mrblib SHARED ${MRBLIB_SRC_C})
target_link_libraries(mrblib ${MRUBY_LIBS})
target_link_libraries(mrblib_static ${MRUBY_LIBS})
install(TARGETS mrblib mrblib_static
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib)


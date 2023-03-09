# See https://docs.python.org/3/extending/extending.html
cmake_minimum_required(VERSION 3.12)

project(spam VERSION "1.0")

find_package(Python COMPONENTS Interpreter Development)

message("Python_FOUND:${Python_FOUND}")
message("Python_VERSION:${Python_VERSION}")
message("Python_Development_FOUND:${Python_Development_FOUND}")
message("Python_LIBRARIES:${Python_LIBRARIES}")

python_add_library(spam MODULE
  spam.c
)

# vim : set et sts=2 sw=2 :
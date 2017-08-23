TEMPLATE = app
CONFIG += console c++11
CONFIG -= app_bundle
CONFIG -= qt

SOURCES += main.cpp
OTHER_FILES += data/data.csv

INCLUDEPATH += fast-cpp-csv-parser

OBJECTS_DIR = dist/obj

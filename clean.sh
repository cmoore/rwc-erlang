#!/usr/bin/env bash

rm -rf ebin/
rm -rf erl_crash.dump
rm -rf dorkinator.beam
rm -f *.log
rm -f *.access
find . -name "*.beam" -exec rm -f {} \;
rm -rf ebin/*
find . -name ".DS_Store" -exec rm -f {} \;
rm -f src/erlydtl/erlydtl_parser.erl


#!/bin/bash

erl -pa ./ebin -pa deps/*/ebin -s scribester_app -config app -s sync

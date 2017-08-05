#!/bin/sh

echo Building snav executable...
echo

ros dump --remove-docstrings --delete-debug-info executable snav.ros

echo
echo snav build complete

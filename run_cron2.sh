#!/bin/bash
set -xv
cd home/marcin/trigeR5/
Rscript R/run_all_wp.R
git add .
git commit -m "update bazy danych"
git push

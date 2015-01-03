# run.sh ---
# Filename: run.sh
# Author: Shi Yudi
# Created: 2015-01-03T10:44:31+0800
# Version:

# Commentary:
#
# 1. First run:
#
#   ./run.sh init
#
# 2. Generate website
#
#   ./run.sh
#
# 3. Test website
#
#   ./run.sh test
#
#

# Code:

# where html files put
PUBLISH_HTTP_DIR=~/sydi.org/html

function usage {
    echo "Usage: ./run.sh [init|test]"
}

case "$1" in
    init)
        echo init
        ;;
    test)
        (
            cd $PUBLISH_HTTP_DIR &&
            (sleep 1 && xdg-open http://localhost:8000 &) &&
            python2 -mSimpleHTTPServer 8000
        )
        ;;
    "")
        emacs --batch  -L htmlize -l sydi-site.el -f sydi/publish
        ;;
    *)
        usage;
        ;;
esac

#
# run.sh ends here

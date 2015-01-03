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
PUBLISH_HTTP_DIR=~/sydi.org/publish
USE_TIDY="TRUE"

REMOTE_USER=ryan;
REMOTE_HOST=sydi.org;
REMOTE_FOLDER=/srv/http/www/;
DESTNATE=${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_FOLDER};

function do_tidy {
    if [ "$USE_TIDY" ]; then
        find $PUBLISH_HTTP_DIR -type f \( -name '*.html' -or -name '*.xml' \) -exec tidy -m {} \;
    fi
}

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
    tidy)
        do_tidy
        ;;
    rsync)
        rsync -avz --progress --delete ${PUBLISH_HTTP_DIR}/ ${DESTNATE}
        ;;
    "")
        emacs -Q --batch -L htmlize -l sydi-site.el -l custom.el -f sydi-publish
        do_tidy
        ;;
    *)
        usage;
        ;;
esac

#
# run.sh ends here

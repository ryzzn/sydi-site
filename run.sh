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
BASE_DIR=~/sydi.org
ORIGIN_HTTP_DIR=$BASE_DIR/origin
PUBLISH_HTTP_DIR=$BASE_DIR/publish
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

function init {
    mkdir -p $ORIGIN_HTTP_DIR/{posts,dynamic,components,assets}

    echo '(setq org-publish-timestamp-directory "'$BASE_DIR'/.org-timestamps/")' >>custom.el
    echo '(setq sydi-base-directory "'$ORIGIN_HTTP_DIR'/")' >>custom.el
    echo '(setq sydi-publish-directory "'$PUBLISH_HTTP_DIR'/")' >>custom.el
    echo '(setq sydi-site-url "http://YOUR_SITE_URL")' >>custom.el
    echo '(setq sydi-site-name "YOUR_SITE_NAME")' >>custom.el

    echo "Write your javascript in file: $ORIGIN_HTTP_DIR/assets/javascripts/site.js"
    echo "Write your css in file: $ORIGIN_HTTP_DIR/assets/css/style.css"
    echo "Write your articles in under: $ORIGIN_HTTP_DIR/posts/"
}

case "$1" in
    init)
        init
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
